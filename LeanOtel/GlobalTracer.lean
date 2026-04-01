/-
  Global tracer singleton + `traced def` syntax.

  Usage:
    -- At startup:
    LeanOtel.initGlobalTracer config

    -- Define a traced function:
    traced def myFunction (x : Nat) : IO Nat := do
      ...

  `traced def` wraps the function body in a span named after the function.
  If the global tracer is not initialized, the function runs without tracing (no-op).
-/
import LeanOtel.Trace
import LeanOtel.AsyncProcessor
import Lean.Elab.Command

namespace LeanOtel

/-- Global tracer reference, initialized to none. -/
initialize globalTracerRef : IO.Ref (Option Tracer) ← IO.mkRef none

/-- Global async processor for background export. -/
initialize globalAsyncRef : IO.Ref (Option AsyncProcessor) ← IO.mkRef none

/-- Current span stack for parent-child nesting in withGlobalSpan. -/
initialize globalSpanStackRef : IO.Ref (Array String) ← IO.mkRef #[]

/-- Parse W3C TRACEPARENT: "00-<traceId>-<parentSpanId>-<flags>" -/
def parseTraceparent (s : String) : Option (String × String) :=
  let parts := s.splitOn "-"
  if parts.length == 4 then
    let traceId := parts[1]!
    let parentSpanId := parts[2]!
    if traceId.length == 32 && parentSpanId.length == 16 then
      some (traceId, parentSpanId)
    else none
  else none

/-- Set up the global tracer with async background export.
    Reads TRACEPARENT from env for parent context.
    Must be called before any traced function runs. -/
def initGlobalTracer (config : BatchConfig) : IO Unit := do
  IO.eprintln s!"lean-otel: initGlobalTracer called, apiKey length={config.apiKey.length}"
  let mut tracer ← Tracer.new config
  -- Check for W3C TRACEPARENT propagation
  match ← IO.getEnv "TRACEPARENT" with
  | some tp =>
    match parseTraceparent tp with
    | some (traceId, parentSpanId) =>
      tracer := { tracer with traceId, parentSpanId := some parentSpanId }
    | none => IO.eprintln s!"lean-otel: invalid TRACEPARENT format: {tp}"
  | none => pure ()
  globalTracerRef.set (some tracer)
  -- Start async processor for background export
  let asyncConfig : AsyncConfig := {
    apiKey := config.apiKey
    endpoint := config.endpoint
    resource := config.resource
    maxQueueSize := config.maxQueueSize
    maxExportBatchSize := config.maxExportBatchSize
    scheduledDelayMs := config.scheduledDelayMs
  }
  let ap ← AsyncProcessor.new asyncConfig
  globalAsyncRef.set (some ap)

/-- Get the global tracer. Returns none if not initialized. -/
def getGlobalTracer : IO (Option Tracer) :=
  globalTracerRef.get

/-- Run an IO action with a span on the global tracer. Spans are exported async.
    Supports nesting: inner spans get the enclosing span as their parent.
    No-op if tracer not initialized. -/
def withGlobalSpan (name : String) (attrs : Array Attribute := #[]) (f : IO α) : IO α := do
  let opt ← getGlobalTracer
  IO.eprintln s!"lean-otel: withGlobalSpan '{name}' tracer={opt.isSome}"
  match opt with
  | some t =>
    let sid ← newSpanId
    -- Parent is top of span stack (enclosing span), or tracer's initial parent
    let stack ← globalSpanStackRef.get
    let parentId := if stack.isEmpty then t.parentSpanId else some stack.back!
    -- Push this span onto the stack
    globalSpanStackRef.set (stack.push sid)
    let start ← t.timeSource
    let result ← f
    let stop ← t.timeSource
    -- Pop this span from the stack
    let stack' ← globalSpanStackRef.get
    globalSpanStackRef.set stack'.pop
    let span : Span := {
      traceId := t.traceId
      spanId := sid
      parentSpanId := parentId
      name := name
      startTimeUnixNano := start
      endTimeUnixNano := stop
      attributes := attrs
      status := .ok
    }
    -- Send to async processor for background export
    match ← globalAsyncRef.get with
    | some ap => let _ ← ap.send span
    | none => let _ ← t.processor.enqueue span  -- fallback to sync
    return result
  | none => f

/-- Emit a span immediately and push it as the current parent context.
    Use this for long-running operations where the "root" span would never
    complete before child spans are exported. Call `popGlobalSpan` when done. -/
def pushGlobalSpan (name : String) (attrs : Array Attribute := #[]) : IO Unit := do
  match ← getGlobalTracer with
  | some t =>
    let sid ← newSpanId
    let stack ← globalSpanStackRef.get
    let parentId := if stack.isEmpty then t.parentSpanId else some stack.back!
    globalSpanStackRef.set (stack.push sid)
    let now ← t.timeSource
    let span : Span := {
      traceId := t.traceId
      spanId := sid
      parentSpanId := parentId
      name := name
      startTimeUnixNano := now
      endTimeUnixNano := now  -- zero-duration marker span
      attributes := attrs
      status := .ok
    }
    match ← globalAsyncRef.get with
    | some ap => let _ ← ap.send span
    | none => let _ ← t.processor.enqueue span
  | none => pure ()

/-- Pop the current span context pushed by `pushGlobalSpan`. -/
def popGlobalSpan : IO Unit := do
  let stack ← globalSpanStackRef.get
  unless stack.isEmpty do
    globalSpanStackRef.set stack.pop

/-- Flush the global tracer. -/
def flushGlobalTracer : IO Unit := do
  match ← globalAsyncRef.get with
  | some ap =>
    -- Async processor flushes via shutdown/restart — just wait for background drain
    IO.sleep 200
  | none =>
    match ← getGlobalTracer with
    | some t => t.flush
    | none => pure ()

/-- Stop the global tracer and async processor. -/
def stopGlobalTracer : IO Unit := do
  match ← globalAsyncRef.get with
  | some ap => ap.shutdown
  | none =>
    match ← getGlobalTracer with
    | some t => t.stop
    | none => pure ()

open Lean Syntax in
/-- Extract explicit parameter names from an optDeclSig's binders. -/
private def getExplicitParams (sig : TSyntax ``Parser.Command.optDeclSig) : Array Name :=
  let args := sig.raw[0].getArgs
  args.foldl (init := #[]) fun acc arg =>
    if arg.getKind == ``Parser.Term.explicitBinder then
      arg[1].getArgs.foldl (init := acc) fun acc2 id =>
        if id.isIdent then acc2.push id.getId else acc2
    else if arg.isIdent then
      acc.push arg.getId
    else acc

open Lean in
private def filterParams (paramNames : Array Name) (filter : Option (Array Name)) : Array Name :=
  match filter with
  | none => paramNames
  | some accept => paramNames.filter (accept.contains ·)

open Lean in
private def mkAttrElems (paramNames : Array Name) : MacroM (Array (TSyntax `term)) :=
  paramNames.mapM fun pn => do
    let pnStr := Lean.quote (toString pn)
    let pnIdent := mkIdent pn
    `(⟨$pnStr, .str (toString $pnIdent)⟩)

open Lean in
/-- `traced def f (x : Nat) (y : String) : IO T := body`
    Captures all explicit params as span attributes.

    `traced +[x] def f (x y : Nat) : IO T := body`
    Only captures `x`. -/
scoped macro "traced " "def " name:ident sig:optDeclSig " := " body:term : command => do
  let spanName := Lean.quote (toString name.getId)
  let paramNames := getExplicitParams sig
  let attrElems ← mkAttrElems paramNames
  let attrArray ← `(#[$[$attrElems],*])
  `(def $name $sig := withGlobalSpan $spanName (attrs := $attrArray) ($body))

scoped macro "traced " "+[" accepts:ident,* "] " "def " name:ident sig:optDeclSig " := " body:term : command => do
  let spanName := Lean.quote (toString name.getId)
  let acceptNames := accepts.getElems.map (·.getId)
  let paramNames := filterParams (getExplicitParams sig) (some acceptNames)
  let attrElems ← mkAttrElems paramNames
  let attrArray ← `(#[$[$attrElems],*])
  `(def $name $sig := withGlobalSpan $spanName (attrs := $attrArray) ($body))

scoped macro "traced " "-[" rejects:ident,* "] " "def " name:ident sig:optDeclSig " := " body:term : command => do
  let spanName := Lean.quote (toString name.getId)
  let rejectNames := rejects.getElems.map (·.getId)
  let paramNames := (getExplicitParams sig).filter (!rejectNames.contains ·)
  let attrElems ← mkAttrElems paramNames
  let attrArray ← `(#[$[$attrElems],*])
  `(def $name $sig := withGlobalSpan $spanName (attrs := $attrArray) ($body))

end LeanOtel
