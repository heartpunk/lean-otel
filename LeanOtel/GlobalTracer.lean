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

/-- Global async processors for background export (one per exporter endpoint). -/
initialize globalAsyncRef : IO.Ref (Array AsyncProcessor) ← IO.mkRef #[]

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

/-- Set up the global tracer with one or more async exporters.
    Reads TRACEPARENT from env for parent context.
    Each config gets its own independent AsyncProcessor.
    Must be called before any traced function runs. -/
def initGlobalTracer (configs : Array BatchConfig) : IO Unit := do
  match configs[0]? with
  | none =>
    IO.eprintln "lean-otel: initGlobalTracer called with no configs"
    return
  | some firstConfig =>
  IO.eprintln s!"lean-otel: initGlobalTracer called, {configs.size} exporter(s)"
  let mut tracer ← Tracer.new firstConfig
  -- Check for W3C TRACEPARENT propagation
  match ← IO.getEnv "TRACEPARENT" with
  | some tp =>
    match parseTraceparent tp with
    | some (traceId, parentSpanId) =>
      tracer := { tracer with traceId, parentSpanId := some parentSpanId }
    | none => IO.eprintln s!"lean-otel: invalid TRACEPARENT format: {tp}"
  | none => pure ()
  globalTracerRef.set (some tracer)
  -- Start one async processor per config
  let mut processors : Array AsyncProcessor := #[]
  for config in configs do
    let asyncConfig : AsyncConfig := {
      apiKey := config.apiKey
      endpoint := config.endpoint
      resource := config.resource
      maxQueueSize := config.maxQueueSize
      maxExportBatchSize := config.maxExportBatchSize
      scheduledDelayMs := config.scheduledDelayMs
      emitOnStart := config.emitOnStart
    }
    let ap ← AsyncProcessor.new asyncConfig
    IO.eprintln s!"lean-otel: exporter started → {config.endpoint}"
    processors := processors.push ap
  globalAsyncRef.set processors

/-- Convenience: single-exporter init. -/
def initGlobalTracerSingle (config : BatchConfig) : IO Unit :=
  initGlobalTracer #[config]

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
    -- Emit partial span (endTime=0) to emitOnStart processors
    let processors ← globalAsyncRef.get
    unless processors.isEmpty do
      let startSpan : Span := {
        traceId := t.traceId
        spanId := sid
        parentSpanId := parentId
        name := name
        startTimeUnixNano := start
        endTimeUnixNano := 0
        attributes := attrs
        status := .unset
      }
      for ap in processors do
        if ap.config.emitOnStart then
          let _ ← ap.send startSpan
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
    -- Send to all async processors (best-effort per exporter)
    let processors ← globalAsyncRef.get
    if processors.isEmpty then
      let _ ← t.processor.enqueue span  -- fallback to sync
    else
      for ap in processors do
        let _ ← ap.send span
    return result
  | none => f

/-- Flush the global tracer. -/
def flushGlobalTracer : IO Unit := do
  let processors ← globalAsyncRef.get
  if processors.isEmpty then
    match ← getGlobalTracer with
    | some t => t.flush
    | none => pure ()
  else
    -- Async processors flush via shutdown/restart — just wait for background drain
    IO.sleep 200

/-- Stop the global tracer and all async processors. -/
def stopGlobalTracer : IO Unit := do
  let processors ← globalAsyncRef.get
  if processors.isEmpty then
    match ← getGlobalTracer with
    | some t => t.stop
    | none => pure ()
  else
    for ap in processors do
      ap.shutdown

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
