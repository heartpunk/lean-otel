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
import Lean.Elab.Command

namespace LeanOtel

/-- Global tracer reference, initialized to none. -/
initialize globalTracerRef : IO.Ref (Option Tracer) ← IO.mkRef none

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

/-- Set up the global tracer. Reads TRACEPARENT from env for parent context.
    Must be called before any traced function runs. -/
def initGlobalTracer (config : BatchConfig) : IO Unit := do
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

/-- Get the global tracer. Returns none if not initialized. -/
def getGlobalTracer : IO (Option Tracer) :=
  globalTracerRef.get

/-- Run an IO action with a span on the global tracer. No-op if tracer not initialized. -/
def withGlobalSpan (name : String) (attrs : Array Attribute := #[]) (f : IO α) : IO α := do
  match ← getGlobalTracer with
  | some t =>
    t.withSpan name (attrs := attrs) fun _ => f
  | none => f

/-- Flush the global tracer. -/
def flushGlobalTracer : IO Unit := do
  match ← getGlobalTracer with
  | some t => t.flush
  | none => pure ()

/-- Stop the global tracer. -/
def stopGlobalTracer : IO Unit := do
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
