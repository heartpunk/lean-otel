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

/-- Set up the global tracer. Must be called before any traced function runs. -/
def initGlobalTracer (config : BatchConfig) : IO Unit := do
  let tracer ← Tracer.new config
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

open Lean in
/-- `traced def f (args) : IO T := body` desugars to
    `def f (args) : IO T := LeanOtel.withGlobalSpan "f" body` -/
scoped macro "traced " "def " name:ident sig:optDeclSig " := " body:term : command =>
  let spanName := Lean.quote (toString name.getId)
  `(def $name $sig := withGlobalSpan $spanName (attrs := #[]) ($body))

end LeanOtel
