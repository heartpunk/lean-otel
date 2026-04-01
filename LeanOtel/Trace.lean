/-
  High-level tracing API.
  Provides `withSpan` for scoped instrumentation.
  Uses BatchProcessor for non-blocking async export.
-/
import LeanOtel.Span
import LeanOtel.BatchProcessor

namespace LeanOtel

/-- The hex digit function. -/
def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

/-- Generate a random hex string of given byte length (2 hex chars per byte). -/
def randomHex (bytes : Nat) : IO String := do
  let mut s := ""
  for _ in List.range bytes do
    let b ← IO.rand 0 255
    let hi := b / 16
    let lo := b % 16
    s := s ++ String.ofList [hexDigit hi, hexDigit lo]
  return s

/-- Generate a trace ID (16 bytes = 32 hex chars). -/
def newTraceId : IO String := randomHex 16

/-- Generate a span ID (8 bytes = 16 hex chars). -/
def newSpanId : IO String := randomHex 8

/-- Time source: returns nanoseconds since epoch. Injectable for testing. -/
def TimeSource := IO UInt64

/-- Default time source: wall clock via `date +%s%N`, monotonic fallback. -/
def defaultTimeSource : TimeSource := do
  let out ← IO.Process.output { cmd := "date", args := #["+%s%N"] }
  match out.stdout.trimAscii.toString.toNat? with
  | some n => return n.toUInt64
  | none =>
    IO.eprintln s!"lean-otel: failed to get wall clock time, falling back to monotonic"
    let ms ← IO.monoMsNow
    return ms.toUInt64 * 1000000

/-- Get current time using the default time source. -/
def nowNanos : IO UInt64 := defaultTimeSource

/-- Tracer: holds batch processor and current trace context. -/
structure Tracer where
  processor : BatchProcessor
  traceId : String
  parentSpanId : Option String := none
  timeSource : TimeSource := defaultTimeSource

/-- Create a new tracer. Spans are queued and shipped on flush/stop. -/
def Tracer.new (config : BatchConfig) : IO Tracer := do
  let bp ← BatchProcessor.new config
  let tid ← newTraceId
  return { processor := bp, traceId := tid }

/-- Run an IO action within a named span. Non-blocking — span is enqueued for async export. -/
def Tracer.withSpan (t : Tracer) (name : String)
    (attrs : Array Attribute := #[])
    (f : Tracer → IO α) : IO α := do
  let sid ← newSpanId
  let start ← t.timeSource
  let childTracer := { t with parentSpanId := some sid }
  let result ← f childTracer
  let stop ← t.timeSource
  let span : Span := {
    traceId := t.traceId
    spanId := sid
    parentSpanId := t.parentSpanId
    name := name
    startTimeUnixNano := start
    endTimeUnixNano := stop
    attributes := attrs
    status := .ok
  }
  let accepted ← t.processor.enqueue span
  unless accepted do
    IO.eprintln s!"lean-otel: span '{name}' dropped (queue full)"
  return result

/-- Flush all pending spans synchronously. -/
def Tracer.flush (t : Tracer) : IO Unit :=
  t.processor.exportAll

/-- Shut down the tracer: flush remaining spans. -/
def Tracer.stop (t : Tracer) : IO Unit :=
  t.processor.exportAll

/-- Get queue stats. -/
def Tracer.stats (t : Tracer) : IO (Nat × Nat) :=
  t.processor.stats

-- Keep old TracerContext API for backward compat during transition
-- TODO: remove after migration

end LeanOtel
