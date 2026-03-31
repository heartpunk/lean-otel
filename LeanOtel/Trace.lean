/-
  High-level tracing API.
  Provides `withSpan` for scoped instrumentation.
-/
import LeanOtel.Span
import LeanOtel.Export

namespace LeanOtel

/-- Generate a random hex string of given byte length (2 hex chars per byte). -/
private def randomHex (bytes : Nat) : IO String := do
  let mut s := ""
  for _ in List.range bytes do
    let b ← IO.rand 0 255
    let hi := b / 16
    let lo := b % 16
    let hexDigit (n : Nat) : Char :=
      if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
    s := s ++ String.mk [hexDigit hi, hexDigit lo]
  return s

/-- Generate a trace ID (16 bytes = 32 hex chars). -/
def newTraceId : IO String := randomHex 16

/-- Generate a span ID (8 bytes = 16 hex chars). -/
def newSpanId : IO String := randomHex 8

/-- Get current wall clock time in nanoseconds since Unix epoch.
    Uses `date +%s%N` for nanosecond precision. -/
def nowNanos : IO UInt64 := do
  let out ← IO.Process.output { cmd := "date", args := #["+%s%N"] }
  match out.stdout.trim.toNat? with
  | some n => return n.toUInt64
  | none =>
    IO.eprintln s!"lean-otel: failed to get wall clock time, falling back to monotonic"
    let ms ← IO.monoMsNow
    return ms.toUInt64 * 1000000

/-- Tracer context: holds current trace/span IDs and the exporter config. -/
structure TracerContext where
  config : ExporterConfig
  traceId : String
  parentSpanId : Option String := none
  pendingSpans : IO.Ref (Array Span)

/-- Create a new tracer context for a trace. -/
def TracerContext.new (config : ExporterConfig) : IO TracerContext := do
  let tid ← newTraceId
  let pending ← IO.mkRef #[]
  return { config, traceId := tid, pendingSpans := pending }

/-- Run an IO action within a named span. Records timing and attributes. -/
def TracerContext.withSpan (ctx : TracerContext) (name : String)
    (attrs : Array Attribute := #[])
    (f : TracerContext → IO α) : IO α := do
  let sid ← newSpanId
  let start ← nowNanos
  let childCtx := { ctx with parentSpanId := some sid }
  let result ← f childCtx
  let stop ← nowNanos
  let span : Span := {
    traceId := ctx.traceId
    spanId := sid
    parentSpanId := ctx.parentSpanId
    name := name
    startTimeUnixNano := start
    endTimeUnixNano := stop
    attributes := attrs
    status := .ok
  }
  ctx.pendingSpans.modify (·.push span)
  return result

/-- Flush all pending spans to the exporter. -/
def TracerContext.flush (ctx : TracerContext) : IO UInt32 := do
  let spans ← ctx.pendingSpans.get
  if spans.isEmpty then return 0
  let code ← exportSpans ctx.config spans
  ctx.pendingSpans.set #[]
  return code

/-- Flush pending spans to a file. -/
def TracerContext.flushToFile (ctx : TracerContext) (path : System.FilePath) : IO Unit := do
  let spans ← ctx.pendingSpans.get
  if spans.isEmpty then return
  exportSpansToFile path ctx.config.resource spans
  ctx.pendingSpans.set #[]

end LeanOtel
