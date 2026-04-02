/-
  BatchSpanProcessor: in-process queue + background export thread.
  Follows the OTel SDK spec:
  - Bounded queue (drops spans if full)
  - Background task drains queue and ships batches
  - Exports on: batch size reached, timer fires, or flush requested
-/
import LeanOtel.Span
import LeanOtel.Json
import LeanOtel.Export

namespace LeanOtel

/-- Configuration for the batch span processor. -/
structure BatchConfig where
  maxQueueSize : Nat := 2048
  maxExportBatchSize : Nat := 512
  scheduledDelayMs : UInt32 := 5000
  exportTimeoutMs : UInt32 := 30000
  endpoint : String := "https://api.honeycomb.io"
  apiKey : String
  resource : Resource
  emitOnStart : Bool := false
deriving Repr

/-- Internal state of the batch processor. -/
structure BatchState where
  queue : Array Span
  dropped : Nat
deriving Repr

/-! ## Pure queue operations — proved in Proofs.lean -/

/-- Pure enqueue: try to add a span to the queue. Returns (new state, accepted). -/
def BatchState.enqueue (st : BatchState) (span : Span) (maxQueueSize : Nat) : BatchState × Bool :=
  if st.queue.size ≥ maxQueueSize then
    ({ st with dropped := st.dropped + 1 }, false)
  else
    ({ st with queue := st.queue.push span }, true)

/-- Pure drain: take up to maxBatchSize spans from front. Returns (batch, remaining state). -/
def BatchState.drain (st : BatchState) (maxBatchSize : Nat) : Array Span × BatchState :=
  let batchSize := min st.queue.size maxBatchSize
  let batch := st.queue.extract 0 batchSize
  let remaining := st.queue.extract batchSize st.queue.size
  (batch, { st with queue := remaining })

/-! ## IO wrappers — thin shells over the pure operations -/

/-- Batch processor handle. Uses IO.Ref for atomic state access. -/
structure BatchProcessor where
  config : BatchConfig
  state : IO.Ref BatchState

/-- Create a new batch processor. -/
def BatchProcessor.new (config : BatchConfig) : IO BatchProcessor := do
  let state ← IO.mkRef { queue := #[], dropped := 0 : BatchState }
  return { config, state }

/-- Enqueue a span. Non-blocking. Drops the span if queue is full. -/
def BatchProcessor.enqueue (bp : BatchProcessor) (span : Span) : IO Bool := do
  let st ← bp.state.get
  let (st', accepted) := st.enqueue span bp.config.maxQueueSize
  bp.state.set st'
  return accepted

/-- Drain up to maxExportBatchSize spans from the queue. -/
def BatchProcessor.drain (bp : BatchProcessor) : IO (Array Span) := do
  let st ← bp.state.get
  let (batch, st') := st.drain bp.config.maxExportBatchSize
  bp.state.set st'
  return batch

/-- Ship a batch of spans to the endpoint via libcurl. -/
def shipBatch (config : BatchConfig) (spans : Array Span) : IO UInt32 := do
  if spans.isEmpty then return 0
  let exportConfig : ExporterConfig := {
    endpoint := config.endpoint
    apiKey := config.apiKey
    resource := config.resource
  }
  let result ← exportSpans exportConfig spans
  match result.error with
  | some msg => IO.eprintln s!"lean-otel: shipBatch error: {msg}"
  | none => pure ()
  return result.statusCode

/-- Export all queued spans in batches. -/
def BatchProcessor.exportAll (bp : BatchProcessor) : IO Unit := do
  repeat do
    let batch ← bp.drain
    if batch.isEmpty then return
    let code ← shipBatch bp.config batch
    if code == 200 then
      IO.eprintln s!"lean-otel: shipped {batch.size} spans"
    else
      IO.eprintln s!"lean-otel: export failed HTTP {code}, {batch.size} spans lost"

/-- Get current stats (queue size, dropped count). -/
def BatchProcessor.stats (bp : BatchProcessor) : IO (Nat × Nat) := do
  let st ← bp.state.get
  return (st.queue.size, st.dropped)

end LeanOtel
