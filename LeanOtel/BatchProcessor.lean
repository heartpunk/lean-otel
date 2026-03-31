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
deriving Repr

/-- Internal state of the batch processor. -/
structure BatchState where
  queue : Array Span
  dropped : Nat
deriving Repr

/-- Thread-safe batch processor handle. Uses IO.Ref for lock-free atomic ops. -/
structure BatchProcessor where
  config : BatchConfig
  state : IO.Ref BatchState
  flushSignal : IO.Ref Bool
  shutdown : IO.Ref Bool

/-- Create a new batch processor. Does NOT start the background task yet. -/
def BatchProcessor.new (config : BatchConfig) : IO BatchProcessor := do
  let state ← IO.mkRef { queue := #[], dropped := 0 : BatchState }
  let flushSignal ← IO.mkRef false
  let shutdown ← IO.mkRef false
  return { config, state, flushSignal, shutdown }

/-- Enqueue a span. Non-blocking. Drops the span if queue is full. -/
def BatchProcessor.enqueue (bp : BatchProcessor) (span : Span) : IO Bool := do
  let st ← bp.state.get
  if st.queue.size ≥ bp.config.maxQueueSize then
    bp.state.modify fun st => { st with dropped := st.dropped + 1 }
    return false
  else
    bp.state.modify fun st => { st with queue := st.queue.push span }
    return true

/-- Drain up to maxExportBatchSize spans from the queue. -/
def BatchProcessor.drain (bp : BatchProcessor) : IO (Array Span) := do
  let st ← bp.state.get
  let batchSize := min st.queue.size bp.config.maxExportBatchSize
  let batch := st.queue.extract 0 batchSize
  let remaining := st.queue.extract batchSize st.queue.size
  bp.state.set { st with queue := remaining }
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
  if result.error.isSome then
    IO.eprintln s!"lean-otel: shipBatch error: {result.error.get!}"
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

/-- Background export loop. Runs until shutdown is signaled. -/
def BatchProcessor.exportLoop (bp : BatchProcessor) : IO Unit := do
  while true do
    let done ← bp.shutdown.get
    if done then
      -- Final drain before exit
      bp.exportAll
      return

    let flush ← bp.flushSignal.get
    if flush then
      bp.flushSignal.set false
      bp.exportAll
    else
      let st ← bp.state.get
      if st.queue.size ≥ bp.config.maxExportBatchSize then
        bp.exportAll
      else
        -- Poll in short intervals, checking shutdown between sleeps
        let pollMs : UInt32 := min 100 bp.config.scheduledDelayMs
        let polls := bp.config.scheduledDelayMs / pollMs
        for _ in List.range polls.toNat do
          let done ← bp.shutdown.get
          if done then
            bp.exportAll
            return
          let flush ← bp.flushSignal.get
          if flush then
            bp.flushSignal.set false
            bp.exportAll
            break
          IO.sleep pollMs
        -- After delay, export if anything queued
        let st2 ← bp.state.get
        if st2.queue.size > 0 then
          bp.exportAll

/-- Start the background export task. Returns the task handle. -/
def BatchProcessor.start (bp : BatchProcessor) : IO (Task (Except IO.Error Unit)) :=
  IO.asTask (bp.exportLoop)

/-- Request an immediate flush of all pending spans. -/
def BatchProcessor.flush (bp : BatchProcessor) : IO Unit := do
  bp.flushSignal.set true
  -- Wait briefly for the background task to pick it up
  IO.sleep 100

/-- Shut down the processor: flush remaining spans, then stop. -/
def BatchProcessor.stop (bp : BatchProcessor) : IO Unit := do
  -- Request flush first
  bp.flushSignal.set true
  IO.sleep 200
  -- Then signal shutdown
  bp.shutdown.set true

/-- Get current stats (queue size, dropped count). -/
def BatchProcessor.stats (bp : BatchProcessor) : IO (Nat × Nat) := do
  let st ← bp.state.get
  return (st.queue.size, st.dropped)

end LeanOtel
