/-
  AsyncProcessor: async batch span export using CloseableChannel.

  Architecture (follows Go OTel SDK pattern):
  - Producer (pipeline) sends spans to a bounded CloseableChannel
  - Worker task reads from channel, accumulates a batch, exports when:
    1. Batch reaches maxExportBatchSize
    2. Timer fires (scheduledDelayMs since last export)
    3. Channel closes (shutdown — drain remaining and exit)
  - Shutdown: close the channel, worker drains and exits, IO.wait on worker task

  Pure core: batch accumulation logic is in BatchAccum (testable without IO).
  IO shell: channel + worker task + libcurl export.
-/
import LeanOtel.Span
import LeanOtel.Json
import LeanOtel.Export
import Std.Sync.Channel

namespace LeanOtel

open Std

/-! ## Pure core: batch accumulation -/

/-- Accumulator state for the worker. -/
structure BatchAccum where
  batch : Array Span
  maxBatchSize : Nat
  totalExported : Nat
  totalDropped : Nat
deriving Repr

/-- Create an empty accumulator. -/
def BatchAccum.empty (maxBatchSize : Nat) : BatchAccum :=
  { batch := #[], maxBatchSize, totalExported := 0, totalDropped := 0 }

/-- Add a span to the batch. Returns (accum, shouldExport). -/
def BatchAccum.add (acc : BatchAccum) (span : Span) : BatchAccum × Bool :=
  let acc' := { acc with batch := acc.batch.push span }
  (acc', acc'.batch.size ≥ acc.maxBatchSize)

/-- Take the current batch for export, reset accumulator. -/
def BatchAccum.take (acc : BatchAccum) : Array Span × BatchAccum :=
  (acc.batch, { acc with batch := #[], totalExported := acc.totalExported + acc.batch.size })

/-- Record dropped spans. -/
def BatchAccum.recordDropped (acc : BatchAccum) (n : Nat) : BatchAccum :=
  { acc with totalDropped := acc.totalDropped + n }

/-! ## IO shell: async processor -/

/-- Configuration for the async processor. -/
structure AsyncConfig where
  maxQueueSize : Nat := 2048
  maxExportBatchSize : Nat := 512
  scheduledDelayMs : UInt32 := 5000
  endpoint : String := "https://api.honeycomb.io"
  apiKey : String
  resource : Resource
deriving Repr

/-- Async processor handle. -/
structure AsyncProcessor where
  channel : CloseableChannel Span
  config : AsyncConfig
  workerTask : Task (Except IO.Error Unit)
  stats : IO.Ref BatchAccum

/-- Export a batch via libcurl. -/
private def doExport (config : AsyncConfig) (spans : Array Span) : IO Bool := do
  if spans.isEmpty then return true
  let exportConfig : ExporterConfig := {
    endpoint := config.endpoint
    apiKey := config.apiKey
    resource := config.resource
  }
  let result ← exportSpans exportConfig spans
  match result.error with
  | some msg =>
    IO.eprintln s!"lean-otel: async export error: {msg}"
    return false
  | none => return result.statusCode == 200

/-- Worker loop: read from channel, batch, export. -/
private def workerLoop (ch : CloseableChannel.Sync Span) (config : AsyncConfig)
    (statsRef : IO.Ref BatchAccum) : IO Unit := do
  let mut acc := BatchAccum.empty config.maxExportBatchSize
  let mut lastExportMs ← IO.monoMsNow

  while true do
    -- Try non-blocking recv first
    let item ← ch.recv

    match item with
    | none =>
      -- Channel closed: export remaining batch, then exit
      if !acc.batch.isEmpty then
        let (batch, acc') := acc.take
        let _ ← doExport config batch
        acc := acc'
      statsRef.set acc
      return

    | some span =>
      let (acc', shouldExport) := acc.add span
      acc := acc'

      if shouldExport then
        -- Batch full: export now
        let (batch, acc') := acc.take
        let ok ← doExport config batch
        if ok then
          IO.eprintln s!"lean-otel: exported {batch.size} spans"
        acc := acc'
        lastExportMs ← IO.monoMsNow
      else
        -- Check timer
        let now ← IO.monoMsNow
        if now - lastExportMs ≥ config.scheduledDelayMs.toNat && !acc.batch.isEmpty then
          let (batch, acc') := acc.take
          let ok ← doExport config batch
          if ok then
            IO.eprintln s!"lean-otel: timer export {batch.size} spans"
          acc := acc'
          lastExportMs ← IO.monoMsNow

    statsRef.set acc

/-- Create and start an async processor. -/
def AsyncProcessor.new (config : AsyncConfig) : IO AsyncProcessor := do
  let ch ← CloseableChannel.new (some config.maxQueueSize)
  let statsRef ← IO.mkRef (BatchAccum.empty config.maxExportBatchSize)
  let syncCh := ch.sync
  let task ← IO.asTask (workerLoop syncCh config statsRef) (prio := Task.Priority.dedicated)
  return { channel := ch, config, workerTask := task, stats := statsRef }

/-- Send a span to the processor. Non-blocking. Returns false if channel is closed. -/
def AsyncProcessor.send (ap : AsyncProcessor) (span : Span) : IO Bool := do
  let closed ← ap.channel.isClosed
  if closed then return false
  let sendTask ← ap.channel.send span
  match ← IO.wait sendTask with
  | .ok () => return true
  | .error _ => return false

/-- Shut down: close channel, wait for worker to drain and exit. -/
def AsyncProcessor.shutdown (ap : AsyncProcessor) : IO Unit := do
  -- Close channel — worker will see `none` from recv and drain
  try ap.channel.close catch | _ => pure ()
  -- Wait for worker task to complete
  let _ ← IO.wait ap.workerTask
  return

/-- Get export stats. -/
def AsyncProcessor.getStats (ap : AsyncProcessor) : IO BatchAccum :=
  ap.stats.get

end LeanOtel
