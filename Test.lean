/-
  Tests for lean-otel.
  Compile-time checks (#guard) for pure logic.
  IO tests for queue operations and integration.
-/
import LeanOtel

open LeanOtel

/-! ## Compile-time checks (pure logic) -/

-- hexDigit produces expected characters
#guard hexDigit 0 == '0'
#guard hexDigit 9 == '9'
#guard hexDigit 10 == 'a'
#guard hexDigit 15 == 'f'

-- statusCodeToInt maps correctly
#guard statusCodeToInt .unset == 0
#guard statusCodeToInt .ok == 1
#guard statusCodeToInt .error == 2

-- BatchState.enqueue: accept when under limit
#guard
  let st : BatchState := { queue := #[], dropped := 0 }
  let span : Span := ⟨"t", "s", none, "test", 0, 1, #[], .unset⟩
  (st.enqueue span 10).2 == true

-- BatchState.enqueue: reject when at limit
#guard
  let st : BatchState := { queue := #[], dropped := 0 }
  let span : Span := ⟨"t", "s", none, "test", 0, 1, #[], .unset⟩
  (st.enqueue span 0).2 == false

-- BatchState.enqueue: dropped count increments on reject
#guard
  let st : BatchState := { queue := #[], dropped := 5 }
  let span : Span := ⟨"t", "s", none, "test", 0, 1, #[], .unset⟩
  (st.enqueue span 0).1.dropped == 6

-- BatchState.drain: empty queue returns empty batch
#guard
  let st : BatchState := { queue := #[], dropped := 0 }
  (st.drain 10).1.size == 0

-- BatchState.drain: respects max batch size
#guard
  let span : Span := ⟨"t", "s", none, "test", 0, 1, #[], .unset⟩
  let st : BatchState := { queue := #[span, span, span, span, span], dropped := 0 }
  (st.drain 2).1.size == 2

-- BatchState.drain: preserves total
#guard
  let span : Span := ⟨"t", "s", none, "test", 0, 1, #[], .unset⟩
  let st : BatchState := { queue := #[span, span, span, span, span], dropped := 0 }
  let (batch, st') := st.drain 2
  batch.size + st'.queue.size == 5

-- JSON serialization produces objects (compile-time via decidable instance)
#guard
  let span : Span := ⟨"abc", "def", none, "test", 100, 200, #[], .unset⟩
  Json.isObj span.toOtlpJson

#guard
  let r : Resource := { serviceName := "test-svc" }
  Json.isObj r.toOtlpJson

#guard
  let r : Resource := { serviceName := "test-svc" }
  Json.isObj (mkTraceExportRequest r #[])

-- JSON key presence verified by Honeycomb 200 in integration test

/-! ## BatchAccum pure logic -/

section BatchAccumTests
open LeanOtel

#guard
  let acc := BatchAccum.empty 3
  let span : Span := ⟨"t", "s", none, "n", 0, 1, #[], .unset⟩
  let (acc1, exp1) := BatchAccum.add acc span
  let (acc2, exp2) := BatchAccum.add acc1 span
  let (_, exp3) := BatchAccum.add acc2 span
  !exp1 && !exp2 && exp3

#guard
  let acc := BatchAccum.empty 10
  let span : Span := ⟨"t", "s", none, "n", 0, 1, #[], .unset⟩
  let (acc1, _) := BatchAccum.add acc span
  let (acc2, _) := BatchAccum.add acc1 span
  let (batch, acc3) := BatchAccum.take acc2
  batch.size == 2 && acc3.batch.size == 0 && acc3.totalExported == 2

#guard
  let acc := BatchAccum.empty 10
  let acc' := BatchAccum.recordDropped acc 5
  acc'.totalDropped == 5

end BatchAccumTests

/-! ## IO tests -/

def testSpan : Span := {
  traceId := "0af7651916cd43dd8448eb211c80319c"
  spanId := "b7ad6b7169203331"
  name := "test-span"
  startTimeUnixNano := 1000000000
  endTimeUnixNano := 2000000000
}

def assert (name : String) (cond : Bool) : IO Unit := do
  if cond then
    IO.println s!"  PASS: {name}"
  else
    IO.eprintln s!"  FAIL: {name}"
    throw (IO.Error.userError s!"Test failed: {name}")

def testQueueOps : IO Unit := do
  IO.println "Queue operations:"
  let bp ← BatchProcessor.new {
    apiKey := "test", maxQueueSize := 3, maxExportBatchSize := 2,
    resource := { serviceName := "test" }
  }

  -- Enqueue 3 spans (fills queue)
  let ok1 ← bp.enqueue testSpan
  let ok2 ← bp.enqueue testSpan
  let ok3 ← bp.enqueue testSpan
  assert "enqueue 1 accepted" ok1
  assert "enqueue 2 accepted" ok2
  assert "enqueue 3 accepted" ok3

  -- 4th should be rejected
  let ok4 ← bp.enqueue testSpan
  assert "enqueue 4 rejected (queue full)" (!ok4)

  -- Stats
  let (queued, dropped) ← bp.stats
  assert "queued == 3" (queued == 3)
  assert "dropped == 1" (dropped == 1)

  -- Drain respects batch size
  let batch ← bp.drain
  assert "drain returns maxBatchSize=2" (batch.size == 2)

  let (queued2, _) ← bp.stats
  assert "1 remaining after drain" (queued2 == 1)

  -- Drain rest
  let batch2 ← bp.drain
  assert "drain returns remaining 1" (batch2.size == 1)

  let (queued3, _) ← bp.stats
  assert "0 remaining after second drain" (queued3 == 0)

def testIdGeneration : IO Unit := do
  IO.println "ID generation:"
  let tid ← newTraceId
  assert "traceId length == 32" (tid.length == 32)

  let sid ← newSpanId
  assert "spanId length == 16" (sid.length == 16)

  -- IDs should be different
  let tid2 ← newTraceId
  assert "traceIds are unique" (tid != tid2)

def testHoneycombIntegration : IO Unit := do
  IO.println "Honeycomb integration:"
  let config : ExporterConfig := {
    apiKey := "zoFbjFUA5ErGjhw9T2CtWC"
    resource := { serviceName := "lean-otel-test" }
  }
  let result ← exportSpans config #[testSpan]
  assert "Honeycomb returns 200" (result.statusCode == 200)
  assert "no error" result.error.isNone

open LeanOtel in
def testAsyncProcessor : IO Unit := do
  IO.println "Async processor:"
  let ap ← AsyncProcessor.new {
    apiKey := "zoFbjFUA5ErGjhw9T2CtWC"
    maxQueueSize := 100
    maxExportBatchSize := 5
    scheduledDelayMs := 500
    resource := { serviceName := "lean-otel-test" }
  }

  -- Send 7 spans — should trigger one batch export at 5
  for i in List.range 7 do
    let span : Span := {
      traceId := "aaaa0000bbbb1111cccc2222dddd3333"
      spanId := "eeee4444ffff5555"
      name := s!"async-test-span-{i}"
      startTimeUnixNano := 1000000000
      endTimeUnixNano := 2000000000
      attributes := #[⟨"index", .int i⟩]
    }
    let ok ← ap.send span
    assert s!"send span {i}" ok

  -- Wait for timer to fire and export remaining 2
  IO.sleep 1000

  -- Shutdown — should drain anything left
  ap.shutdown

  let stats ← ap.getStats
  assert "all 7 exported" (stats.totalExported == 7)
  assert "none dropped" (stats.totalDropped == 0)
  assert "batch empty after shutdown" (stats.batch.isEmpty)

open LeanOtel in
def testAsyncShutdownRejects : IO Unit := do
  IO.println "Async shutdown rejects:"
  let ap ← AsyncProcessor.new {
    apiKey := "zoFbjFUA5ErGjhw9T2CtWC"
    maxQueueSize := 100
    maxExportBatchSize := 100
    scheduledDelayMs := 60000
    resource := { serviceName := "lean-otel-test" }
  }

  -- Send one span
  let ok ← ap.send testSpan
  assert "send before shutdown accepted" ok

  -- Shutdown
  ap.shutdown

  -- Send after shutdown should be rejected
  let ok2 ← ap.send testSpan
  assert "send after shutdown rejected" (!ok2)

def main : IO UInt32 := do
  IO.println "lean-otel test suite"
  IO.println "==================="
  try
    testQueueOps
    testIdGeneration
    testHoneycombIntegration
    testAsyncProcessor
    testAsyncShutdownRejects
    IO.println "\nAll tests passed!"
    return 0
  catch e =>
    IO.eprintln s!"\nTest suite failed: {e}"
    return 1
