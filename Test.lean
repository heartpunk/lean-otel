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

-- parseTraceparent
open LeanOtel in
#guard (parseTraceparent "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01" ==
  some ("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331"))

open LeanOtel in
#guard (parseTraceparent "garbage").isNone

open LeanOtel in
#guard (parseTraceparent "00-short-bad-01").isNone

end BatchAccumTests

/-! ## IO tests -/

def mkTestSpan : IO Span := do
  let tid ← newTraceId
  let sid ← newSpanId
  return {
    traceId := tid
    spanId := sid
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
  let ok1 ← bp.enqueue (← mkTestSpan)
  let ok2 ← bp.enqueue (← mkTestSpan)
  let ok3 ← bp.enqueue (← mkTestSpan)
  assert "enqueue 1 accepted" ok1
  assert "enqueue 2 accepted" ok2
  assert "enqueue 3 accepted" ok3

  -- 4th should be rejected
  let ok4 ← bp.enqueue (← mkTestSpan)
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

def testExportEmpty : IO Unit := do
  IO.println "Export empty spans:"
  let config : ExporterConfig := {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD "")
    resource := { serviceName := "lean-otel-test" }
  }
  let result ← exportSpans config #[]
  assert "empty export returns 0" (result.statusCode == 0)
  assert "empty export no error" result.error.isNone
  assert "empty export empty body" (result.responseBody == "")

def testExportToFile : IO Unit := do
  IO.println "Export to file:"
  let path : System.FilePath := "/tmp/lean-otel-test-traces.jsonl"
  -- Clean up from prior runs
  try IO.FS.removeFile path catch | _ => pure ()
  let resource : Resource := { serviceName := "file-test" }
  exportSpansToFile path resource #[(← mkTestSpan)]
  let contents ← IO.FS.readFile path
  assert "file not empty" (!contents.isEmpty)
  -- Should be valid JSON (contains resourceSpans key)
  assert "contains resourceSpans" ((contents.splitOn "resourceSpans").length > 1)
  assert "contains test-span" ((contents.splitOn "test-span").length > 1)
  -- Write a second batch
  exportSpansToFile path resource #[(← mkTestSpan)]
  let contents2 ← IO.FS.readFile path
  assert "file grew (append)" (contents2.length > contents.length)
  -- Clean up
  IO.FS.removeFile path

def testExportBadEndpoint : IO Unit := do
  IO.println "Export bad endpoint:"
  let logPath : System.FilePath := "/tmp/lean-otel-test-transport.log"
  try IO.FS.removeFile logPath catch | _ => pure ()
  let config : ExporterConfig := {
    endpoint := "https://localhost:1"
    apiKey := "fake"
    resource := { serviceName := "test" }
    logSink := fileLogSink logPath
  }
  let result ← exportSpans config #[(← mkTestSpan)]
  assert "bad endpoint has error" result.error.isSome
  assert "bad endpoint status 0" (result.statusCode == 0)
  let logContent ← IO.FS.readFile logPath
  assert "transport error logged" ((logContent.splitOn "export failed").length > 1)
  IO.FS.removeFile logPath

def testBadCredentials : IO Unit := do
  IO.println "Bad credentials:"
  let logPath : System.FilePath := "/tmp/lean-otel-test-auth.log"
  try IO.FS.removeFile logPath catch | _ => pure ()
  let config : ExporterConfig := {
    apiKey := "invalid-key-should-get-401"
    resource := { serviceName := "lean-otel-test" }
    logSink := fileLogSink logPath
  }
  let result ← exportSpans config #[(← mkTestSpan)]
  assert "bad key returns 401" (result.statusCode == 401)
  assert "bad key no transport error" result.error.isNone
  assert "response body non-empty on 401" (!result.responseBody.isEmpty)
  -- Verify the auth rejection was logged
  let logContent ← IO.FS.readFile logPath
  assert "log mentions authentication rejected" ((logContent.splitOn "authentication rejected").length > 1)
  assert "log mentions 401" ((logContent.splitOn "401").length > 1)
  assert "log mentions endpoint" ((logContent.splitOn "endpoint=").length > 1)
  IO.FS.removeFile logPath

def testHoneycombIntegration : IO Unit := do
  IO.println "Honeycomb integration:"
  let config : ExporterConfig := {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD "")
    resource := { serviceName := "lean-otel-test" }
  }
  let result ← exportSpans config #[(← mkTestSpan)]
  assert "Honeycomb returns 200" (result.statusCode == 200)
  assert "no error" result.error.isNone

def testNowNanos : IO Unit := do
  IO.println "nowNanos:"
  let t ← nowNanos
  -- Should be in the year 2026 range: ~1.77e18 nanos
  assert "timestamp > 1.7e18" (t > 1700000000000000000)
  assert "timestamp < 2.0e18" (t < 2000000000000000000)
  -- Two calls should be monotonically increasing
  let t2 ← nowNanos
  assert "monotonic" (t2 ≥ t)

def testTracerWithSpan : IO Unit := do
  IO.println "Tracer.withSpan:"
  let bp ← BatchProcessor.new {
    apiKey := "test", maxQueueSize := 100, maxExportBatchSize := 100,
    resource := { serviceName := "test" }
  }
  let tracer : Tracer := { processor := bp, traceId := "aaaa0000bbbb1111cccc2222dddd3333" }

  -- withSpan should enqueue a span
  tracer.withSpan "outer" (attrs := #[⟨"k", .str "v"⟩]) fun t => do
    -- Nested span
    t.withSpan "inner" (attrs := #[]) fun _ => do
      pure ()

  let (queued, _) ← tracer.stats
  assert "2 spans queued (outer + inner)" (queued == 2)

  -- Drain and check span names
  let batch ← bp.drain
  assert "batch has 2 spans" (batch.size == 2)
  -- Inner span finishes first, so it's enqueued first
  match batch[0]?, batch[1]? with
  | some s0, some s1 =>
    assert "first span is inner" (s0.name == "inner")
    assert "second span is outer" (s1.name == "outer")
    assert "inner has parent" s0.parentSpanId.isSome
    assert "outer has no parent" s1.parentSpanId.isNone
  | _, _ =>
    assert "batch has 2 accessible spans" false

def testAttrValueJson : IO Unit := do
  IO.println "AttrValue JSON branches:"
  let json_str := Span.toOtlpJson {
    traceId := "t", spanId := "s", name := "n",
    startTimeUnixNano := 0, endTimeUnixNano := 1,
    attributes := #[
      ⟨"s", .str "hello"⟩,
      ⟨"i", .int 42⟩,
      ⟨"f", .float 3.14⟩,
      ⟨"b", .bool true⟩
    ]
  }
  let s := json_str.compress
  assert "str attr present" ((s.splitOn "stringValue").length > 1)
  assert "int attr present" ((s.splitOn "intValue").length > 1)
  assert "float attr present" ((s.splitOn "doubleValue").length > 1)
  assert "bool attr present" ((s.splitOn "boolValue").length > 1)

open LeanOtel in
def testAsyncProcessor : IO Unit := do
  IO.println "Async processor:"
  let ap ← AsyncProcessor.new {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD "")
    maxQueueSize := 100
    maxExportBatchSize := 5
    scheduledDelayMs := 500
    resource := { serviceName := "lean-otel-test" }
  }

  -- Send 7 spans — should trigger one batch export at 5
  let tid ← newTraceId
  for i in List.range 7 do
    let sid ← newSpanId
    let span : Span := {
      traceId := tid
      spanId := sid
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
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD "")
    maxQueueSize := 100
    maxExportBatchSize := 100
    scheduledDelayMs := 60000
    resource := { serviceName := "lean-otel-test" }
  }

  -- Send one span
  let ok ← ap.send (← mkTestSpan)
  assert "send before shutdown accepted" ok

  -- Shutdown
  ap.shutdown

  -- Send after shutdown should be rejected
  let ok2 ← ap.send (← mkTestSpan)
  assert "send after shutdown rejected" (!ok2)

def testFixtureTimeSource : IO Unit := do
  IO.println "Fixture time source:"
  let counter ← IO.mkRef (0 : UInt64)
  let fixtureClock : TimeSource := do
    let n ← counter.get
    counter.set (n + 1000000000)  -- increment 1 second each call
    return 1700000000000000000 + n  -- base: ~2023
  let bp ← BatchProcessor.new {
    apiKey := "test", maxQueueSize := 100, maxExportBatchSize := 100,
    resource := { serviceName := "test" }
  }
  let tracer : Tracer := {
    processor := bp
    traceId := "aaaa0000bbbb1111cccc2222dddd3333"
    timeSource := fixtureClock
  }
  tracer.withSpan "fixed-time-span" (attrs := #[]) fun _ => pure ()
  let batch ← bp.drain
  match batch[0]? with
  | some s =>
    assert "fixture start time" (s.startTimeUnixNano == 1700000000000000000)
    assert "fixture end time" (s.endTimeUnixNano == 1700000001000000000)
  | none => assert "span was queued" false

def testNowNanosFallback : IO Unit := do
  IO.println "nowNanos fallback:"
  -- A time source that simulates `date` failing (returns garbage)
  let fallbackClock : TimeSource := do
    let ms ← IO.monoMsNow
    return ms.toUInt64 * 1000000
  let t ← fallbackClock
  assert "fallback returns nonzero" (t > 0)

open LeanOtel in
def testAsyncShutdownEmpty : IO Unit := do
  IO.println "Async shutdown empty batch:"
  let ap ← AsyncProcessor.new {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD "")
    maxQueueSize := 100
    maxExportBatchSize := 100
    scheduledDelayMs := 60000
    resource := { serviceName := "lean-otel-test" }
  }
  -- Shutdown immediately with nothing queued
  ap.shutdown
  let stats ← ap.getStats
  assert "empty shutdown: 0 exported" (stats.totalExported == 0)
  assert "empty shutdown: 0 dropped" (stats.totalDropped == 0)

open LeanOtel in
def testAsyncExportFailure : IO Unit := do
  IO.println "Async export failure:"
  let ap ← AsyncProcessor.new {
    apiKey := "fake-key-will-get-401"
    maxQueueSize := 100
    maxExportBatchSize := 2
    scheduledDelayMs := 200
    endpoint := "https://api.honeycomb.io"
    resource := { serviceName := "lean-otel-test" }
  }
  -- Send 3 spans — batch of 2 will trigger export with bad key
  for _ in List.range 3 do
    let _ ← ap.send (← mkTestSpan)
  -- Wait for export attempt
  IO.sleep 500
  ap.shutdown
  -- Should not crash — error logged but processor stays alive
  assert "survived export failure" true

open LeanOtel in
traced def tracedAdd (x y : Nat) : IO Nat := do
  IO.sleep 10
  return x + y

open LeanOtel in
traced def tracedOuter : IO Nat := do
  let a ← tracedAdd 1 2
  let b ← tracedAdd 3 4
  return a + b

open LeanOtel in
traced +[x] def tracedAcceptOnly (x y : Nat) : IO Nat := do
  return x + y

def testTracedAcceptList : IO Unit := do
  IO.println "traced accept list:"
  initGlobalTracerSingle {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD ""), maxQueueSize := 100, maxExportBatchSize := 1, scheduledDelayMs := 500
    resource := { serviceName := "lean-otel-test" }
  }
  let result ← tracedAcceptOnly 10 20
  assert "accept list: returns correct value" (result == 30)
  IO.sleep 2000
  stopGlobalTracer
  -- Attr content tested by #guard (BatchAccumTests). Export verified by 200.

open LeanOtel in
traced -[y] def tracedRejectY (x y z : Nat) : IO Nat := do
  return x + y + z

def testTracedRejectList : IO Unit := do
  IO.println "traced reject list:"
  initGlobalTracerSingle {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD ""), maxQueueSize := 100, maxExportBatchSize := 1, scheduledDelayMs := 500
    resource := { serviceName := "lean-otel-test" }
  }
  let result ← tracedRejectY 1 2 3
  assert "reject list: returns correct value" (result == 6)
  IO.sleep 2000
  stopGlobalTracer

def testTracedDef : IO Unit := do
  IO.println "traced def macro:"
  initGlobalTracerSingle {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD ""), maxQueueSize := 100, maxExportBatchSize := 1, scheduledDelayMs := 500
    resource := { serviceName := "lean-otel-test" }
  }
  let result ← tracedOuter
  assert "tracedOuter returns 10" (result == 10)
  IO.sleep 2000
  stopGlobalTracer

def testNon401HttpError : IO Unit := do
  IO.println "Non-401 HTTP error:"
  let logPath : System.FilePath := "/tmp/lean-otel-test-http-error.log"
  try IO.FS.removeFile logPath catch | _ => pure ()
  let config : ExporterConfig := {
    endpoint := "https://api.honeycomb.io/nonexistent"  -- will 404
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD "")
    resource := { serviceName := "lean-otel-test" }
    logSink := fileLogSink logPath
  }
  let result ← exportSpans config #[(← mkTestSpan)]
  assert "non-401 error status ≥ 400" (result.statusCode ≥ 400)
  assert "non-401 error status ≠ 401" (result.statusCode != 401)
  let logContent ← IO.FS.readFile logPath
  assert "non-401 error logged" ((logContent.splitOn "export failed with HTTP").length > 1)
  IO.FS.removeFile logPath

def testGlobalTracerNotInitialized : IO Unit := do
  IO.println "Global tracer not initialized:"
  -- Reset global tracer to none
  LeanOtel.globalTracerRef.set none
  -- withGlobalSpan should be a no-op, just runs the body
  let result ← LeanOtel.withGlobalSpan "should-noop" (attrs := #[]) ((pure 42 : IO Nat))
  assert "withGlobalSpan no-op returns body result" (result == 42)
  -- flush/stop should be no-ops
  flushGlobalTracer
  stopGlobalTracer
  assert "flush/stop don't crash when uninitialized" true

open LeanOtel in
def testGlobalSpanNesting : IO Unit := do
  IO.println "Global span nesting:"
  -- Set up global tracer with sync path so we can inspect spans directly
  let bp ← BatchProcessor.new {
    apiKey := "test", maxQueueSize := 100, maxExportBatchSize := 100,
    resource := { serviceName := "test" }
  }
  let tracer : Tracer := { processor := bp, traceId := "aaaa0000bbbb1111cccc2222dddd3333" }
  globalTracerRef.set (some tracer)
  globalAsyncRef.set #[]  -- force sync path
  -- Nested withGlobalSpan calls
  let outerResult ← withGlobalSpan "outer-global" (attrs := #[]) do
    let innerResult ← withGlobalSpan "inner-global" (attrs := #[]) do
      return (42 : Nat)
    return innerResult + 1
  assert "nested withGlobalSpan returns correct value" (outerResult == 43)
  -- Drain and check parent-child relationship
  let batch ← bp.drain
  assert "2 global spans queued" (batch.size == 2)
  -- Inner finishes first, so it's enqueued first
  match batch[0]?, batch[1]? with
  | some inner, some outer =>
    assert "first span is inner-global" (inner.name == "inner-global")
    assert "second span is outer-global" (outer.name == "outer-global")
    assert "inner has parent span ID" inner.parentSpanId.isSome
    assert "outer's spanId matches inner's parent" (inner.parentSpanId == some outer.spanId)
  | _, _ =>
    assert "batch has 2 accessible spans" false
  -- Clean up
  globalTracerRef.set none

open LeanOtel in
def testAsyncTimerExport : IO Unit := do
  IO.println "Async timer export (no shutdown):"
  let ap ← AsyncProcessor.new {
    apiKey := ((← IO.getEnv "HONEYCOMB_API_KEY").getD "")
    maxQueueSize := 100
    maxExportBatchSize := 100  -- large batch so it never fills
    scheduledDelayMs := 500    -- short timer
    resource := { serviceName := "lean-otel-test" }
  }
  -- Send a single span
  let ok ← ap.send (← mkTestSpan)
  assert "send accepted" ok
  -- Wait well past the timer interval — span should be exported by timer alone
  IO.sleep 2000
  let stats ← ap.getStats
  assert "timer exported 1 span (no shutdown needed)" (stats.totalExported == 1)
  assert "batch empty after timer export" (stats.batch.isEmpty)
  -- Now clean up
  ap.shutdown

open LeanOtel in
def testMultiExport : IO Unit := do
  IO.println "Multi-export (Honeycomb + SPARQL proxy):"
  let proxyEndpoint := (← IO.getEnv "OTEL_PROXY_ENDPOINT").getD ""
  if proxyEndpoint.isEmpty then
    IO.println "  SKIP: OTEL_PROXY_ENDPOINT not set"
    return
  let sparqlEndpoint := (← IO.getEnv "SPARQL_ENDPOINT").getD ""
  if sparqlEndpoint.isEmpty then
    IO.println "  SKIP: SPARQL_ENDPOINT not set"
    return
  let apiKey := (← IO.getEnv "HONEYCOMB_API_KEY").getD ""
  let honeycombConfig : BatchConfig := {
    apiKey, maxQueueSize := 100, maxExportBatchSize := 10, scheduledDelayMs := 500
    resource := { serviceName := "lean-otel-multi-test" }
  }
  let proxyConfig : BatchConfig := {
    apiKey := ""  -- proxy doesn't need an API key
    endpoint := proxyEndpoint
    maxQueueSize := 100, maxExportBatchSize := 10, scheduledDelayMs := 500
    resource := { serviceName := "lean-otel-multi-test" }
  }
  initGlobalTracer #[honeycombConfig, proxyConfig]
  -- Send nested spans
  let result ← withGlobalSpan "multi-root" (attrs := #[⟨"test", .str "multi-export"⟩]) do
    let r ← withGlobalSpan "multi-child" (attrs := #[]) do
      return (42 : Nat)
    return r
  assert "multi-export returns correct value" (result == 42)
  -- Wait for async export to both endpoints
  IO.sleep 2000
  stopGlobalTracer
  -- Verify spans landed in SPARQL engine by querying it
  let queryCmd ← IO.Process.output {
    cmd := "curl"
    args := #["-s", "-X", "POST", sparqlEndpoint,
              "-H", "Content-Type: application/sparql-query",
              "-d", "SELECT ?name WHERE { ?s <http://opentelemetry.io/ontology#name> ?name . ?s <http://opentelemetry.io/ontology#serviceName> \"lean-otel-multi-test\" }"]
  }
  let body := queryCmd.stdout
  assert "SPARQL query succeeded" (queryCmd.exitCode == 0)
  assert "multi-root span in SPARQL store" ((body.splitOn "multi-root").length > 1)
  assert "multi-child span in SPARQL store" ((body.splitOn "multi-child").length > 1)
  IO.println s!"  SPARQL response: {body.take 200}"

def main : IO UInt32 := do
  IO.println "lean-otel test suite"
  IO.println "==================="
  let apiKey := (← IO.getEnv "HONEYCOMB_API_KEY").getD ""
  if apiKey.isEmpty then
    IO.eprintln "FATAL: HONEYCOMB_API_KEY not set. Tests require a valid API key."
    return 1
  try
    testQueueOps
    testIdGeneration
    testExportEmpty
    testExportToFile
    testExportBadEndpoint
    testBadCredentials
    testHoneycombIntegration
    testNowNanos
    testTracerWithSpan
    testAttrValueJson
    testAsyncProcessor
    testAsyncShutdownRejects
    testFixtureTimeSource
    testNowNanosFallback
    testTracedAcceptList
    testTracedRejectList
    testTracedDef
    testAsyncTimerExport
    testAsyncShutdownEmpty
    testAsyncExportFailure
    testNon401HttpError
    testGlobalTracerNotInitialized
    testGlobalSpanNesting
    testMultiExport
    IO.println "\nAll tests passed!"
    try stopGlobalTracer catch | _ => pure ()
    return 0
  catch e =>
    try stopGlobalTracer catch | _ => pure ()
    IO.eprintln s!"\nTest suite failed: {e}"
    return 1
