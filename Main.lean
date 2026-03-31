import LeanOtel

open LeanOtel

def main : IO Unit := do
  let config : ExporterConfig := {
    apiKey := "zoFbjFUA5ErGjhw9T2CtWC"
    resource := { serviceName := "lean-otel-test" }
  }
  let ctx ← TracerContext.new config

  ctx.withSpan "integration-test" (attrs := #[⟨"test.source", .str "lean-otel Main.lean"⟩]) fun ctx => do
    IO.println "inside test span..."

    ctx.withSpan "child-span" (attrs := #[⟨"child.index", .int 1⟩]) fun _ => do
      IO.println "inside child span..."
      IO.sleep 100

    ctx.withSpan "another-child" (attrs := #[⟨"child.index", .int 2⟩]) fun _ => do
      IO.println "inside another child..."
      IO.sleep 50

  -- Dump JSON for debugging
  let spans ← ctx.pendingSpans.get
  let json := LeanOtel.mkTraceExportRequest config.resource spans
  IO.println s!"JSON: {json.compress}"

  let code ← ctx.flush
  IO.println s!"export status: {code}"
  if code == 200 then
    IO.println "SUCCESS: Honeycomb accepted our traces!"
  else
    IO.println s!"FAILED: got HTTP {code}"
