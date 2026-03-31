import LeanOtel

open LeanOtel

def main : IO Unit := do
  let config : BatchConfig := {
    apiKey := "zoFbjFUA5ErGjhw9T2CtWC"
    maxExportBatchSize := 10
    scheduledDelayMs := 1000
    resource := { serviceName := "lean-otel-test" }
  }
  let tracer ← Tracer.new config

  tracer.withSpan "integration-test" (attrs := #[⟨"test.source", .str "lean-otel async"⟩]) fun t => do
    IO.println "inside test span..."

    t.withSpan "child-span" (attrs := #[⟨"child.index", .int 1⟩]) fun _ => do
      IO.println "inside child span..."
      IO.sleep 50

    t.withSpan "another-child" (attrs := #[⟨"child.index", .int 2⟩]) fun _ => do
      IO.println "inside another child..."
      IO.sleep 30

  let (queued, dropped) ← tracer.stats
  IO.println s!"before flush: queued={queued}, dropped={dropped}"

  tracer.flush

  let (queued2, _) ← tracer.stats
  IO.println s!"after flush: queued={queued2}"
  IO.println "done"
