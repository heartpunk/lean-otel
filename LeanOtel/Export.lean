/-
  OTLP HTTP JSON exporter.
  Sends spans to any OTLP-compatible endpoint (Honeycomb, Jaeger, OTel Collector).
-/
import LeanOtel.Json

namespace LeanOtel

/-- Configuration for the OTLP HTTP exporter. -/
structure ExporterConfig where
  endpoint : String := "https://api.honeycomb.io"
  apiKey : String
  headers : Array (String × String) := #[]
  resource : Resource
deriving Repr

/-- Build curl arguments for sending spans. -/
private def curlArgs (config : ExporterConfig) (jsonStr : String) : Array String :=
  let url := s!"{config.endpoint}/v1/traces"
  let base : Array String := #[
    "-s", "-o", "/dev/null", "-w", "%{http_code}",
    "-X", "POST",
    url,
    "-H", "Content-Type: application/json",
    "-H", s!"x-honeycomb-team: {config.apiKey}",
    "-d", jsonStr
  ]
  let extra := config.headers.foldl (init := #[]) fun acc (k, v) =>
    acc.push "-H" |>.push s!"{k}: {v}"
  base ++ extra

/-- Export spans to the configured endpoint. Returns HTTP status code. -/
def exportSpans (config : ExporterConfig) (spans : Array Span) : IO UInt32 := do
  if spans.isEmpty then return 0
  let json := mkTraceExportRequest config.resource spans
  let jsonStr := json.compress
  let args := curlArgs config jsonStr
  let out ← IO.Process.output { cmd := "curl", args := args }
  let trimmed := out.stdout.trimRight
  match trimmed.toNat? with
  | some code => return (code % 1000).toUInt32
  | none =>
    IO.eprintln s!"lean-otel: failed to parse HTTP status from curl output: '{trimmed}'"
    IO.eprintln s!"lean-otel: curl stderr: {out.stderr}"
    return 999

/-- File exporter: append spans as OTLP JSON lines to a file (for offline/archive). -/
def exportSpansToFile (path : System.FilePath) (resource : Resource) (spans : Array Span) : IO Unit := do
  let json := mkTraceExportRequest resource spans
  let h ← IO.FS.Handle.mk path .append
  h.putStrLn json.compress

end LeanOtel
