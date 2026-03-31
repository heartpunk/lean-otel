/-
  OTLP HTTP JSON exporter using libcurl FFI.
  No subprocess spawning. In-process HTTP POST.
-/
import LeanOtel.Json
import Curl

namespace LeanOtel

open Curl

/-- Configuration for the OTLP HTTP exporter. -/
structure ExporterConfig where
  endpoint : String := "https://api.honeycomb.io"
  apiKey : String
  headers : Array (String × String) := #[]
  resource : Resource
deriving Repr

/-- Result of an export attempt. -/
structure ExportResult where
  statusCode : UInt32
  responseBody : String
  error : Option String := none
deriving Repr

/-- Export spans to the configured OTLP endpoint via libcurl.
    Returns the HTTP status code and response body. -/
def exportSpans (config : ExporterConfig) (spans : Array Span) : IO ExportResult := do
  if spans.isEmpty then return { statusCode := 0, responseBody := "", error := none }

  let json := mkTraceExportRequest config.resource spans
  let jsonStr := json.compress
  let url := s!"{config.endpoint}/v1/traces"

  try
    let response ← IO.mkRef { : IO.FS.Stream.Buffer }
    let curl ← curl_easy_init
    curl_set_option curl (CurlOption.URL url)
    curl_set_option curl (CurlOption.VERBOSE 0)
    curl_set_option curl (CurlOption.COPYPOSTFIELDS jsonStr)

    -- Build header list
    let mut hdrs : Array String := #[
      "Content-Type: application/json",
      s!"x-honeycomb-team: {config.apiKey}"
    ]
    for (k, v) in config.headers do
      hdrs := hdrs.push s!"{k}: {v}"
    curl_set_option curl (CurlOption.HTTPHEADER hdrs)

    curl_set_option curl (CurlOption.WRITEDATA response)
    curl_set_option curl (CurlOption.WRITEFUNCTION Curl.writeBytes)

    curl_easy_perform curl

    let bytes ← response.get
    let body := String.fromUTF8! bytes.data
    -- TODO: extract HTTP status code from curl (curl_easy_getinfo)
    -- For now, if curl_easy_perform didn't throw, assume 200
    return { statusCode := 200, responseBody := body, error := none }

  catch e =>
    let msg := s!"lean-otel: export failed: {e}"
    IO.eprintln msg
    return { statusCode := 0, responseBody := "", error := some msg }

/-- File exporter: append spans as OTLP JSON lines to a file (for offline/archive). -/
def exportSpansToFile (path : System.FilePath) (resource : Resource) (spans : Array Span) : IO Unit := do
  let json := mkTraceExportRequest resource spans
  let h ← IO.FS.Handle.mk path .append
  h.putStrLn json.compress

end LeanOtel
