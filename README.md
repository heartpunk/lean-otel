# lean-otel

OTLP trace exporter for Lean 4. Async batch processor, libcurl FFI, Honeycomb-compatible.

## Architecture

```
traced def myFunc : IO α        ←  macro wraps body in a span
    ↓
GlobalTracer (IO.Ref)            ←  singleton, set at startup
    ↓
BatchProcessor / AsyncProcessor  ←  bounded queue, background worker via CloseableChannel
    ↓
Export (libcurl FFI via leanCurl) ←  OTLP JSON over HTTPS, no subprocess
    ↓
Honeycomb / Jaeger / any OTLP endpoint
```

**Pure core / IO shell:** Queue logic (`BatchState.enqueue`, `BatchState.drain`, `BatchAccum`) is pure and proved. IO wrappers are thin shells over the proved functions.

## Usage

```lean
import LeanOtel
open LeanOtel

def main : IO Unit := do
  initGlobalTracer {
    apiKey := (← IO.getEnv "HONEYCOMB_API_KEY").getD ""
    resource := { serviceName := "my-service" }
  }
  let result ← myFunction 42
  stopGlobalTracer

traced def myFunction (n : Nat) : IO Nat := do
  return n + 1
```

`traced def` desugars to `withGlobalSpan "funcName" (body)`. If the global tracer is not initialized, the function runs untraced.

## Manual spans

```lean
match ← getGlobalTracer with
| some t => t.withSpan "name" (attrs := #[⟨"key", .str "val"⟩]) fun _ => body
| none => body
```

## Async export

`AsyncProcessor` uses `Std.CloseableChannel` with a background worker task. Spans ship in batches (configurable size + timer). Shutdown closes the channel; worker drains and exits cleanly.

## Proofs

19 theorems, 0 sorry. Queue bounds, batch accumulator correctness, hex ID validity, JSON structure, OTLP status codes.

## Tests

57 tests. Compile-time `#guard` + IO tests: queue ops, ID gen, file export, Honeycomb integration, async lifecycle, error paths, `traced def` macro, injectable time source.

## Dependencies

- [leanCurl](https://github.com/bergmannjg/leanCurl) (libcurl FFI; macOS: `brew reinstall curl`)
- Lean 4.27.0

## License

Dual MIT / Apache-2.0. Copyright (c) 2026 Sophie Smithburg.
