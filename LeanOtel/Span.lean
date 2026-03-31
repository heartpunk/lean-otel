/-
  OpenTelemetry Span types for Lean 4.
  Minimal implementation targeting OTLP JSON over HTTP.
-/
import Lean.Data.Json

namespace LeanOtel

/-- Attribute value: string, int, float, or bool. -/
inductive AttrValue where
  | str : String → AttrValue
  | int : Int → AttrValue
  | float : Float → AttrValue
  | bool : Bool → AttrValue
deriving Repr

/-- A key-value attribute. -/
structure Attribute where
  key : String
  value : AttrValue
deriving Repr

/-- Span status code per OTLP spec. -/
inductive StatusCode where
  | unset
  | ok
  | error
deriving Repr

/-- A completed span ready for export. -/
structure Span where
  traceId : String          -- 32 hex chars
  spanId : String           -- 16 hex chars
  parentSpanId : Option String := none
  name : String
  startTimeUnixNano : UInt64
  endTimeUnixNano : UInt64
  attributes : Array Attribute := #[]
  status : StatusCode := .unset
deriving Repr

/-- Resource attributes identifying the service. -/
structure Resource where
  serviceName : String
  attributes : Array Attribute := #[]
deriving Repr

end LeanOtel
