/-
  OTLP JSON serialization for spans.
  Produces the exact JSON format Honeycomb expects at POST /v1/traces.
-/
import LeanOtel.Span
import Lean.Data.Json

namespace LeanOtel

open Lean Json

private def attrValueToJson : AttrValue → Json
  | .str s => Json.mkObj [("stringValue", Json.str s)]
  | .int n => Json.mkObj [("intValue", Json.str (toString n))]
  | .float f => Json.mkObj [("doubleValue", Json.str (toString f))]
  | .bool b => Json.mkObj [("boolValue", Json.bool b)]

private def attributeToJson (a : Attribute) : Json :=
  Json.mkObj [("key", Json.str a.key), ("value", attrValueToJson a.value)]

private def statusCodeToInt : StatusCode → Nat
  | .unset => 0
  | .ok => 1
  | .error => 2

def Span.toOtlpJson (s : Span) : Json :=
  let fields : List (String × Json) := [
    ("traceId", Json.str s.traceId),
    ("spanId", Json.str s.spanId),
    ("name", Json.str s.name),
    ("startTimeUnixNano", Json.str (toString s.startTimeUnixNano)),
    ("endTimeUnixNano", Json.str (toString s.endTimeUnixNano)),
    ("attributes", Json.arr (s.attributes.map attributeToJson)),
    ("status", Json.mkObj [("code", Json.str (toString (statusCodeToInt s.status)))])
  ]
  let withParent := match s.parentSpanId with
    | some pid => ("parentSpanId", Json.str pid) :: fields
    | none => fields
  Json.mkObj withParent

def Resource.toOtlpJson (r : Resource) : Json :=
  let svcAttr := attributeToJson ⟨"service.name", .str r.serviceName⟩
  let attrs := #[svcAttr] ++ r.attributes.map attributeToJson
  Json.mkObj [("attributes", Json.arr attrs)]

/-- Wrap spans into the full OTLP ExportTraceServiceRequest JSON envelope. -/
def mkTraceExportRequest (resource : Resource) (spans : Array Span) : Json :=
  Json.mkObj [
    ("resourceSpans", Json.arr #[
      Json.mkObj [
        ("resource", resource.toOtlpJson),
        ("scopeSpans", Json.arr #[
          Json.mkObj [
            ("scope", Json.mkObj [("name", Json.str "lean-otel")]),
            ("spans", Json.arr (spans.map Span.toOtlpJson))
          ]
        ])
      ]
    ])
  ]

end LeanOtel
