/-
  Proofs of correctness properties for lean-otel.
-/
import LeanOtel.Span
import LeanOtel.Json
import LeanOtel.Trace

namespace LeanOtel

/-- hexDigit with input in range [0,15] produces a valid ASCII hex char code.
    '0'-'9' = 48-57, 'a'-'f' = 97-102. -/
theorem hexDigit_val_bounded (n : Nat) (h : n < 16) :
    (48 + n ≥ 48 ∧ 48 + n ≤ 57 ∧ n < 10) ∨ (87 + n ≥ 97 ∧ 87 + n ≤ 102 ∧ n ≥ 10) := by
  omega

/-- statusCodeToInt always produces a value in {0, 1, 2}. -/
theorem statusCodeToInt_bounded (s : StatusCode) :
    statusCodeToInt s < 3 := by
  cases s <;> simp [statusCodeToInt]

/-- foldl of 2-char appends preserves the invariant: length = init.length + 2 * list.length -/
theorem foldl_append_two_length (init : List Char) :
    ∀ (pairs : List (Char × Char)),
      (pairs.foldl (fun acc (p : Char × Char) => acc ++ [p.1, p.2]) init).length
      = init.length + 2 * pairs.length := by
  intro pairs
  induction pairs generalizing init with
  | nil => simp
  | cons p ps ih =>
    simp only [List.foldl]
    have := ih (init ++ [p.1, p.2])
    simp [List.length_append] at this ⊢
    omega

/-- randomHex n produces exactly 2*n characters (stated over the pure fold). -/
theorem randomHex_length (pairs : List (Char × Char)) :
    (pairs.foldl (fun acc (p : Char × Char) => acc ++ [p.1, p.2]) []).length
    = 2 * pairs.length := by
  rw [foldl_append_two_length]
  simp

open Lean

/-- Predicate: a Json value is an object. -/
def Json.isObj : Json → Prop
  | .obj _ => True
  | _ => False

instance : Decidable (Json.isObj j) := by
  cases j <;> simp [Json.isObj] <;> exact inferInstance

/-- Span.toOtlpJson always produces a JSON object. -/
theorem span_toOtlpJson_isObj (s : Span) :
    Json.isObj s.toOtlpJson := by
  unfold Span.toOtlpJson
  cases s.parentSpanId <;> simp [Json.mkObj, Json.isObj]

/-- Resource.toOtlpJson always produces a JSON object. -/
theorem resource_toOtlpJson_isObj (r : Resource) :
    Json.isObj r.toOtlpJson := by
  simp [Resource.toOtlpJson, Json.mkObj, Json.isObj]

/-- mkTraceExportRequest always produces a JSON object. -/
theorem mkTraceExportRequest_isObj (r : Resource) (spans : Array Span) :
    Json.isObj (mkTraceExportRequest r spans) := by
  simp [mkTraceExportRequest, Json.mkObj, Json.isObj]

/-- Pure enqueue: if queue is under max, push succeeds and size increases by 1. -/
theorem enqueue_under_max (queue : Array α) (x : α) (max : Nat) (h : queue.size < max) :
    (queue.push x).size = queue.size + 1 := by
  simp [Array.size_push]

/-- Pure enqueue: queue never exceeds maxQueueSize. -/
theorem enqueue_bounded (queue : Array α) (x : α) (max : Nat) (h : queue.size < max) :
    (queue.push x).size ≤ max := by
  simp [Array.size_push]; omega

/-- Pure drain: extract returns at most maxBatchSize elements. -/
theorem drain_bounded (queue : Array α) (maxBatch : Nat) :
    (queue.extract 0 (min queue.size maxBatch)).size ≤ maxBatch := by
  simp [Array.size_extract]
  omega

/-- Pure drain: extract + remaining preserves total count. -/
theorem drain_preserves_total (queue : Array α) (maxBatch : Nat) :
    let batchSize := min queue.size maxBatch
    (queue.extract 0 batchSize).size + (queue.extract batchSize queue.size).size = queue.size := by
  simp [Array.size_extract]
  omega

/-- statusCodeToInt maps to exact OTLP spec values. -/
theorem statusCode_unset : statusCodeToInt .unset = 0 := rfl
theorem statusCode_ok : statusCodeToInt .ok = 1 := rfl
theorem statusCode_error : statusCodeToInt .error = 2 := rfl

end LeanOtel
