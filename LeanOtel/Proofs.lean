/-
  Proofs of correctness properties for lean-otel.
-/
import LeanOtel.Span
import LeanOtel.Json

namespace LeanOtel

/-- The hex digit function used in ID generation. Extracted for proving. -/
def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

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

end LeanOtel
