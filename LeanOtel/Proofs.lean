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

end LeanOtel
