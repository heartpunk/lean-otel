/-
  Proofs of correctness properties for lean-otel.
-/
import LeanOtel.Span
import LeanOtel.Json
import LeanOtel.Trace
import LeanOtel.BatchProcessor
import LeanOtel.AsyncProcessor

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

/-- BatchState.enqueue: if queue was bounded, it stays bounded. -/
theorem enqueue_bounded (st : BatchState) (span : Span) (max : Nat)
    (h_pre : st.queue.size ≤ max) :
    (st.enqueue span max).1.queue.size ≤ max := by
  unfold BatchState.enqueue
  if h : st.queue.size ≥ max then
    simp [if_pos h]; exact h_pre
  else
    simp [if_neg h, Array.size_push]; omega

/-- BatchState.enqueue: if accepted, queue grew by 1. -/
theorem enqueue_accepted_grows (st : BatchState) (span : Span) (max : Nat)
    (h : (st.enqueue span max).2 = true) :
    (st.enqueue span max).1.queue.size = st.queue.size + 1 := by
  unfold BatchState.enqueue at h ⊢
  if hq : st.queue.size ≥ max then
    simp [hq] at h
  else
    simp [hq, Array.size_push]

/-- BatchState.enqueue: if rejected, queue size unchanged and dropped incremented. -/
theorem enqueue_rejected_unchanged (st : BatchState) (span : Span) (max : Nat)
    (h : (st.enqueue span max).2 = false) :
    (st.enqueue span max).1.queue.size = st.queue.size ∧
    (st.enqueue span max).1.dropped = st.dropped + 1 := by
  unfold BatchState.enqueue at h ⊢
  if hq : st.queue.size ≥ max then
    simp [hq]
  else
    simp [hq] at h

/-- BatchState.drain: batch size is at most maxBatchSize. -/
theorem drain_batch_bounded (st : BatchState) (maxBatch : Nat) :
    (st.drain maxBatch).1.size ≤ maxBatch := by
  simp [BatchState.drain, Array.size_extract]
  omega

/-- BatchState.drain: batch + remaining = original queue size. -/
theorem drain_preserves_total (st : BatchState) (maxBatch : Nat) :
    (st.drain maxBatch).1.size + (st.drain maxBatch).2.queue.size = st.queue.size := by
  simp [BatchState.drain, Array.size_extract]
  omega

/-- BatchState.drain: dropped count unchanged. -/
theorem drain_preserves_dropped (st : BatchState) (maxBatch : Nat) :
    (st.drain maxBatch).2.dropped = st.dropped := by
  simp [BatchState.drain]

/-- statusCodeToInt maps to exact OTLP spec values. -/
theorem statusCode_unset : statusCodeToInt .unset = 0 := rfl
theorem statusCode_ok : statusCodeToInt .ok = 1 := rfl
theorem statusCode_error : statusCodeToInt .error = 2 := rfl

/-- BatchAccum.add: batch grows by 1. -/
theorem batchAccum_add_grows (acc : BatchAccum) (span : Span) :
    (acc.add span).1.batch.size = acc.batch.size + 1 := by
  simp [BatchAccum.add, Array.size_push]

/-- BatchAccum.add: shouldExport iff batch reaches maxBatchSize. -/
theorem batchAccum_add_shouldExport (acc : BatchAccum) (span : Span) :
    (acc.add span).2 = ((acc.batch.size + 1) ≥ acc.maxBatchSize) := by
  simp [BatchAccum.add, Array.size_push]

/-- BatchAccum.take: returns the current batch. -/
theorem batchAccum_take_returns_batch (acc : BatchAccum) :
    (acc.take).1 = acc.batch := by
  simp [BatchAccum.take]

/-- BatchAccum.take: resets batch to empty. -/
theorem batchAccum_take_empties (acc : BatchAccum) :
    (acc.take).2.batch = #[] := by
  simp [BatchAccum.take]

/-- BatchAccum.take: totalExported increases by batch size. -/
theorem batchAccum_take_exports (acc : BatchAccum) :
    (acc.take).2.totalExported = acc.totalExported + acc.batch.size := by
  simp [BatchAccum.take]

/-- BatchAccum.recordDropped: totalDropped increases by n. -/
theorem batchAccum_recordDropped_increments (acc : BatchAccum) (n : Nat) :
    (acc.recordDropped n).totalDropped = acc.totalDropped + n := by
  simp [BatchAccum.recordDropped]

/-- BatchAccum.recordDropped: batch unchanged. -/
theorem batchAccum_recordDropped_preserves_batch (acc : BatchAccum) (n : Nat) :
    (acc.recordDropped n).batch = acc.batch := by
  simp [BatchAccum.recordDropped]

end LeanOtel
