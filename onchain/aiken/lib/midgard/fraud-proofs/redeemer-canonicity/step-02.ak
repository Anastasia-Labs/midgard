use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/redeemer_canonicity/rule.{BoundRedeemerV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  /// `None` reads the redeemer by walking field 8; `Some(i)` reads it through
  /// the field grammar certificate at reference input `i`, so the step's cost
  /// no longer grows with the field's item count.
  grammar_ref_input_index: Option<Int>,
}

pub type Datum =
  ct.StepDatum<BoundRedeemerV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
