use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/redeemer_canonicity/rule.{BoundRedeemerV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  /// `True` names the arm that answers a forced rejection whose coordinate
  /// field 8 does not have: a negative redeemer, one at or past the field's
  /// authenticated count, or a `PlutusExecutionFailed` of a transaction with
  /// no redeemers. The validator decides the claim from the authenticated
  /// count, never from the flag alone; the flag only spares the item arm a
  /// count that a walked variable-width field under tier 3 cannot answer.
  coordinate_absent: Bool,
  /// `None` reads the redeemer by walking field 8; `Some(i)` reads it through
  /// the field grammar certificate at reference input `i`, so the step's cost
  /// does not grow with the field's item count.
  grammar_ref_input_index: Option<Int>,
}

pub type Datum =
  ct.StepDatum<BoundRedeemerV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
