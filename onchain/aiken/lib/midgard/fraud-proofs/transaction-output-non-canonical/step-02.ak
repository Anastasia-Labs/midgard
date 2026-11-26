use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{BoundOutputV1}

pub type Datum =
  ct.StepDatum<BoundOutputV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  /// `None` reads the output by walking the field; `Some(i)` reads it through
  /// the field grammar certificate at reference input `i`, so the step's cost
  /// does not grow with the field's item count.
  grammar_ref_input_index: Option<Int>,
  /// `True` closes a rejection whose output field 2 does not have: a negative
  /// coordinate, or one at or past the field's count. The step then reads no
  /// item and hands the absent state to the terminal.
  coordinate_absent: Bool,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
