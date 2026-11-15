use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/output_reference_script_decoding/rule.{BoundOutputV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  /// `True` names the arm that answers a forced rejection whose output
  /// coordinate field 2 does not have: a negative position, or one at or past
  /// the field's authenticated count. The validator decides the claim from
  /// that count, never from the flag alone.
  coordinate_absent: Bool,
  /// `None` reads the output by walking the field; `Some(i)` reads it through
  /// the field grammar certificate at reference input `i`, so the step's cost
  /// no longer grows with the field's item count.
  grammar_ref_input_index: Option<Int>,
}

pub type Datum =
  ct.StepDatum<BoundOutputV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
