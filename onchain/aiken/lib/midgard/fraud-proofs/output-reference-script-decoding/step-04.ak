use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  OutputDescriptorStateV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  /// Reference input holding the script's decode record when the item is a
  /// Plutus script, or -1.
  record_reference_input_index: Int,
  /// `None` reads the output by walking the field; `Some(i)` reads it through
  /// the field grammar certificate at reference input `i`, so the step's cost
  /// no longer grows with the field's item count.
  grammar_ref_input_index: Option<Int>,
}

pub type Datum =
  ct.StepDatum<OutputDescriptorStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
