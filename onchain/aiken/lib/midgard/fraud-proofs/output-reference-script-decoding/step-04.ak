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
}

pub type Datum =
  ct.StepDatum<OutputDescriptorStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
