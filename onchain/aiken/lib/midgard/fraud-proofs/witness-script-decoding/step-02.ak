use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/witness_script_decoding/rule.{BoundWitnessScriptV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<BoundWitnessScriptV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
