use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/receive.{OutputStateV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  outputs_opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  target_ordinal: Option<Int>,
}

pub type Datum =
  ct.StepDatum<OutputStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
