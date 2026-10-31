use midgard/computation_thread as ct
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  OutputDescriptorStateV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  window: ByteArray,
}

pub type Datum =
  ct.StepDatum<OutputDescriptorStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
