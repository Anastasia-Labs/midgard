use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/execution_source_script_decoding/rule.{
  AuthenticatedExecutionSourceV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  first_chunk: ChunkProofV1,
}

pub type Datum =
  ct.StepDatum<AuthenticatedExecutionSourceV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
