use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_redeemer/rule.{AuthenticatedItemHeaderV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
}

pub type Datum =
  ct.StepDatum<AuthenticatedItemHeaderV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
