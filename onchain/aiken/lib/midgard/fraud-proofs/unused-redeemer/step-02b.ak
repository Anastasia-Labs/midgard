use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_redeemer/rule.{AuthenticatedControlV1}
use midgard/redeemer_item_proof_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  item_control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
}

pub type Datum =
  ct.StepDatum<AuthenticatedControlV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
