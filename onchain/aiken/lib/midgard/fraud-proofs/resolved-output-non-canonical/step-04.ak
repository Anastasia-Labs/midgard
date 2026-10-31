use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/resolved_output_non_canonical/step_03.{
  ReconstructionV1,
}

pub type ActionV1 {
  Advance { chunk_proof: ChunkProofV1, next_chunk_proof: Option<ChunkProofV1> }
  FinalizeCanonical
}

pub type Args {
  input_index: Int,
  output_index: Int,
  action: ActionV1,
}

pub type Datum =
  ct.StepDatum<ReconstructionV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
