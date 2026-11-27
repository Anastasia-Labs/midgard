use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/canonical_plutus_data_stream_v1.{StreamFrameV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{
  DatumVerdictStateV1,
}

pub type Datum =
  ct.StepDatum<DatumVerdictStateV1>

/// `chunk_proof` and `next_chunk_proof` carry the output chunks the step
/// reads; `revealed` and `tail_root` open the stored container stack;
/// `node_budget` bounds the nodes read in this transaction.
pub type Args {
  input_index: Int,
  output_index: Int,
  chunk_proof: ChunkProofV1,
  next_chunk_proof: Option<ChunkProofV1>,
  revealed: List<StreamFrameV1>,
  tail_root: ByteArray,
  node_budget: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
