use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  ReferenceScriptScanStateV1,
}
use midgard/native_script_scan_v1.{NativeScriptFrameV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  control_cbor: ByteArray,
  chunk_proof: Option<ChunkProofV1>,
  next_chunk_proof: Option<ChunkProofV1>,
  frames: List<NativeScriptFrameV1>,
  step_budget: Int,
}

pub type Datum =
  ct.StepDatum<ReferenceScriptScanStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
