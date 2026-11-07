//// Datum and redeemer wire shapes for the decoding-fault thread's split
//// third step (#635, #633): open the accused subject, bind its resolved
//// descriptor, then advance or close the frozen scan machine.

use aiken/merkle_patricia_forestry as mpf
use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/native_script_decoding/engine.{ScanThreadStateV1}
use midgard/native_script_scan_v1.{NativeScriptFrameV1}

pub type State =
  ScanThreadStateV1

pub type Datum =
  ct.StepDatum<State>

/// `step_03_open_subject`: authenticate the accused field and either open the
/// exact in-domain outpoint or close an out-of-domain direction-B claim.
pub type OpenSubjectArgs {
  input_index: Int,
  output_index: Int,
  /// Required exactly when the source kind names a field and the ordinal is
  /// non-negative. The authenticated count then distinguishes in-domain from
  /// out-of-domain without a second redeemer arm.
  subject_field_opening: Option<FieldOpeningV1>,
}

/// `step_03_bind_descriptor`: authenticate the ledger's value for the opened
/// outpoint, freeze the reference-script anchor, and initialise or close the
/// scan machine.
pub type BindDescriptorArgs {
  input_index: Int,
  output_index: Int,
  /// Canonical outpoint bytes whose hash was committed by `OpenSubject`.
  outpoint_key_cbor: ByteArray,
  descriptor_cbor: ByteArray,
  ledger_membership_proof: mpf.Proof,
  /// Required (chunk 0) whenever the descriptor names a tag-0 script.
  first_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
}

/// `step_03_advance_or_close`: advance the machine under a bounded fold,
/// self-loop when more work remains, or close on a direction-A refusal / the
/// direction-B exact terminal.
pub type AdvanceOrCloseArgs {
  input_index: Int,
  output_index: Int,
  control_cbor: ByteArray,
  chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  frames: List<NativeScriptFrameV1>,
  step_budget: Int,
}

pub type OpenSubjectSpendRedeemer =
  ct.StepRedeemer<OpenSubjectArgs>

pub type BindDescriptorSpendRedeemer =
  ct.StepRedeemer<BindDescriptorArgs>

pub type AdvanceOrCloseSpendRedeemer =
  ct.StepRedeemer<AdvanceOrCloseArgs>
