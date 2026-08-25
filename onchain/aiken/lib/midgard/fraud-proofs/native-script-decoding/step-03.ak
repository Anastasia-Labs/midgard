//// Datum and redeemer wire shapes for the decoding-fault thread's third
//// step: bind the accused outpoint's resolved descriptor, then fold the
//// frozen scan primitives under a prover-chosen budget (self-loop), then
//// consume the verdict (#635, #633).

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

pub type Args {
  /// One-shot: open the accused field at the accused ordinal through the §8.8
  /// door, authenticate the resolved output's descriptor under the pre-state
  /// ledger root, freeze the scan anchor, and either initialise the machine
  /// (self-loop) or close at the bind step (wrapper malformed for direction
  /// A; descriptor contradiction for direction B).
  BindOutpoint {
    input_index: Int,
    output_index: Int,
    subject_field_opening: FieldOpeningV1,
    descriptor_cbor: ByteArray,
    ledger_membership_proof: mpf.Proof,
    /// Required (chunk 0) whenever the descriptor names a tag-0 script.
    first_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
  /// Self-loop: advance the machine by up to `step_budget` primitive steps
  /// over a once-authenticated chunk window. Chunk proofs may be omitted for
  /// frame/finalize-only stretches.
  Scan {
    input_index: Int,
    output_index: Int,
    control_cbor: ByteArray,
    chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
    frames: List<NativeScriptFrameV1>,
    step_budget: Int,
  }
  /// Consume the machine state: direction A exhibits exactly one refusing
  /// primitive step; direction B exhibits the exact canonical terminal.
  Verdict {
    input_index: Int,
    output_index: Int,
    control_cbor: ByteArray,
    chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
