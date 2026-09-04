use midgard/computation_thread as ct
use midgard/native_tx_script_pushdown_v1.{NativeScriptFrameV1}
use midgard/validation_machine/machine_types.{SignerSetProofV1}
use midgard/validation_merkle_v1.{FrontierPeak}

pub const staged_node_batch_limit: Int = 16

pub type PhaseV1 {
  ScriptReady
  ScriptWalk { cursor_hash: ByteArray }
}

pub type State {
  bad_tx_id: ByteArray,
  script_item_hash: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  signer_count: Int,
  signer_peaks: List<FrontierPeak>,
  phase: PhaseV1,
}

pub type SignerQueryV1 {
  signer_hash: ByteArray,
  proof: SignerSetProofV1,
}

pub type Args {
  StartScriptScan {
    input_index: Int,
    output_index: Int,
    script_item_cbor: ByteArray,
    node_budget: Int,
    signer_queries: List<SignerQueryV1>,
  }
  ResumeScriptScan {
    input_index: Int,
    output_index: Int,
    script_item_cbor: ByteArray,
    cursor_bytes: ByteArray,
    frames: List<NativeScriptFrameV1>,
    node_budget: Int,
    signer_queries: List<SignerQueryV1>,
  }
  StartScriptFinalize {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    script_item_cbor: ByteArray,
    node_budget: Int,
    signer_queries: List<SignerQueryV1>,
  }
  FinalizeScriptScan {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    script_item_cbor: ByteArray,
    cursor_bytes: ByteArray,
    frames: List<NativeScriptFrameV1>,
    node_budget: Int,
    signer_queries: List<SignerQueryV1>,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
