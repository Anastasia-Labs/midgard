use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/validation_merkle_v1.{FrontierPeak}

pub const staged_signer_resume_batch_limit: Int = 16

pub const staged_signer_finalize_batch_limit: Int = 16

pub type State {
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  script_item_hash: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  signer_checkpoint_hash: ByteArray,
  previous_signer_hash: ByteArray,
  signer_count: Int,
  signer_peaks: List<FrontierPeak>,
}

pub type Args {
  ResumeSignerScan {
    input_index: Int,
    output_index: Int,
    addr_tx_wits_opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  FinalizeSignerScan {
    input_index: Int,
    output_index: Int,
    addr_tx_wits_opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
