use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub const direct_signer_limit: Int = 28

pub const direct_script_bytes_limit: Int = 1024

pub const staged_signer_start_batch_limit: Int = 16

pub type State {
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  script_item_hash: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
}

pub type Args {
  DirectFinalize {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    script_item_cbor: ByteArray,
    addr_tx_wits_opening: FieldOpeningV1,
  }
  StartSignerScan {
    input_index: Int,
    output_index: Int,
    script_item_cbor: ByteArray,
    addr_tx_wits_opening: FieldOpeningV1,
    item_budget: Int,
  }
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
