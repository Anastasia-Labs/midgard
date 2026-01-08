use aiken/merkle_patricia_forestry as mpf
use midgard/ledger_state.{MidgardTxBodyCompact, MidgardTxCompact}

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    bad_tx_hash: ByteArray,
    bad_tx_info_hash: ByteArray,
    bad_tx_to_root_proof_path: mpf.Proof,
    bad_tx: MidgardTxCompact,
    bad_tx_body: MidgardTxBodyCompact,
    bad_input_index: Int,
    bad_input_as_data: Data,
    bad_input_membership_proof: mpf.Proof,
    bad_input_utxo_info_hash: ByteArray,
    bad_input_utxo_to_root_proof_path: mpf.Proof,
    bad_input_tx_info_hash: ByteArray,
    bad_input_tx_to_root_proof_path: mpf.Proof,
    index_output: Int,
    computation_thread_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
}
