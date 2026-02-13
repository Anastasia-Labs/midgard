use aiken/crypto.{Signature, VerificationKey, VerificationKeyHash}
use aiken/merkle_patricia_forestry as mpf
use midgard/ledger_state.{
  MidgardTxBodyCompact, MidgardTxCompact, MidgardTxWitnessSetCompact,
}

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    bad_tx_hash: ByteArray,
    bad_tx_info_hash: ByteArray,
    bad_tx_to_root_proof_path: mpf.Proof,
    bad_tx: MidgardTxCompact,
    bad_tx_body: MidgardTxBodyCompact,
    bad_tx_wits_as_data: Data<MidgardTxWitnessSetCompact>,
    bad_addr_tx_wit_as_data: Data<(VerificationKey, Signature)>,
    bad_addr_tx_wit_verification_key_hash: VerificationKeyHash,
    bad_addr_tx_wit_membership_proof: mpf.Proof,
    index_output: Int,
    computation_thread_redeemer_index: Int,
  }
}
