use aiken/crypto.{VerificationKeyHash}
use cardano/transaction.{Output}
use midgard/fraud_proof/common/types.{Proof}
use midgard/ledger_state.{MidgardTxBodyCompact, MidgardTxCompact}

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    bad_tx_hash: ByteArray,
    bad_tx_info_hash: ByteArray,
    bad_tx_to_root_proof_path: Proof,
    bad_tx: MidgardTxCompact,
    bad_tx_body: MidgardTxBodyCompact,
    parent_tx_hash: ByteArray,
    parent_tx_info_hash: ByteArray,
    parent_tx_to_root_proof_path: Proof,
    parent_tx: MidgardTxCompact,
    parent_tx_body: MidgardTxBodyCompact,
    bad_spend_inputs: Data,
    bad_input: Data,
    parent_outputs: Data,
    required_signers: List<VerificationKeyHash>,
    index_output: Int,
    computation_thread_redeemer_index: Int,
  }
}
