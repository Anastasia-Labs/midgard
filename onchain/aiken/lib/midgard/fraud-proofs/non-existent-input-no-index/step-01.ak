use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxBodyCompact, MidgardTxCompact, MidgardTxId}

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  bad_state_queue_node_ref_input_index: Int,
  bad_tx_id: MidgardTxId,
  bad_tx: MidgardTxCompact,
  bad_tx_body: MidgardTxBodyCompact,
  bad_tx_membership_proof: Proof,
  fraud_proof_mint_redeemer_index: Int,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  bad_state_queue_node_ref_input_index: Int,
  bad_tx_hash: ByteArray,
  bad_tx_info_hash: ByteArray,
  bad_tx_to_root_proof_path: Proof,
  bad_tx: MidgardTxCompact,
  bad_tx_body: MidgardTxBodyCompact,
  bad_spend_inputs: Data,
  bad_input: Data,
  bad_input_tx_info_hash: ByteArray,
  bad_input_tx_to_root_proof_path: Proof,
  index_output: Int,
  computation_thread_redeemer_index: Int,
  previous_tx: MidgardTxCompact,
  previous_tx_body: MidgardTxBodyCompact,
  real_outputs: Data,
  hub_ref_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
