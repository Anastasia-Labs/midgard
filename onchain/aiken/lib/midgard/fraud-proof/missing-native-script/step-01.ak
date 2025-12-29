use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{Output}
use midgard/common/types.{MerkleRoot}
use midgard/fraud_proof/common/types.{Proof} as midgard_fraud_proof_common_types
use midgard/ledger_state.{
  MidgardScript, MidgardTxBodyCompact, MidgardTxCompact,
  MidgardTxWitnessSetCompact,
}

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    bad_state_queue_node_ref_input_index: Int,
    bad_tx_hash: ByteArray,
    bad_tx_info_hash: ByteArray,
    bad_tx_to_root_proof_path: Proof,
    bad_tx: MidgardTxCompact,
    bad_tx_body: MidgardTxBodyCompact,
    bad_tx_wits: MidgardTxWitnessSetCompact,
    bad_output_index: Int,
    bad_output_as_data: Data<Output>,
    bad_output_membership_proof: mpf.Proof,
    script_wit_to_delete_key: ScriptHash,
    script_wit_to_delete_value: MidgardScript,
    script_wits_delete_proof: mpf.Proof,
    script_wits_new_root: MerkleRoot<ScriptHash, MidgardScript>,
    index_output: Int,
    computation_thread_redeemer_index: Int,
  }
}

pub type StepState {
  bad_output_script_hash: ByteArray,
  current_root: MerkleRoot<ScriptHash, MidgardScript>,
}
