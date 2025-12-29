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
    script_wit_to_delete_key: ScriptHash,
    script_wit_to_delete_value: MidgardScript,
    script_wits_delete_proof: mpf.Proof,
    script_wits_new_root: MerkleRoot<ScriptHash, MidgardScript>,
    index_output: Int,
    computation_thread_redeemer_index: Int,
  }
}
