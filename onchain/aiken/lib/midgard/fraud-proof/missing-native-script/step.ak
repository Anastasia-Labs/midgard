use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash}
use cardano/transaction.{Output}
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
    bad_tx_outputs_as_data: Data<List<Output>>,
    bad_tx_wits_as_data: Data<MidgardTxWitnessSetCompact>,
    script_tx_wits_as_data: Data<Dict<ScriptHash, MidgardScript>>,
    bad_output_index: Int,
    index_output: Int,
    computation_thread_redeemer_index: Int,
  }
}
