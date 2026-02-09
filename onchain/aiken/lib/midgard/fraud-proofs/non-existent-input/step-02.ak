use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputId}

pub type State {
  bad_tx_inputs_root: MerkleRoot<Int, OutputReference>,
  blocks_prev_utxos_root: MerkleRoot<Int, OutputReference>,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  non_existent_input_key: MidgardInputId,
  non_existent_input_value: Data,
  membership_proof_in_bad_tx: Proof,
  membership_proof_script_redeemer_index: Int,
  non_membership_proof_in_ledger: Proof,
  non_membership_proof_script_redeemer_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
