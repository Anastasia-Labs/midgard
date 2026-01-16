use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputId}

pub type State {
  verified_tx2_inputs_root: MerkleRoot,
  double_spent_input_key: MidgardInputId,
  double_spent_input_value: Data,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  input_membership_proof: Proof,
  fraud_proof_mint_redeemer_index: Int,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
