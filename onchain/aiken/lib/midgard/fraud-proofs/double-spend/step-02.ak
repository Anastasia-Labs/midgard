use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputId}

pub type State {
  verified_tx1_inputs_root: MerkleRoot,
  verified_tx2_inputs_root: MerkleRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  double_spent_input_key: MidgardInputId,
  double_spent_input_value: Data,
  double_spent_input_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
