use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct

pub type State {
  verified_tx2_inputs_root: MerkleRoot,
  double_spent_input_key: Data,
  double_spent_input_value: Data,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  input_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
