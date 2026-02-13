use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputId}

pub type State {
  verified_tx_inputs_root: MerkleRoot<Int, OutputReference>,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  non_existent_input_key: MidgardInputId,
  non_existent_input_value: Data,
  non_existent_input_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
