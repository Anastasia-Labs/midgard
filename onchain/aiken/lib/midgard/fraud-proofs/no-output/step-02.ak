use cardano/transaction.{OutputReference}
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
  no_input_key: MidgardInputId,
  no_input_value: Data,
  no_input_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
