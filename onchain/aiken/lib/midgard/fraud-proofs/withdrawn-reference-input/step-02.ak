use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputs, MidgardInputsHash, WithdrawalsRoot}

pub type State {
  bad_tx_reference_inputs_hash: MidgardInputsHash,
  blocks_withdrawals_root: WithdrawalsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  reference_inputs_preimage: MidgardInputs,
  bad_reference_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
