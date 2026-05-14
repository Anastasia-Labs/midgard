use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputs, MidgardInputsHash}

pub type State {
  verified_tx_inputs_hash: MidgardInputsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  inputs_preimage: MidgardInputs,
  bad_inputs_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
