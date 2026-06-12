use midgard/computation_thread as ct
use midgard/ledger_state.{
  MidgardInputs, MidgardInputsHash, MidgardScriptWitsHash,
}

pub type State {
  bad_tx_inputs_hash: MidgardInputsHash,
  bad_tx_script_wits_hash: MidgardScriptWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  bad_input_index: Int,
  inputs_preimage: MidgardInputs,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
