use midgard/computation_thread as ct
use midgard/ledger_state.{
  MidgardOutputs, MidgardOutputsHash, MidgardScriptWitsHash,
}

pub type State {
  producing_tx_outputs_hash: MidgardOutputsHash,
  bad_input_output_index: Int,
  verified_script_wits_hash: MidgardScriptWitsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  outputs_preimage: MidgardOutputs,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
