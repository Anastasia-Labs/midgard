use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_state.{MidgardInputsHash, MidgardScriptWitsHash}

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
  inputs_preimage: List<MidgardTxInput>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
