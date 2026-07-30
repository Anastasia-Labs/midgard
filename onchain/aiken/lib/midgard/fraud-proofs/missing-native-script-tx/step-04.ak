use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput}
use midgard/ledger_state.{MidgardOutputsHash, MidgardScriptWitsHash}

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
  outputs_preimage: List<MidgardTxOutput>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
