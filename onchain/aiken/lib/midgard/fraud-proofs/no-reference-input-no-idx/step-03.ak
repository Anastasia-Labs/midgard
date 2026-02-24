use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputId,
  MidgardTxBodyCompact, MidgardTxCompact}

pub type State {
  verified_no_reference_input_key: MidgardInputId,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  previous_tx: MidgardTxCompact,
  previous_tx_body: MidgardTxBodyCompact,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
