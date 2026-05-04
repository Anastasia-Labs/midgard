use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxBodyCompact, MidgardTxCompact, MidgardTxId}

pub type State {
  bad_input_tx_id: MidgardTxId,
  bad_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  producing_tx: MidgardTxCompact,
  producing_tx_body: MidgardTxBodyCompact,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
