use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}
use midgard/ledger_state.{MidgardTxId}

pub type State {
  bad_input_tx_id: MidgardTxId,
  bad_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args =
  TxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
