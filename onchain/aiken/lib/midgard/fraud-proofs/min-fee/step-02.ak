use cardano/assets.{Lovelace}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxCompact}

pub type State {
  bad_tx: MidgardTxCompact,
  bad_tx_body_fee: Lovelace,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
