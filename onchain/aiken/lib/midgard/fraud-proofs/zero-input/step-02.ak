use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputsHash}

pub type State {
  bad_tx_spend_inputs_hash: MidgardInputsHash,
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
