//// Q27 min-Ada terminal: all authenticated semantic phases have completed.

use midgard/computation_thread as ct

pub type State {
  PredicateAuthenticated
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
