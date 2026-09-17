//// Datum and redeemer wire shapes for the mint-authorization thread's
//// terminal step: everything substantive was proven upstream, and the
//// closed verdict finalizes into a permanent fraud-proof token.

use midgard/computation_thread as ct

pub type State {
  policy_id: ByteArray,
  direction: Int,
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
