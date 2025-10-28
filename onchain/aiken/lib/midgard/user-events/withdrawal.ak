use aiken/merkle_patricia_forestry as mpf
use midgard/ledger_state
use midgard/user_events

pub type Datum =
  user_events.Datum<ledger_state.WithdrawalEvent>

pub type SpendPurpose {
  InitializePayout
  Refund
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  payout_mint_redeemer_index: Int,
  membership_proof: mpf.Proof,
  purpose: SpendPurpose,
}
