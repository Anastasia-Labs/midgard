use midgard/common/types.{Proof}
use midgard/ledger_state.{WithdrawalEvent, WithdrawalValidity}
use midgard/user_events

pub type Datum =
  user_events.OptimisticDatum<WithdrawalEvent>

pub type SpendPurpose {
  InitializePayout
  Refund { validity_override: WithdrawalValidity }
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  payout_mint_redeemer_index: Int,
  membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  purpose: SpendPurpose,
}
