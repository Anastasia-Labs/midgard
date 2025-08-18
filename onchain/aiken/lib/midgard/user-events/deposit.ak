use aiken/merkle_patricia_forestry as mpf
use midgard/ledger_state
use midgard/user_events

pub type Datum =
  user_events.Datum<ledger_state.DepositEvent>

pub type SpendPurpose {
  TransferToReserve { membership_proof: mpf.Proof }
  Refund { refund_approach: user_events.RefundApproach }
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  mint_redeemer_index: Int,
  purpose: SpendPurpose,
}
