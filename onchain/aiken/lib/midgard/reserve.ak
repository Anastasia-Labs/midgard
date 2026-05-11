pub type SpendRedeemer {
  Spend {
    reserve_input_index: Int,
    payout_input_index: Int,
    payout_spend_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
}
