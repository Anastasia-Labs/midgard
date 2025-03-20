pub type SpendRedeemer {
  MergeUTxOs { withdrawal_index: Int }
  FundWithdrawal {
    hub_ref_input_index: Int,
    withdrawal_event_ref_input_index: Int,
  }
}

pub type MergeRedeemer {
  hub_ref_input_index: Int,
  output_index: Int,
}
