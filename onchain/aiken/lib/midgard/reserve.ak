pub type SpendRedeemer {
  MergeUTxOs { withdrawal_index: Int }
  FundWithdrawal
}

pub type MergeRedeemer {
  hub_ref_input_index: Int,
  output_index: Int,
}
