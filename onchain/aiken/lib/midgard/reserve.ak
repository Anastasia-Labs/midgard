pub type SpendRedeemer {
  MergeUTxOs
  SatisfyWithdrawal
}

pub type MergeRedeemer {
  hub_ref_input_index: Int,
  output_index: Int,
}
