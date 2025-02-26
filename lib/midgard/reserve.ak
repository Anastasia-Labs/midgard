use midgard/common/types.{TokensOfPolicy}

pub const max_policy_count_per_utxo = 10

pub type Datum = List<TokensOfPolicy>

pub type SpendRedeemer {
  MergeUTxOs
  SatisfyWithdrawal
}
