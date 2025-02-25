use cardano/assets.{AssetName, PolicyId}

pub const max_token_count = 10

pub type Datum = List<Token>

pub type Token {
  mint_policy_id: PolicyId,
  mint_asset_name: AssetName,
  mint_quantity: Int,
}

pub type SpendRedeemer {
  MergeUTxOs
  SatisfyWithdrawal
}
