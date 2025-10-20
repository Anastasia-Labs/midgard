use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/escape_hatch.{MintRedeemer, SpendRedeemer}

validator mint(
  _hub_oracle_policy_id: PolicyId,
  _hub_oracle_asset_name: ByteArray,
) {
  mint(_redeemer: MintRedeemer, _policy_id: PolicyId, _self: Transaction) {
    True
  }

  else(_) {
    fail
  }
}

validator spend(_escape_hatch_policy_id: PolicyId) {
  spend(
    _datum: Option<Data>,
    _redeemer: SpendRedeemer,
    _own_ref: OutputReference,
    _self: Transaction,
  ) {
    True
  }

  else(_) {
    fail
  }
}
