use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, OutputReference, Redeemer, Transaction,
}
use midgard/common/utils.{get_singleton_asset_with_policy}
use midgard/fraud_proof_catalogue.{asset_name}
use midgard/hub_oracle as hub

validator spend {
  else(_) {
    trace @"Midgard - Fraud Proof Catalogue Spending Validator"
    fail
  }
}

validator mint(hub_oracle_script_hash: PolicyId) {
  mint(_redeemer: Redeemer, own_policy_id: PolicyId, self: Transaction) {
    let Transaction { mint, .. } = self
    and {
      assets.quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == 1,
      Pair(asset_name, 1) == get_singleton_asset_with_policy(
        mint,
        own_policy_id,
      ),
    }
  }

  else(_) {
    fail
  }
}
