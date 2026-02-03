use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, OutputReference, Redeemer, Transaction,
}
use midgard/fraud_proof_catalogue.{
  Deinit, Init, MintRedeemer, NewFraudCategory, RemoveFraudCategory,
  SpendRedeemer,
}
use midgard/hub_oracle as hub

validator spend {
  else(_) {
    trace @"Midgard - Fraud Proof Catalogue Spending Validator"
    fail
  }
}

validator mint(hub_oracle_script_hash: PolicyId) {
  mint(_redeemer: Redeemer, _policy_id: PolicyId, self: Transaction) {
    assets.quantity_of(self.mint, hub_oracle_script_hash, hub.asset_name) == 1
  }

  else(_) {
    fail
  }
}
