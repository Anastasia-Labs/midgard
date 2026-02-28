use aiken/collection/list
use cardano/assets.{AssetName, PolicyId, quantity_of}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils

validator mint(init_utxo: OutputReference, hub_oracle_asset_name: AssetName) {
  mint(_redeemer: Data, policy_id: PolicyId, self: Transaction) {
    let qty = quantity_of(self.mint, policy_id, hub_oracle_asset_name)

    expect or {
      qty == 1,
      qty == -1,
    }

    expect
      utils.validate_mint(
        self.mint,
        policy_id,
        hub_oracle_asset_name,
        qty,
      )

    if qty == 1 {
      expect Some(_) =
        list.find(self.inputs, fn(input) { input.output_reference == init_utxo })
      True
    } else {
      True
    }
  }

  else(_) {
    fail
  }
}
