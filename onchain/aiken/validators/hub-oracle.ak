use aiken/collection/dict
use aiken/collection/list
use cardano/assets.{AssetName, PolicyId, quantity_of}
use cardano/transaction.{OutputReference, Transaction}
use midgard/claim_registry_v1 as claim_registry
use midgard/correction_lock

pub fn hub_mint_set_is_exact(
  mint: assets.Value,
  policy_id: PolicyId,
  hub_oracle_asset_name: AssetName,
) -> Bool {
  let qty = quantity_of(mint, policy_id, hub_oracle_asset_name)
  dict.to_pairs(assets.tokens(mint, policy_id)) == [
    Pair(claim_registry.asset_name, qty),
    Pair(correction_lock.asset_name, qty),
    Pair(hub_oracle_asset_name, qty),
  ]
}

validator mint(init_utxo: OutputReference, hub_oracle_asset_name: AssetName) {
  mint(_redeemer: Data, policy_id: PolicyId, self: Transaction) {
    let qty = quantity_of(self.mint, policy_id, hub_oracle_asset_name)
    expect hub_mint_set_is_exact(self.mint, policy_id, hub_oracle_asset_name)

    if qty == 1 {
      expect Some(_) = list.find(
        self.inputs,
        fn(input) { input.output_reference == init_utxo },
      )
      True
    } else {
      expect qty == -1
      True
    }
  }

  else(_) {
    fail
  }
}

const test_policy: ByteArray =
  #"11111111111111111111111111111111111111111111111111111111"

const test_hub_asset_name: ByteArray = "MIDGARD_HUB_ORACLE"

test hub_policy_accepts_equal_claim_registry_correction_lock_and_hub_quantities() {
  and {
    hub_mint_set_is_exact(
      assets.from_asset(test_policy, claim_registry.asset_name, 1)
        |> assets.merge(
            assets.from_asset(test_policy, correction_lock.asset_name, 1),
          )
        |> assets.merge(assets.from_asset(test_policy, test_hub_asset_name, 1)),
      test_policy,
      test_hub_asset_name,
    ),
    hub_mint_set_is_exact(
      assets.from_asset(test_policy, claim_registry.asset_name, -1)
        |> assets.merge(
            assets.from_asset(test_policy, correction_lock.asset_name, -1),
          )
        |> assets.merge(assets.from_asset(test_policy, test_hub_asset_name, -1)),
      test_policy,
      test_hub_asset_name,
    ),
  }
}

test hub_policy_rejects_unequal_correction_lock_quantity() {
  !hub_mint_set_is_exact(
    assets.from_asset(test_policy, claim_registry.asset_name, 1)
      |> assets.merge(
          assets.from_asset(test_policy, correction_lock.asset_name, 2),
        )
      |> assets.merge(assets.from_asset(test_policy, test_hub_asset_name, 1)),
    test_policy,
    test_hub_asset_name,
  )
}

test hub_policy_rejects_unequal_claim_registry_quantity() {
  !hub_mint_set_is_exact(
    assets.from_asset(test_policy, claim_registry.asset_name, 2)
      |> assets.merge(
          assets.from_asset(test_policy, correction_lock.asset_name, 1),
        )
      |> assets.merge(assets.from_asset(test_policy, test_hub_asset_name, 1)),
    test_policy,
    test_hub_asset_name,
  )
}

test hub_policy_rejects_extra_asset_name() {
  !hub_mint_set_is_exact(
    assets.from_asset(test_policy, claim_registry.asset_name, 1)
      |> assets.merge(
          assets.from_asset(test_policy, correction_lock.asset_name, 1),
        )
      |> assets.merge(assets.from_asset(test_policy, test_hub_asset_name, 1))
      |> assets.merge(assets.from_asset(test_policy, "EXTRA", 1)),
    test_policy,
    test_hub_asset_name,
  )
}
