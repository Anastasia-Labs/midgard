use cardano/assets.{PolicyId, quantity_of}
use cardano/script_context.{ScriptContext, Spending}
use cardano/transaction.{Redeemer, Transaction, placeholder}
use midgard/common/utils.{get_singleton_asset_with_policy}
use midgard/fraud_proof_catalogue.{asset_name}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
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
      quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == 1,
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

// ## Q01 catalogue-immutability selectors

fn run_catalogue_mint(minted: assets.Value) -> Bool {
  let unit_redeemer: Data = 0
  mint.mint(
    fixture.hub_oracle_script_hash,
    unit_redeemer,
    fixture.catalogue_script_hash,
    Transaction { ..placeholder, mint: minted },
  )
}

/// Genesis positive: the catalogue NFT can only come into existence coupled
/// one-to-one with the hub oracle NFT.
test catalogue_mint_accepts_genesis_coupled_with_hub_oracle() {
  run_catalogue_mint(
    assets.from_asset(fixture.hub_oracle_script_hash, hub.asset_name, 1)
      |> assets.add(fixture.catalogue_script_hash, asset_name, 1),
  )
}

/// Immutability negative: a standalone (post-genesis) catalogue re-mint
/// without the hub oracle NFT rejects, so the deployed catalogue reference
/// cannot be substituted by re-minting its authenticating NFT.
test catalogue_mint_rejects_standalone_catalogue_mint() fail {
  run_catalogue_mint(
    assets.from_asset(fixture.catalogue_script_hash, asset_name, 1),
  )
}

/// Immutability negative: minting more than one catalogue NFT rejects.
test catalogue_mint_rejects_duplicate_catalogue_token() fail {
  run_catalogue_mint(
    assets.from_asset(fixture.hub_oracle_script_hash, hub.asset_name, 1)
      |> assets.add(fixture.catalogue_script_hash, asset_name, 2),
  )
}

/// Immutability: the deployed catalogue UTxO is unspendable (always-fail
/// spending validator), so its Merkle root can never be mutated mid-thread.
test catalogue_spend_always_rejects() fail {
  spend.else(
    ScriptContext {
      transaction: placeholder,
      redeemer: {
        let unit_redeemer: Data = 0
        unit_redeemer
      },
      info: Spending { output: fixture.own_out_ref(), datum: None },
    },
  )
}
