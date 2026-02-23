use cardano/assets.{PolicyId}
use cardano/transaction.{Mint, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof.{MintRedeemer}

/// The spending validator of a fraud proof token never allows it to be spent.
/// Midgard fraud proof tokens last forever.
validator spend {
  else(_) {
    trace @"Midgard - Fraud Proof Spending Validator"
    fail
  }
}

/// Midgard fraud proof tokens last forever.
validator mint(computation_thread_script_hash: ByteArray) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let Transaction { mint, redeemers, .. } = self

    let MintRedeemer {
      computation_thread_token_asset_name,
      computation_thread_mint_redeemer_index,
    } = redeemer

    // 1. `Success` redeemer of the computation thread's token policy must be
    //    invoked.
    let mint_redeemer_data =
      utils.get_redeemer_at(
        redeemers: redeemers,
        expected_purpose: Mint(computation_thread_script_hash),
        redeemer_index: computation_thread_mint_redeemer_index,
      )
    expect ct.Success {
      burning_token_asset_name: success_redeemer_burning_asset_name,
    } = mint_redeemer_data

    // 2. Asset name from the computation thread's redeemer must match the asset
    //    name being minted here.
    expect
      success_redeemer_burning_asset_name == computation_thread_token_asset_name

    // 3. Only the computation thread token must be burned, and fraud proof
    //    token with the same asset name must be minted.
    let expected_mint =
      assets.merge(
        assets.from_asset(
          computation_thread_script_hash,
          computation_thread_token_asset_name,
          -1,
        ),
        assets.from_asset(policy_id, computation_thread_token_asset_name, 1),
      )
    expect mint == expected_mint

    // Done.
    True
  }

  else(_) {
    fail
  }
}
