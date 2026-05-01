use cardano/address.{Address}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use midgard/common/types.{Proof, ValuePairs}

pub type Datum {
  l2_value: ValuePairs,
  l1_address: Address,
  l1_datum: CardanoDatum,
}

pub type AssetsToAdd {
  Lovelace {
    reserve_addr: Address,
    reserve_input_index: Int,
    reserve_redeemer_index: Int,
    m_reserve_change_output_index: Option<Int>,
  }
  CardanoAssets {
    assets: ValuePairs,
    reserve_addr: Address,
    reserve_input_index: Int,
    reserve_redeemer_index: Int,
    reserve_change_output_index: Int,
  }
  MidgardAssets {
    policy_id_to_mint: PolicyId,
    assets_to_mint: Pairs<AssetName, Int>,
    quantity_to_mint: Int,
    registry_ref_input_index: Int,
    policy_id_index_in_target: Int,
    asset_name_index_in_target: Int,
  }
}

pub type AccumulationArgs {
  payout_input_index: Int,
  payout_output_index: Int,
  assets_to_add: AssetsToAdd,
  payout_nft_asset_name: AssetName,
}

pub type SpendRedeemer {
  AddFunds {
    accumulation_args: AccumulationArgs,
    payout_input_outref: OutputReference,
    payout_input_redeemer_index: Int,
  }
  ConcludeWithdrawal { input_index: Int, burn_redeemer_index: Int }
}

pub type MintRedeemer {
  MintPayout {
    withdrawal_input_index: Int,
    // Index in redeemers of Spend(withdrawal_utxo_out_ref); its redeemer must
    // decode as InitializePayout, coupling minting to withdrawal spending.
    withdrawal_spend_redeemer_index: Int,
  }
  BurnPayout {
    payout_asset_name: AssetName,
    // Index in inputs of the payout UTxO being spent.
    payout_input_index: Int,
    // Index in redeemers of Spend(payout_outref); its redeemer must decode as
    // ConcludeWithdrawal, ensuring burn only happens for fully funded accumulators.
    payout_spend_redeemer_index: Int,
  }
}

/// Given a value `v`, a policy ID and asset name of a target authentication
/// NFT, this function expects the NFT to be present in `v` with a quantity of
/// 1, removes it, and also pops ADA from the value to return it separately.
pub fn separate_ada_and_remove_auth_nft(
  v: Value,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
) -> (Lovelace, Dict<PolicyId, Dict<AssetName, Int>>) {
  let (v_lovelace_dict, v_assets) =
    v |> assets.to_dict |> dict.expect_pop(ada_policy_id)
  let (nft_assets, v_assets_without_nft) =
    v_assets |> dict.expect_pop(nft_policy_id)
  expect [nft_name_qty_pair] = nft_assets |> dict.to_pairs
  expect nft_name_qty_pair == Pair(nft_asset_name, 1)
  (builtin.head_list(dict.to_pairs(v_lovelace_dict)).2nd, v_assets_without_nft)
}
