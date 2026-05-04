use aiken/builtin
use aiken/collection/dict.{Dict}
use cardano/assets.{AssetName, Lovelace, PolicyId, Value, ada_policy_id}
use cardano/transaction.{OutputReference}
use midgard/common/types.{ValuePairs}

pub type AssetsToAdd {
  AddLovelace { m_reserve_change_output_index: Option<Int> }
  AddCardanoAssets { assets: ValuePairs, reserve_change_output_index: Int }
  AddMidgardAssets {
    policy_id_to_mint: PolicyId,
    assets_to_mint: Pairs<AssetName, Int>,
    quantity_to_mint: Int,
    registry_ref_input_index: Int,
    policy_id_index_in_target: Int,
    asset_name_index_in_target: Int,
  }
}

pub type WithdrawRedeemer {
  payout_input_outref: OutputReference,
  payout_input_index: Int,
  payout_output_index: Int,
  payout_nft_asset_name: AssetName,
  assets_to_add: AssetsToAdd,
  hub_oracle_ref_input_index: Int,
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
