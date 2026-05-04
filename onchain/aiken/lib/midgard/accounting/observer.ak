pub type AssetsToAdd {
  AddLovelace {
    m_reserve_change_output_index: Option<Int>,
  }
  AddCardanoAssets {
    assets: ValuePairs,
    reserve_change_output_index: Int,
  }
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
  payout_input_index: Int,
  payout_output_index: Int,
  hub_oracle_ref_input_index: Int,
}
