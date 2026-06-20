use cardano/assets.{AssetName}

pub const hub_oracle_spend_asset_name: AssetName = "ReserveSpend"

pub const hub_oracle_observer_asset_name: AssetName = "ReserveObserver"

pub type SpendRedeemer {
  Spend {
    reserve_input_index: Int,
    payout_input_index: Int,
    payout_spend_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
}
