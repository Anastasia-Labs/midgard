use cardano/address.{Address}
use cardano/assets.{AssetName}
use cardano/transaction.{Datum as CardanoDatum, OutputReference}
use midgard/common/types.{ValuePairs}

pub type Datum {
  l2_value: ValuePairs,
  l1_address: Address,
  l1_datum: CardanoDatum,
}

pub type SpendRedeemer {
  AddFunds {
    payout_input_index: Int,
    payout_output_index: Int,
    reserve_input_index: Int,
    reserve_change_output_index: Option<Int>,
    reserve_spend_redeemer_index: Int,
    payout_spend_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
  ConcludeWithdrawal {
    payout_input_index: Int,
    l1_output_index: Int,
    burn_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
}

pub type MintRedeemer {
  MintPayout {
    withdrawal_utxo_out_ref: OutputReference,
    withdrawal_input_index: Int,
    withdrawal_spend_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
  BurnPayout {
    payout_input_index: Int,
    payout_asset_name: AssetName,
    payout_spend_redeemer_index: Int,
    hub_ref_input_index: Int,
  }
}
