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
    payout_input_outref: OutputReference,
    observer_withdraw_redeemer_index: Int,
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
