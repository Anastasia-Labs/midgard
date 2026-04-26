use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use midgard/common/types.{Proof, ValuePairs}

pub type Datum {
  l2_value: ValuePairs,
}

pub type AccumulationArgs {
  payout_input_index: Int,
  payout_output_index: Int,
  reserve_input_index: Int,
  asset_to_add: AssetToAdd,
  payout_nft_asset_name: AssetName,
}

pub type SpendRedeemer {
  AddFunds { accumulation_args: AccumulationArgs }
  ConcludeWithdrawal {
    payout_input_index: Int,
    l1_output_index: Int,
    hub_ref_input_index: Int,
    settlement_ref_input_index: Int,
    burn_redeemer_index: Int,
    // Original withdrawal UTxO outref. blake2b_256(cbor(withdrawal_out_ref))
    // must equal the payout NFT asset name, linking this payout to its
    // withdrawal trustlessly without needing the (already-spent) withdrawal UTxO.
    withdrawal_out_ref: OutputReference,
    // The WithdrawalInfo stored in the settlement tree for this withdrawal.
    // Validated against the tree by the Merkle proof — cannot be forged.
    withdrawal_event_info: Data,
    membership_proof: Proof,
    inclusion_proof_script_withdraw_redeemer_index: Int,
  }
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

pub type AssetToAdd {
  Lovelace { m_reserve_change_output_index: Option<Int> }
  Asset {
    policy_id: PolicyId,
    asset_name: AssetName,
    policy_id_index_in_target: Int,
    asset_name_index_in_target: Int,
    reserve_change_output_index: Int,
  }
}

pub type AddFundsRedeemer {
  accumulation_args: AccumulationArgs,
  payout_input_outref: OutputReference,
  payout_input_redeemer_index: Int,
}
