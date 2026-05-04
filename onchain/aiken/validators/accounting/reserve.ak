use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{Datum, OutputReference, Transaction}
use midgard/payout

validator spend(payout_policy_id: PolicyId, payout_addr: Address) {
  spend(
    _datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, redeemers, .. } = tx

    let SpendRedeemer { accumulation_args, payout_redeemer_index } = redeemer

    let payout.AccumulationArgs {
      payout_input_index,
      assets_to_add,
      payout_nft_asset_name,
      ..
    } = accumulation_args

    // 1. Specified payout input must be authentic.
    let Input {
      output_reference: payout_outref,
      output: Output { address: payout_input_address, .. },
    } =
      utils.get_authentic_input_with_nft_at(
        inputs,
        payout_policy_id,
        payout_nft_asset_name,
        payout_input_index,
      )

    // 2. Specified payout input must be getting spend for fund accumulation
    //    with a matching set of arguments.
    expect Pair(Spend(payout_redeemer_outref), payout_spend_redeemer_data) =
      redeemers |> list.expect_at(payout_redeemer_index)
    expect payout_redeemer_outref == payout_outref
    expect payout.AddFunds { accumulation_args: payout_acc_args, .. } =
      payout_spend_redeemer_data
    let payout_acc_args_data: Data = payout_acc_args
    let own_acc_args_data: Data = accumulation_args
    expect payout_acc_args_data = own_acc_args_data

    when assets_to_add is {
      payout.AddLovelace {
        reserve_addr,
        reserve_input_index,
        m_reserve_change_output_index,
        ..
      } |
      payout.AddCardanoAssets {
        reserve_addr,
        reserve_input_index,
        m_reserve_change_output_index,
        ..
      } -> {
        let Input {
          output_reference,
          output: Output { address: own_address, .. },
        } = tx.inputs |> list.expect_at(reserve_input_index)

        // 3a. The input index from both redeemers must correspond to the UTxO
        //     being validated.
        expect output_reference == own_out_ref

        // 4a. The address specified in the payout redeemer must match this
        //     spending script's (TODO: this might not be needed).
        expect own_address == reserve_addr

        // Done.
        True
      }
      payout.AddMidgardAssets { .. } ->
        fail @"Specified token must be minted and not withdrawn from Midgard reserve"
    }
  }

  else(_) {
    fail
  }
}
