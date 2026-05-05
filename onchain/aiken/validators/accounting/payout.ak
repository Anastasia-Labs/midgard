use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/stake_validator
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/accounting/observer
use midgard/accounting/payout.{
  AddFunds, BurnPayout, ConcludeWithdrawal, Datum, MintPayout, MintRedeemer,
  SpendRedeemer,
}
use midgard/common/utils
use midgard/user_events/withdrawal

validator mint(withdrawal_addr: Address) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let Transaction { inputs, outputs, redeemers, mint, .. } = tx

    when redeemer is {
      // Minting: must be coupled with InitializePayout in the withdrawal
      // spending validator, ensuring a payout NFT is only ever created for a
      // confirmed, valid withdrawal.
      MintPayout { withdrawal_input_index, withdrawal_spend_redeemer_index } -> {
        let Transaction { inputs, redeemers, .. } = tx

        // 1. Its spending redeemer must be InitializePayout. The withdrawal
        //    spending validator already validates all other minting conditions
        //    (asset name, output value, destination, etc.).
        expect withdrawal.SpendRedeemer {
          purpose: withdrawal.InitializePayout,
          ..
        } =
          utils.get_spending_redeemer_data_at(
            script_address: withdrawal_addr,
            input_index: withdrawal_input_index,
            redeemer_index: withdrawal_spend_redeemer_index,
            inputs: inputs,
            redeemers: redeemers,
          )

        // 2. Exactly one payout NFT minted with quantity 1.
        //    The withdrawal validator ensures the asset name matches the
        //    burnt withdrawal NFT.
        expect Pair(_, 1) =
          utils.get_singleton_asset_with_policy(mint, own_policy)

        // Done.
        True
      }

      // Burning: must be coupled with ConcludeWithdrawal in the payout
      // spending validator, ensuring the NFT can only be destroyed when
      // the accumulator is fully funded and funds are released to L1.
      BurnPayout {
        payout_asset_name,
        payout_input_index,
        payout_output_index,
        payout_spend_redeemer_index,
      } -> {
        // 1. Exactly one payout NFT must be burned.
        expect utils.validate_mint(mint, own_policy, payout_asset_name, -1)

        // 2. The payout UTxO must be spent with the ConcludeWithdrawal
        //    redeemer with a matching payout input index.
        expect Input {
          output_reference: payout_input_outref,
          output: Output {
            datum: InlineDatum(input_datum_data),
            value: payout_input_value,
            ..
          },
        } = inputs |> list.expect_at(payout_input_index)
        expect ConcludeWithdrawal { input_index: spend_payout_input_index, .. } =
          utils.get_redeemer_at(
            redeemers,
            Spend(payout_input_outref),
            payout_spend_redeemer_index,
          )
        expect spend_payout_input_index == payout_input_index

        // 3. The specified payout input must be authentic. Note that it may
        //    have assets other than ADA and auth NFT.
        expect
          payout_input_value
            |> assets.has_nft(own_policy, payout_asset_name)

        // 4. Produced UTxO from the payout accumulator must correspond to the
        //    initial withdrawal order.
        let Output {
          address: payout_output_address,
          value: payout_output_value,
          datum: payout_output_datum,
          ..
        } = outputs |> list.expect_at(payout_output_index)

        expect Datum { l2_value, l1_address, l1_datum } = input_datum_data

        expect payout_output_address == l1_address
        expect
          payout_output_value |> assets.match(l2_value, fn(l0, l1) { l0 == l1 })
        expect payout_output_datum == l1_datum

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

validator spend(
  observer_withdraw_script_hash: ScriptHash,
  payout_policy_id: PolicyId,
) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      AddFunds { observer_withdraw_redeemer_index } -> {
        let observer_withdraw_redeemer_data <-
          stake_validator.validate_withdraw(
            withdraw_script_hash: observer_withdraw_script_hash,
            redeemers: tx.redeemers,
            withdraw_redeemer_index: observer_withdraw_redeemer_index,
          )

        expect observer.WithdrawRedeemer {
          payout_input_outref: payout_outref_from_observer,
          ..
        } = observer_withdraw_redeemer_data

        expect payout_outref_from_observer == own_out_ref

        // Done.
        True
      }
      ConcludeWithdrawal { input_index, burn_redeemer_index } -> {
        let Transaction { inputs, redeemers, .. } = tx

        // 1. The minting policy of the payout script must be invoked via the
        //    `BurnPayout` redeemer, with a matching payout input index. Let
        //    this index be `input_index`.
        expect BurnPayout { payout_input_index, .. } =
          redeemers
            |> utils.get_redeemer_at(
                Mint(payout_policy_id),
                burn_redeemer_index,
              )
        expect input_index == payout_input_index

        // 2. The input corresponding to `input_index` must have a matching
        //    output reference from the script info.
        let Input { output_reference, .. } =
          inputs |> list.expect_at(input_index)
        expect output_reference == own_out_ref

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
