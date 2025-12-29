use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/common/utils.{plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/payout
use midgard/settlement
use midgard/user_events
use midgard/user_events/withdrawal.{
  Datum, InitializePayout, Refund, SpendRedeemer,
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { reference_inputs, redeemers, .. } = tx

    let SpendRedeemer {
      input_index,
      output_index,
      hub_ref_input_index,
      settlement_ref_input_index,
      burn_redeemer_index,
      payout_mint_redeemer_index,
      membership_proof,
      purpose,
    } = redeemer

    let hub.Datum {
      withdrawal: withdrawal_policy_id,
      settlement: settlement_policy_id,
      payout: payout_policy_id,
      payout_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let settlement.Datum { withdrawals_root, .. } =
      settlement.get_datum(
        reference_inputs,
        settlement_policy_id,
        settlement_ref_input_index,
      )

    expect Some(withdrawal_datum) = datum
    let user_events.OptimisticDatum { event: withdrawal_event, .. } =
      withdrawal_datum

    // 1. Spending a user event UTxO must also be accompanied by the burning of
    //    its NFT (and consequently deregistration of its witness staking
    //    script).
    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(withdrawal_policy_id),
        burn_redeemer_index,
      )

    singular_utxo_indexer.spend(
      fn(_in_ix, input, spend_purpose, _out_ix, output) {
        let Input { output: Output { value: own_value, .. }, .. } = input

        // 2. The produced UTxO must not have any scripts attached to it.
        expect Output {
          address: output_address,
          value: output_value,
          datum: output_datum,
          reference_script: None,
        } = output

        when spend_purpose is {
          InitializePayout -> {
            // 3. Output value must be the same as the input value, without the
            //    withdrawal NFT, and with the payout NFT added.
            expect
              output_value == (
                own_value
                  |> assets.add(withdrawal_policy_id, burn_asset_name, -1)
                  |> assets.add(payout_policy_id, burn_asset_name, 1)
              )

            // 4. TODO: Anticipating what the payout mint redeemer should look
            //          like.
            expect payout.MintRedeemer { withdrawal_utxo_out_ref }: payout.MintRedeemer =
              utils.get_redeemer_at(
                redeemers,
                Mint(payout_policy_id),
                payout_mint_redeemer_index,
              )
            expect withdrawal_utxo_out_ref == own_out_ref

            let Pair(withdrawal_event_id, withdrawal_event_info) =
              ledger_state.unsafe_event_to_key_value_pair(withdrawal_event)

            // 5. Initiation of a payout accumulator can only happen for valid
            //    withdrawals.
            expect ledger_state.WithdrawalInfo {
              body: withdrawal_body,
              validity: ledger_state.WithdrawalIsValid,
              ..
            } = withdrawal_event_info

            // 6. The referenced settlement UTxO's withdrawal tree must contain
            //    the subject withdrawal event.
            expect
              plutarch_phas(
                withdrawals_root,
                withdrawal_event_id,
                withdrawal_event_info,
                membership_proof,
                redeemers,
              )

            let ledger_state.WithdrawalBody {
              l2_value,
              l1_address,
              l1_datum,
              ..
            } = withdrawal_body

            let expected_output_datum: Data =
              payout.Datum { l2_value, l1_address, l1_datum }

            // 7. Assert payout datum properly carries over all the required
            //    info.
            expect output_datum == InlineDatum(expected_output_datum)

            // 8. Withdrawal must go to the payout contract.
            expect output_address == payout_addr

            // Done.
            True
          }
          Refund { validity_override } -> {
            // 3. Only invalid withdrawals can be refunded.
            //    TODO: worth defining a separate type for this?
            expect validity_override != ledger_state.WithdrawalIsValid

            // 4. Output value must be the same as the input value, without the
            //    withdrawal NFT.
            expect
              output_value == (
                own_value
                  |> assets.add(withdrawal_policy_id, burn_asset_name, -1)
              )

            // 5. For refunding invalid withdrawal orders, they must be included
            //    in the settlement's tree with the same invalidity provided via
            //    the redeemer.
            let Pair(withdrawal_event_id, withdrawal_event_info) =
              ledger_state.unsafe_event_to_key_value_pair(withdrawal_event)
            //
            expect coerced_withdrawal_info: ledger_state.WithdrawalInfo =
              withdrawal_event_info
            //
            let overridden_withdrawal_info: Data =
              ledger_state.WithdrawalInfo {
                ..coerced_withdrawal_info,
                validity: validity_override,
              }
            //
            expect
              plutarch_phas(
                withdrawals_root,
                withdrawal_event_id,
                overridden_withdrawal_info,
                membership_proof,
                redeemers,
              )

            // Done.
            True
          }
        }
      },
      purpose,
      input_index,
      output_index,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(
    redeemer: user_events.MintRedeemer,
    own_policy: PolicyId,
    tx: Transaction,
  ) {
    // 1. Perform numerous validations (refer to `lib/midgard/user-events.ak`)
    //    and grant access to:
    //    - Serialized and hashed nonce based on the output reference of a
    //      specified input UTxO.
    //    - The output `Value` of the UTxO at the withdrawal address for the
    //      minting (not burning) endpoint.
    //    - The `WithdrawalInfo` as `Data`, which we are ignoring here (TODO:
    //      try remodeling the types to avoid inclusion of `WithdrawalValidity`
    //      on L1 datum).
    let
      l1_id,
      output_withdrawal_order_value,
      withdrawal_info_data,
    <-
      user_events.validate_mint(
        hub_oracle,
        fn(hub_datum) {
          let hub.Datum { withdrawal_addr, .. } = hub_datum
          withdrawal_addr
        },
        redeemer,
        own_policy,
        tx,
      )

    let output_withdrawal_order_nft_triplet =
      utils.get_single_asset_from_value_apart_from_ada(
        output_withdrawal_order_value,
      )

    // 2. The output value of the UTxO at the withdrawal address must contain
    //    only ADA and the authentication NFT.
    expect output_withdrawal_order_nft_triplet == (own_policy, l1_id, 1)

    // 3. The withdrawal event must be marked as valid. This is needed since
    //    only valid withdrawals can initialize payout accumulators.
    expect ledger_state.WithdrawalInfo {
      validity: ledger_state.WithdrawalIsValid,
      ..
    } = withdrawal_info_data

    // Done.
    True
  }

  else(_) {
    fail
  }
}
