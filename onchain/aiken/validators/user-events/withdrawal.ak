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
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/withdrawal.{
  AuthenticateWithdrawal, BurnWithdrawalNFT, Datum, MintRedeemer, Refund,
  SpendRedeemer, TransferToPayout,
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
      settlement_queue: settlement_queue_policy_id,
      payout: payout_policy_id,
      payout_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let parsed_settlement_node_datum =
      settlement_queue.get_datum(
        reference_inputs,
        settlement_queue_policy_id,
        settlement_ref_input_index,
      )

    expect settlement_queue.NodeData {
      withdrawals_root,
      start_time: settlement_start_time,
      end_time: settlement_end_time,
      ..
    } = parsed_settlement_node_datum.data

    expect Some(withdrawal_datum) = datum
    let user_events.Datum {
      event: withdrawal_event,
      inclusion_time,
      refund_address,
      refund_datum,
      ..
    } = withdrawal_datum

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
          TransferToPayout { membership_proof } -> {
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

            // 5. The referenced settlement node's withdrawal tree must contain
            //    the subject withdrawal event. No need to check for the
            //    inclusion time of the withdrawal.
            expect
              plutarch_phas(
                withdrawals_root,
                withdrawal_event_id,
                withdrawal_event_info,
                membership_proof,
                redeemers,
              )

            expect ledger_state.WithdrawalInfo { body: withdrawal_body, .. } =
              withdrawal_event_info

            let ledger_state.WithdrawalBody {
              l2_value,
              l1_address,
              l1_datum,
              ..
            } = withdrawal_body

            let expected_output_datum: Data =
              payout.Datum { l2_value, l1_address, l1_datum }

            and {
              // 6. Assert proper datum is attached to the payout accumulator
              //    UTxO.
              output_datum == InlineDatum(expected_output_datum),
              // 7. Withdrawal must go to the payout contract.
              output_address == payout_addr,
            }
          }
          Refund { refund_approach } ->
            // 3. Numerous validations for the refund logic.
            user_events.validate_refund(
              own_value,
              withdrawal_policy_id,
              burn_asset_name,
              output_address,
              output_value,
              output_datum,
              refund_address,
              refund_datum,
              refund_approach,
              withdrawals_root,
              ledger_state.unsafe_event_to_id_data(withdrawal_event),
              settlement_queue_policy_id,
              parsed_settlement_node_datum.link,
              settlement_start_time,
              settlement_end_time,
              inclusion_time,
              reference_inputs,
              redeemers,
            )
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

    // 3. The withdrawal event must be marked as valid. This is needed to make
    //    distinguishing between resolutions and refunds easier when spending
    //    the produced UTxO (as this value will be different for invalid cases).
    expect ledger_state.WithdrawalInfo {
      validity: ledger_state.WithdrawalIsValid,
      ..
    } = withdrawal_info_data

    True
  }

  else(_) {
    fail
  }
}
