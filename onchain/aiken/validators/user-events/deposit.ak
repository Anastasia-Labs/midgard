use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, Transaction,
}
use env
use midgard/common/utils.{plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/deposit.{Datum,
  Refund, SpendRedeemer, TransferToReserve}

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
      mint_redeemer_index,
      purpose,
    } = redeemer

    let hub.Datum {
      deposit: deposit_policy_id,
      settlement_queue: settlement_queue_policy_id,
      reserve_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let parsed_settlement_node_datum =
      settlement_queue.get_datum(
        reference_inputs,
        settlement_queue_policy_id,
        settlement_ref_input_index,
      )

    expect settlement_queue.NodeData {
      deposits_root,
      start_time: settlement_start_time,
      end_time: settlement_end_time,
      ..
    } = parsed_settlement_node_datum.data

    expect Some(deposit_datum) = datum
    let user_events.Datum {
      event: deposit_event,
      inclusion_time,
      refund_address,
      refund_datum,
      ..
    } = deposit_datum

    // 1. Spending a user event UTxO must also be accompanied by the burning of
    //    its NFT (and consequently deregistration of its witness staking
    //    script).
    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(deposit_policy_id),
        mint_redeemer_index,
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
          TransferToReserve { membership_proof } -> {
            // 3. Output value must be the same as the input value, except for
            //    the deposit NFT.
            //    TODO: Reserve contract might end up requiring an NFT.
            //    TODO: Fees must be deduced whenever we have a clear plan for
            //          them.
            expect
              output_value == (
                own_value |> assets.add(deposit_policy_id, burn_asset_name, -1)
              )

            let Pair(deposit_event_id, deposit_event_info) =
              ledger_state.unsafe_event_to_key_value_pair(deposit_event)

            and {
              // 4. The referenced settlement node's deposit tree must contain
              //    the subject deposit event. No need to check for the
              //    inclusion time of the deposit.
              plutarch_phas(
                deposits_root,
                deposit_event_id,
                deposit_event_info,
                membership_proof,
                redeemers,
              ),
              // 5. Assert no datum is attached to the produced UTxO.
              //    TODO: This might change depending on implementation of
              //          reserve.
              output_datum == NoDatum,
              // 6. Deposit must go to the reserve.
              output_address == reserve_addr,
            }
          }
          Refund { refund_approach } ->
            // 3. Numerous validations for the refund logic.
            user_events.validate_refund(
              own_value,
              deposit_policy_id,
              burn_asset_name,
              output_address,
              output_value,
              output_datum,
              refund_address,
              refund_datum,
              refund_approach,
              deposits_root,
              ledger_state.unsafe_event_to_id_data(deposit_event),
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
    // 1. Perform numerous validations and grant access to:
    //    - Serialized and hashed nonce based on the output reference of a
    //      specified input UTxO
    //    - The output value of the UTxO at the deposit address
    let
      l2_id,
      output_deposit_value,
      deposit_info_data,
    <-
      user_events.validate_mint(
        hub_oracle,
        fn(hub_datum) {
          let hub.Datum { deposit_addr, .. } = hub_datum
          deposit_addr
        },
        redeemer,
        own_policy,
        tx,
      )

    expect ledger_state.DepositInfo { midgard_tokens, .. } = deposit_info_data

    let midgard_tokens_value: Value = assets.from_asset_list(midgard_tokens)

    // 2. Going through all the tokens of the produced UTxO, counting its
    //    tokens, while also validating that it only contains one valid
    //    authentication NFT, and that all the specified Midgard-compatible
    //    tokens are includedd.
    let
      output_deposit_nft_is_valid,
      total_token_count,
      mcts_not_included_in_deposit_utxo,
    <-
      utils.foldl3(
        output_deposit_value |> assets.to_dict |> dict.to_pairs,
        False,
        0,
        midgard_tokens_value |> assets.to_dict |> dict.to_pairs,
        fn(
          policy_tokens_pair,
          beacon_found,
          token_count_so_far,
          remaining_midgard_tokens,
          return,
        ) {
          if !beacon_found && policy_tokens_pair.1st == own_policy {
            expect [output_deposit_nft_tn_qty] =
              policy_tokens_pair.2nd |> dict.to_pairs
            expect output_deposit_nft_tn_qty == Pair(l2_id, 1)
            return(True, token_count_so_far, remaining_midgard_tokens)
          } else {
            let policy_token_count = dict.size(policy_tokens_pair.2nd)
            when remaining_midgard_tokens is {
              [] ->
                return(
                  beacon_found,
                  token_count_so_far + policy_token_count,
                  remaining_midgard_tokens,
                )
              [mct_pair, ..rest_of_mct_pairs] -> {
                expect mct_pair == policy_tokens_pair
                return(
                  beacon_found,
                  token_count_so_far + policy_token_count,
                  rest_of_mct_pairs,
                )
              }
            }
          }
        },
      )

    and {
      // 3. Deposit NFT must be included in the produced UTxO.
      output_deposit_nft_is_valid,
      // 4. Total token count in the deposit UTxO (including ADA) must not
      //    exceed the relevant protocol parameter.
      total_token_count <= env.max_tokens_allowed_in_deposits,
      // 5. All declared MCTs must be included in the deposit UTxO.
      list.is_empty(mcts_not_included_in_deposit_utxo),
    }
  }

  else(_) {
    fail
  }
}
