use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Mint, Output, OutputReference, Transaction}
use midgard/common/utils.{plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue
use midgard/user_events
use midgard/user_events/tx_order.{Conclude, Datum, Refund, SpendRedeemer}

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
      purpose,
    } = redeemer

    let hub.Datum {
      tx_order: tx_order_policy_id,
      settlement_queue: settlement_queue_policy_id,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let parsed_settlement_node_datum =
      settlement_queue.get_datum(
        reference_inputs,
        settlement_queue_policy_id,
        settlement_ref_input_index,
      )

    expect settlement_queue.NodeData {
      tx_orders_root,
      start_time: settlement_start_time,
      end_time: settlement_end_time,
      ..
    } = parsed_settlement_node_datum.data

    expect Some(tx_order_datum) = datum
    let user_events.Datum {
      event: tx_order_event,
      inclusion_time,
      refund_address,
      refund_datum,
      ..
    } = tx_order_datum

    // 1. Spending a user event UTxO must also be accompanied by the burning of
    //    its NFT (and consequently deregistration of its witness staking
    //    script).
    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(tx_order_policy_id),
        burn_redeemer_index,
      )

    singular_utxo_indexer.spend(
      fn(_in_ix, input, spend_purpose, _out_ix, output) {
        let Input { output: Output { value: own_value, .. }, .. } = input

        // 2. The produced UTxO must not have any scripts attached to it.
        //    TODO: Is this a good idea here?
        expect Output {
          address: output_address,
          value: output_value,
          datum: output_datum,
          reference_script: None,
        } = output

        // 3. Output value must be the same as the input value, without the tx
        //    order NFT.
        expect
          output_value == (
            own_value
              |> assets.add(tx_order_policy_id, burn_asset_name, -1)
          )

        when spend_purpose is {
          Conclude { membership_proof } -> {
            let Pair(tx_order_event_id, tx_order_event_tx) =
              ledger_state.unsafe_event_to_key_value_pair(tx_order_event)

            // 5. The referenced settlement node's tx_order tree must contain
            //    the subject tx_order event. No need to check for the
            //    inclusion time of the tx_order.
            expect
              plutarch_phas(
                tx_orders_root,
                tx_order_event_id,
                tx_order_event_tx,
                membership_proof,
                redeemers,
              )

            and {
              // 6. Assert refund datum is attached to the output.
              output_datum == refund_datum,
              // 7. Refund address must be the recipient.
              output_address == refund_address,
            }
          }
          Refund { refund_approach } ->
            // 3. Numerous validations for the refund logic.
            user_events.validate_refund(
              own_value,
              tx_order_policy_id,
              burn_asset_name,
              output_address,
              output_value,
              output_datum,
              refund_address,
              refund_datum,
              refund_approach,
              tx_orders_root,
              ledger_state.unsafe_event_to_id_data(tx_order_event),
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
    //    - The output value of the UTxO at the withdrawal address for the
    //      minting (not burning) endpoint
    //    - Order info (unused)
    let
      l1_id,
      output_tx_order_value,
      _,
    <-
      user_events.validate_mint(
        hub_oracle,
        fn(hub_datum) {
          let hub.Datum { tx_order_addr, .. } = hub_datum
          tx_order_addr
        },
        redeemer,
        own_policy,
        tx,
      )

    let output_tx_order_nft_triplet =
      utils.get_single_asset_from_value_apart_from_ada(output_tx_order_value)

    // 2. The output value of the UTxO at the withdrawal address must contain
    //    only ADA and the authentication NFT.
    output_tx_order_nft_triplet == (own_policy, l1_id, 1)
  }

  else(_) {
    fail
  }
}
