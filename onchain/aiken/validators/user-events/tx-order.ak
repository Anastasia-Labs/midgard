use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Mint, Output, OutputReference, Transaction}
use midgard/common/utils.{plutarch_phas}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement
use midgard/user_events
use midgard/user_events/tx_order.{Datum, SpendRedeemer}

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
      membership_proof,
      validity_override,
    } = redeemer

    let hub.Datum {
      tx_order: tx_order_policy_id,
      settlement: settlement_policy_id,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let settlement.Datum { transactions_root, .. } =
      settlement.get_datum(
        reference_inputs,
        settlement_policy_id,
        settlement_ref_input_index,
      )

    expect Some(tx_order_datum) = datum
    let user_events.OptimisticDatum {
      event: tx_order_event,
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

    // TODO: `singular_utxo_indexer` has a flawed interface currently. Passing
    //       `0` as the throwaway redeemer.
    singular_utxo_indexer.spend(
      fn(_in_ix, input, _redeemer, _out_ix, output) {
        let Input { output: Output { value: own_value, .. }, .. } = input

        // 2. The produced UTxO must not have any scripts attached to it.
        //    TODO: Is this a good idea here?
        expect Output {
          address: output_address,
          value: output_value,
          datum: output_datum,
          reference_script: None,
        } = output

        // 3. Refund address must be the recipient.
        expect output_address == refund_address

        // 4. Assert refund datum is attached to the output.
        expect output_datum == refund_datum

        // 5. Output value must be the same as the input value, without the tx
        //    order NFT.
        expect
          output_value == (
            own_value
              |> assets.add(tx_order_policy_id, burn_asset_name, -1)
          )

        // 6. For tx order UTxOs to be collectible, they must be stored in
        //    the tree with the validity provided via the redeemer.
        let Pair(tx_order_event_id, tx_order_event_tx) =
          ledger_state.unsafe_event_to_key_value_pair(tx_order_event)
        //
        expect tx_order_tx: ledger_state.MidgardTxCompact = tx_order_event_tx
        //
        let overridden_tx_order_tx: Data =
          ledger_state.MidgardTxCompact {
            ..tx_order_tx,
            validity: validity_override,
          }

        // 7. The referenced settlement UTxO's tx_order tree must contain the
        //    subject tx_order event.
        expect
          plutarch_phas(
            transactions_root,
            tx_order_event_id,
            overridden_tx_order_tx,
            membership_proof,
            redeemers,
          )

        // Done.
        True
      },
      0,
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
    //    - The output value of the UTxO at the tx order address for the minting
    //      (not burning) endpoint
    //    - Included Midgard tx as `Data`
    let
      l1_id,
      output_tx_order_value,
      midgard_tx_data,
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

    // 2. The output value of the UTxO at the tx order address must contain only
    //    ADA and the authentication NFT.
    expect output_tx_order_nft_triplet == (own_policy, l1_id, 1)

    // 3. Tx order must be marked as valid. Only operators can inject invalid tx
    //    orders in the tree.
    expect ledger_state.MidgardTxCompact {
      validity: ledger_state.TxIsValid,
      ..
    } = midgard_tx_data

    // Done.
    True
  }

  else(_) {
    fail
  }
}
