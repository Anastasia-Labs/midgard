use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId, add}
use cardano/transaction.{Input, Mint, Output, OutputReference, Transaction}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/tx_order_v1.{
  Datum, SpendRedeemer, forced_inclusion_key_value, verify_order_receipts,
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

    let
        SpendRedeemer {
          input_index,
          output_index,
          hub_ref_input_index,
          settlement_ref_input_index,
          burn_redeemer_index,
          membership_proof,
          inclusion_proof_script_withdraw_redeemer_index,
          validity_override,
        }
      = redeemer

    let
        hub.Datum {
          tx_order: tx_order_policy_id,
          settlement: settlement_policy_id,
          ..
        }
      = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let settlement.Datum { forced_transactions_root, .. } = settlement.get_datum(
      reference_inputs,
      settlement_policy_id,
      settlement_ref_input_index,
    )

    expect Some(tx_order_datum) = datum
    let
        user_events.OptimisticDatum {
          event: tx_order_event,
          inclusion_time: _inclusion_time,
          refund_address,
          refund_datum,
          ..
        }
      = tx_order_datum

    expect
        user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer
      = utils.get_redeemer_at(
      redeemers,
      Mint(tx_order_policy_id),
      burn_redeemer_index,
    )

    let input, output <- singular_utxo_indexer.one_to_one(
      input_index,
      output_index,
      own_out_ref,
      inputs,
      outputs,
      True,
    )

    let Input { output: Output { value: own_value, .. }, .. } = input
    expect
        Output {
          address: output_address,
          value: output_value,
          datum: output_datum,
          reference_script: None,
        }
      = output
    expect output_address == refund_address
    expect output_datum == refund_datum
    expect
      output_value == (
        own_value
          |> add(tx_order_policy_id, burn_asset_name, -1)
      )
    let tx_order_id, forced_inclusion_tx <- forced_inclusion_key_value(
      tx_order_event_data: tx_order_event,
      validity_override: validity_override,
    )

    expect
      settlement.valid_counted_membership(
        expected_domain: transition_trace.ForcedTransactionsV1RootDomain,
        expected_root: forced_transactions_root,
        witness: membership_proof,
        key: tx_order_id,
        value: forced_inclusion_tx,
        redeemers: redeemers,
        inclusion_proof_script_withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )

    True
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle: ScriptHash,
  receipt_script_hash: ScriptHash,
  field_receipt_policy_id: PolicyId,
) {
  mint(
    redeemer: user_events.MintRedeemer,
    own_policy: PolicyId,
    tx: Transaction,
  ) {
    let l1_id, output_tx_order_value, tx_order_payload_data <- user_events.validate_mint(
      hub_oracle,
      fn(hub_datum) {
        let hub.Datum { tx_order_addr, .. } = hub_datum
        tx_order_addr
      },
      redeemer,
      own_policy,
      tx,
    )

    let output_tx_order_nft_triplet = utils.get_single_asset_from_value_apart_from_ada(
      output_tx_order_value,
    )
    expect output_tx_order_nft_triplet == (own_policy, l1_id, 1)

    expect tx_order_payload: ledger_state.TxOrderPayloadV1 = tx_order_payload_data
    expect user_events.AuthenticateEvent { nonce_input_index, .. } = redeemer
    expect Some(nonce_input) = tx.inputs |> list.at(nonce_input_index)
    expect _verified = verify_order_receipts(
      tx.reference_inputs,
      receipt_script_hash,
      field_receipt_policy_id,
      own_policy,
      nonce_input.output_reference,
      tx_order_payload,
    )

    True
  }

  else(_) {
    fail
  }
}
