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
  Datum, MintRedeemer, SpendRedeemer, forced_inclusion_key_value,
  material_carriage_matches_event, verify_order_material,
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

/// `field_preimage_certificate_policy_id` is the §8.6 certificate minting
/// policy, consulted by the field-access door on tier-3 carriage only. It is the
/// same deployment role #592 threads into the validation-trace validators, so one
/// deployed certificate policy serves both readers; the catalogue,
/// deployment-registry and format-pin churn this parameter causes is recorded for
/// #579's single regeneration batch (rider 2 of its owner-authorized scope
/// amendment) and is never edited in this lane.
validator mint(
  hub_oracle: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let MintRedeemer { event, material_carriage } = redeemer

    // A burn reads no material, so it must carry no vector rather than carry one
    // nothing looks at: an unread wire field is where a second spelling of the
    // same transaction hides (§6.1). The rule lives in the library so it has a
    // refuting negative — this module has no test harness.
    expect material_carriage_matches_event(event, material_carriage)

    let l1_id, output_tx_order_value, tx_order_payload_data <- user_events.validate_mint(
      hub_oracle,
      fn(hub_datum) {
        let hub.Datum { tx_order_addr, .. } = hub_datum
        tx_order_addr
      },
      event,
      own_policy,
      tx,
    )

    let output_tx_order_nft_triplet = utils.get_single_asset_from_value_apart_from_ada(
      output_tx_order_value,
    )
    expect output_tx_order_nft_triplet == (own_policy, l1_id, 1)

    expect tx_order_payload: ledger_state.TxOrderPayloadV1 = tx_order_payload_data
    expect
      verify_order_material(
        tx_order_payload,
        material_carriage,
        tx.reference_inputs,
        field_preimage_certificate_policy_id,
      )

    True
  }

  else(_) {
    fail
  }
}
