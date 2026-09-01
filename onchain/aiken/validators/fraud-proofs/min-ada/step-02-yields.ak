use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{
  published_chunk_membership_four_chunks,
}
use midgard/fraud_proofs/common.{
  PublishedChunkMembership, RedeemerCarriedMembership, verify_membership_carried,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, outputs_field_index,
}
use midgard/fraud_proofs/min_ada/step_01.{MinAdaTx, MinAdaUtxo}
use midgard/fraud_proofs/min_ada/step_02.{Args, YieldRedeemer}
use midgard/fraud_proofs/min_ada/step_03 as step_03
use midgard/fraud_proofs/min_ada/yield.{unique_dispatch}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_v1
use midgard/native_tx_field_access_v1.{field_item_at}

validator tx(
  dispatcher_script_hash: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(
    _yield_redeemer: YieldRedeemer,
    _credential: Credential,
    transaction: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = transaction
    let (step_datum, spend_redeemer, _own_out_ref) = unique_dispatch(
      dispatcher_script_hash,
      inputs,
      redeemers,
    )
    expect
        ct.Continue(Args {
          input_index: _,
          output_index,
          yield_to_ref_input_index: _,
          outputs_opening: Some(opening),
          post_membership: None,
        })
      = spend_redeemer
    expect Some(state) = step_datum.data
    expect MinAdaTx { output_index: bad_output_index } = state.fault
    let outputs_view = opened_field_view(
      opening,
      BodyAnchor { tx_id: state.bad_tx_id },
      outputs_field_index,
      reference_inputs,
      field_preimage_certificate_policy_id,
    )
    let output_cbor = field_item_at(outputs_view, bad_output_index)
    expect Some(output) = ledger_output_v1.decode_canonical_output(output_cbor)
    expect Some(next_output) = outputs |> list.at(output_index)
    expect InlineDatum(next_datum_data) = next_output.datum
    expect next_datum: ct.StepDatum<step_03.State> = next_datum_data
    expect Some(output_state) = next_datum.data
    output_state == step_03.MinAdaTxDescriptor {
      total_length: bytearray.length(output_cbor),
      lovelace: output.value.lovelace,
    }
  }

  else(_) {
    fail
  }
}

validator utxo(dispatcher_script_hash: ScriptHash) {
  withdraw(
    _yield_redeemer: YieldRedeemer,
    _credential: Credential,
    transaction: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = transaction
    let (step_datum, spend_redeemer, _own_out_ref) = unique_dispatch(
      dispatcher_script_hash,
      inputs,
      redeemers,
    )
    expect
        ct.Continue(Args {
          input_index: _,
          output_index,
          yield_to_ref_input_index: _,
          outputs_opening: None,
          post_membership: Some(membership),
        })
      = spend_redeemer
    expect Some(state) = step_datum.data
    expect MinAdaUtxo = state.fault
    expect Some(post) = state.post_utxo
    expect Some(next_output) = outputs |> list.at(output_index)
    expect InlineDatum(next_datum_data) = next_output.datum
    expect next_datum: ct.StepDatum<step_03.State> = next_datum_data
    expect Some(output_state) = next_datum.data
    expect
      output_state == step_03.MinAdaUtxoDescriptor {
        descriptor_cbor: post.descriptor_cbor,
        out_ref_key: ledger_outref_key(post.out_ref),
        prev_utxos_root: post.prev_utxos_root,
      }
    when membership is {
      RedeemerCarriedMembership { .. } ->
        verify_membership_carried(
          membership,
          post.post_utxos_root,
          ledger_outref_key(post.out_ref),
          post.descriptor_cbor,
          reference_inputs,
          redeemers,
        )
      PublishedChunkMembership(published) ->
        published_chunk_membership_four_chunks(
          reference_inputs,
          published,
          post.post_utxos_root,
          ledger_outref_key(post.out_ref),
          post.descriptor_cbor,
        )
    }
  }

  else(_) {
    fail
  }
}
