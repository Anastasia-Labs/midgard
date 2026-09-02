use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, verify_membership_carried}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reference_source.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, reference_inputs_field_index,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  evaluator_step_03_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(args) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect and {
            reconstruction.state_is_authentic_v1(state),
            state.phase == reconstruction.phase_reference_source,
            state.next_expected_script_hash == own_hash,
          }
        let view = opened_field_view(
          args.reference_inputs_opening,
          BodyAnchor { tx_id: state.bound.subject.transaction_id },
          reference_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect state.field_cursor < field_item_count(view)
        let selected = spend_input_at(view, state.field_cursor)
        let out_ref = OutputReference {
          transaction_id: selected.tx_id,
          output_index: selected.output_index,
        }
        expect
          verify_membership_carried(
            args.membership,
            state.bound.prior_ledger_root,
            ledger_outref_key(out_ref),
            args.descriptor_cbor,
            reference_inputs,
            redeemers,
          )
        let descriptor = ledger_output_commitment_v1.decode(
          args.descriptor_cbor,
        )
        expect descriptor.output_index == out_ref.output_index
        let advanced = if descriptor.reference_script_language == -1 {
          reconstruction.advance_reference_without_source_v1(state, own_hash)
        } else {
          reconstruction.append_source_v1(
            state,
            reconstruction.SelectedSourceV1 {
              source_index: state.source_cursor,
              origin_kind: 1,
              source_key: field_item_at(view, state.field_cursor),
              language_tag: descriptor.reference_script_language,
              script_hash: descriptor.reference_script_hash,
              total_length: descriptor.reference_script_total_length,
              item_commitment: descriptor.reference_script_item_commitment,
            },
            if descriptor.reference_script_hash == expect_selected_hash(state) {
              evaluator_step_03_script_hash
            } else {
              own_hash
            },
          )
        }
        let expected: Data = if advanced.selected_source == None {
          let encoded: Data = advanced
          encoded
        } else {
          let encoded: Data = reconstruction.authenticated_source_v1(advanced)
          encoded
        }
        and {
          output_hash == advanced.next_expected_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn expect_selected_hash(state: reconstruction.StateV1) -> ByteArray {
  expect Some(purpose) = state.selected_purpose
  purpose.script_hash
}
