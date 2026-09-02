use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, verify_membership_carried}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.{
  Action, Datum, FinishSpends, ScanSpend, SpendRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_tx/types.{ScriptCredential}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  accepted_mint_prefix_script_hash: ScriptHash,
  accepted_inline_source_script_hash: ScriptHash,
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
      ct.Continue(action) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          action_input_index(action),
          action_output_index(action),
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect and {
            reconstruction.state_is_authentic_v1(state),
            state.phase == reconstruction.phase_spend,
            state.next_expected_script_hash == own_hash,
          }
        let opening = when action is {
          ScanSpend { spend_inputs_opening, .. } -> spend_inputs_opening
          FinishSpends { spend_inputs_opening, .. } -> spend_inputs_opening
        }
        let view = opened_field_view(
          opening,
          BodyAnchor { tx_id: state.bound.subject.transaction_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        when action is {
          ScanSpend { descriptor_cbor, membership, .. } -> {
            expect state.field_cursor < field_item_count(view)
            let selected = spend_input_at(view, state.field_cursor)
            let out_ref = OutputReference {
              transaction_id: selected.tx_id,
              output_index: selected.output_index,
            }
            expect
              verify_membership_carried(
                membership,
                state.bound.prior_ledger_root,
                ledger_outref_key(out_ref),
                descriptor_cbor,
                reference_inputs,
                redeemers,
              )
            let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
            expect descriptor.output_index == out_ref.output_index
            expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
              descriptor.address,
            )
            let canonical_key = field_item_at(view, state.field_cursor)
            let advanced = when address.payment_credential is {
              ScriptCredential(script_hash) ->
                reconstruction.append_purpose_v1(
                  state,
                  reconstruction.phase_spend,
                  state.field_cursor,
                  script_hash,
                  canonical_key,
                  canonical_key,
                  if state.execution_cursor == state.bound.execution_index {
                    accepted_inline_source_script_hash
                  } else {
                    own_hash
                  },
                )
              _ ->
                reconstruction.advance_non_script_item_v1(
                  state,
                  canonical_key,
                  own_hash,
                )
            }
            let expected: Data = advanced
            and {
              output_hash == advanced.next_expected_script_hash,
              output_state == expected,
            }
          }
          FinishSpends { .. } -> {
            expect state.field_cursor == field_item_count(view)
            let advanced = reconstruction.finish_purpose_phase_v1(
              state,
              accepted_mint_prefix_script_hash,
            )
            let expected: Data = advanced
            and {
              output_hash == accepted_mint_prefix_script_hash,
              output_state == expected,
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn action_input_index(action: Action) -> Int {
  when action is {
    ScanSpend { input_index, .. } -> input_index
    FinishSpends { input_index, .. } -> input_index
  }
}

fn action_output_index(action: Action) -> Int {
  when action is {
    ScanSpend { output_index, .. } -> output_index
    FinishSpends { output_index, .. } -> output_index
  }
}
