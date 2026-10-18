use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, verify_membership_carried}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reference_source.{
  Action, Datum, FinishResolvedOutputs, ScanResolvedOutput, SpendRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyTxOpening, anchored_field_view, anchored_native_tx,
  opened_field_view, reference_inputs_field_index, spend_inputs_field_index,
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
            state.phase == reconstruction.phase_reference_source,
            state.next_expected_script_hash == own_hash,
          }
        let anchor = BodyAnchor {
          tx_id: state.bound.subject.transaction_id,
          source_kind: state.bound.subject.source_kind,
        }
        when action is {
          ScanResolvedOutput {
            field_index,
            item_index,
            inputs_opening,
            descriptor_cbor,
            membership,
            ..
          } -> {
            expect or {
                field_index == spend_inputs_field_index,
                field_index == reference_inputs_field_index,
              }
            let view = opened_field_view(
              inputs_opening,
              anchor,
              field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let selected = spend_input_at(view, item_index)
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
            let key = field_item_at(view, item_index)
            let advanced = reconstruction.scan_resolved_output_v1(
              state,
              key,
              if descriptor.reference_script_language == -1 {
                None
              } else {
                Some(
                  reconstruction.SelectedSourceV1 {
                    source_index: state.source_cursor,
                    origin_kind: 1,
                    source_key: key,
                    language_tag: descriptor.reference_script_language,
                    script_hash: descriptor.reference_script_hash,
                    total_length: descriptor.reference_script_total_length,
                    item_commitment: descriptor.reference_script_item_commitment,
                  },
                )
              },
              own_hash,
            )
            let expected: Data = advanced
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          FinishResolvedOutputs {
            native_tx_compact_cbor,
            spend_inputs_carriage,
            reference_inputs_carriage,
            ..
          } -> {
            let anchored = anchored_native_tx(
              BodyTxOpening { native_tx_compact_cbor },
              anchor,
            )
            let resolved_output_count = field_item_count(
              anchored_field_view(
                anchored,
                spend_inputs_field_index,
                spend_inputs_carriage,
                reference_inputs,
                field_preimage_certificate_policy_id,
              ),
            ) + field_item_count(
              anchored_field_view(
                anchored,
                reference_inputs_field_index,
                reference_inputs_carriage,
                reference_inputs,
                field_preimage_certificate_policy_id,
              ),
            )
            let expected: Data = reconstruction.finish_resolved_outputs_v1(
              state,
              resolved_output_count,
            )
            and {
              output_hash == evaluator_step_03_script_hash,
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
    ScanResolvedOutput { input_index, .. } -> input_index
    FinishResolvedOutputs { input_index, .. } -> input_index
  }
}

fn action_output_index(action: Action) -> Int {
  when action is {
    ScanResolvedOutput { output_index, .. } -> output_index
    FinishResolvedOutputs { output_index, .. } -> output_index
  }
}
