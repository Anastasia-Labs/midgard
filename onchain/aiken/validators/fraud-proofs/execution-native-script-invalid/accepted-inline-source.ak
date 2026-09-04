use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_inline_source.{
  Action, Datum, FinishInline, ScanInline, SpendRedeemer,
}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, opened_field_view, script_witnesses_field_index,
}
use midgard/fraud_proofs/native_tx/compact.{decode_native_tx_compact_v1}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}
use midgard/script_proof_v1

validator main(
  evaluator_step_03_script_hash: ScriptHash,
  accepted_reference_source_script_hash: ScriptHash,
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
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
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
            state.phase == reconstruction.phase_inline_source,
            state.next_expected_script_hash == own_hash,
          }
        let compact = decode_native_tx_compact_v1(state.bound.compact_cbor)
        let opening = when action is {
          ScanInline { scripts_opening, .. } -> scripts_opening
          FinishInline { scripts_opening, .. } -> scripts_opening
        }
        let view = opened_field_view(
          opening,
          WitnessAnchor {
            tx_id: state.bound.subject.transaction_id,
            witness_set_hash: compact.witness_set_hash,
          },
          script_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        when action is {
          ScanInline { .. } -> {
            expect state.field_cursor < field_item_count(view)
            let item = field_item_at(view, state.field_cursor)
            let Pair(offset, script) = decode_midgard_versioned_script_at(
              item,
              0,
            )
            expect and {
                offset == bytearray.length(item),
                encode_midgard_versioned_script(script) == item,
              }
            let advanced = reconstruction.append_source_v1(
              state,
              reconstruction.SelectedSourceV1 {
                source_index: state.source_cursor,
                origin_kind: 0,
                source_key: cbor.serialise(state.field_cursor),
                language_tag: script_proof_v1.language_tag(script.language),
                script_hash: script_proof_v1.versioned_script_hash(script),
                total_length: bytearray.length(item),
                item_commitment: bounded_item_v1.from_bytes(
                  script_witnesses_field_index,
                  state.field_cursor,
                  item,
                ),
              },
              if script_proof_v1.versioned_script_hash(script) == expect_selected_hash(
                state,
              ) {
                evaluator_step_03_script_hash
              } else {
                own_hash
              },
            )
            let expected: Data = if advanced.selected_source == None {
              let encoded: Data = advanced
              encoded
            } else {
              let encoded: Data = reconstruction.authenticated_source_v1(
                advanced,
              )
              encoded
            }
            and {
              output_hash == advanced.next_expected_script_hash,
              output_state == expected,
            }
          }
          FinishInline { .. } -> {
            expect state.field_cursor == field_item_count(view)
            let advanced = reconstruction.finish_inline_sources_v1(
              state,
              accepted_reference_source_script_hash,
            )
            let expected: Data = advanced
            and {
              output_hash == accepted_reference_source_script_hash,
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

fn expect_selected_hash(state: reconstruction.StateV1) -> ByteArray {
  expect Some(purpose) = state.selected_purpose
  purpose.script_hash
}

fn action_input_index(action: Action) -> Int {
  when action is {
    ScanInline { input_index, .. } -> input_index
    FinishInline { input_index, .. } -> input_index
  }
}

fn action_output_index(action: Action) -> Int {
  when action is {
    ScanInline { output_index, .. } -> output_index
    FinishInline { output_index, .. } -> output_index
  }
}
