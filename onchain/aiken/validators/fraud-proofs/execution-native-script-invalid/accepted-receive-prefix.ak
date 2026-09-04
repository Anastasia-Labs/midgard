use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.{
  Action, Datum, FinishOutputPass, ScanOutput, SpendRedeemer,
}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, outputs_field_index,
}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_output_cbor}
use midgard/fraud_proofs/native_tx/types.{ScriptCredential}
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}

validator main(
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
            state.phase == reconstruction.phase_receive,
            state.next_expected_script_hash == own_hash,
          }
        let opening = when action is {
          ScanOutput { outputs_opening, .. } -> outputs_opening
          FinishOutputPass { outputs_opening, .. } -> outputs_opening
        }
        let view = opened_field_view(
          opening,
          BodyAnchor { tx_id: state.bound.subject.transaction_id },
          outputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        when action is {
          ScanOutput { .. } -> {
            expect state.field_cursor < field_item_count(view)
            let output = decode_midgard_tx_output_cbor(
              field_item_at(view, state.field_cursor),
            )
            let candidate = if output.address.protected {
              when output.address.payment_credential is {
                ScriptCredential(script_hash) -> Some(script_hash)
                _ -> None
              }
            } else {
              None
            }
            let advanced = reconstruction.scan_receive_output_v1(
              state,
              candidate,
              own_hash,
            )
            let expected: Data = advanced
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          FinishOutputPass { .. } -> {
            expect state.field_cursor == field_item_count(view)
            let advanced = reconstruction.finish_receive_pass_v1(
              state,
              own_hash,
              accepted_inline_source_script_hash,
            )
            let expected: Data = advanced
            and {
              output_hash == advanced.next_expected_script_hash,
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
    ScanOutput { input_index, .. } -> input_index
    FinishOutputPass { input_index, .. } -> input_index
  }
}

fn action_output_index(action: Action) -> Int {
  when action is {
    ScanOutput { output_index, .. } -> output_index
    FinishOutputPass { output_index, .. } -> output_index
  }
}
