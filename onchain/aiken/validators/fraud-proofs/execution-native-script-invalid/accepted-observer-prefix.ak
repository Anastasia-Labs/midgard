use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.{
  Action, Datum, FinishObservers, ScanObserver, SpendRedeemer,
}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, required_observers_field_index,
}
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}

validator main(
  accepted_receive_prefix_script_hash: ScriptHash,
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
            state.phase == reconstruction.phase_observe,
            state.next_expected_script_hash == own_hash,
          }
        let opening = when action is {
          ScanObserver { observer_opening, .. } -> observer_opening
          FinishObservers { observer_opening, .. } -> observer_opening
        }
        let view = opened_field_view(
          opening,
          BodyAnchor { tx_id: state.bound.subject.transaction_id },
          required_observers_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        when action is {
          ScanObserver { .. } -> {
            expect state.field_cursor < field_item_count(view)
            let observer = field_item_at(view, state.field_cursor)
            expect bytearray.length(observer) == 28
            let advanced = reconstruction.append_purpose_v1(
              state,
              reconstruction.phase_observe,
              state.field_cursor,
              observer,
              observer,
              observer,
              if state.execution_cursor == state.bound.execution_index {
                accepted_inline_source_script_hash
              } else {
                own_hash
              },
            )
            let expected: Data = advanced
            and {
              output_hash == advanced.next_expected_script_hash,
              output_state == expected,
            }
          }
          FinishObservers { .. } -> {
            expect state.field_cursor == field_item_count(view)
            let advanced = reconstruction.finish_purpose_phase_v1(
              state,
              accepted_receive_prefix_script_hash,
            )
            let expected: Data = advanced
            and {
              output_hash == accepted_receive_prefix_script_hash,
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
    ScanObserver { input_index, .. } -> input_index
    FinishObservers { input_index, .. } -> input_index
  }
}

fn action_output_index(action: Action) -> Int {
  when action is {
    ScanObserver { output_index, .. } -> output_index
    FinishObservers { output_index, .. } -> output_index
  }
}
