use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_redeemer_normalization_v1 as envelope

pub type Datum =
  ct.StepDatum<Data>

pub type Action {
  AuthenticateSource {
    input_index: Int,
    output_index: Int,
    witness: item.RedeemerItemProofWitnessV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

validator main(
  deployment_id: ByteArray,
  computation_thread_policy_id: PolicyId,
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
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) -> {
        let AuthenticateSource { input_index, output_index, witness } = action
        expect Some(step_datum) = datum
        continue(
          computation_thread_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(
            input_script_hash,
            _token_name,
            _prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(raw_state) = input_state
            expect state: normalized.ControlsChecked = raw_state
            expect state.envelope.deployment_id == deployment_id
            expect
              output_script_hash == state.envelope.semantic_executor_script_hash
            let witness_data: Data = witness
            expect normalized.raw_data_hash(witness_data) == state.witness_hash
            expect Some(source_bytes) = item.authenticate_source_window(
              state.current_control,
              item.prevalidated_next_source_span(state.current_control),
              witness,
            )
            let output = normalized.execution_output(
              state.envelope,
              state.witness_hash,
            )
            let expected: Data = when witness.action is {
              item.RedeemerItemTraverseData { action: traversal_action } -> {
                expect Some(current) = state.current_control.traversal
                expect Some(next) = state.next_control.traversal
                expect
                  state.next_control == item.RedeemerItemProofControlV1 {
                    ..state.current_control,
                    traversal: Some(next),
                  }
                let result: Data = normalized.TraversalExecution {
                  output,
                  current,
                  next,
                  action: traversal_action,
                  source_bytes,
                }
                result
              }
              action -> {
                let result: Data = normalized.OuterExecution {
                  output,
                  current: state.current_control,
                  next: state.next_control,
                  action,
                  source_bytes,
                }
                result
              }
            }
            output_state == expected
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
