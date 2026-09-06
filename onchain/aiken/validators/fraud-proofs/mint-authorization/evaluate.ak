use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/mint_authorization/evaluate as evaluate
use midgard/fraud_proofs/mint_authorization/step_05

validator main(
  step_05_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<evaluate.Datum>,
    redeemer: evaluate.SpendRedeemer,
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
        let Pair(input_index, output_index) = when action is {
          evaluate.Advance { input_index, output_index, .. } |
          evaluate.Finalize { input_index, output_index } ->
            Pair(input_index, output_index)
        }
        expect Some(step_datum) = datum
        let own_hash, _, _, state_data, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = state_data
        when action is {
          evaluate.Advance { chunk_reference_indices, operations, .. } -> {
            let bytes = evaluate.authenticate_payload(
              state,
              evaluate.chunks(chunk_reference_indices, tx.reference_inputs),
            )
            let expected: Data = evaluate.advance(state, bytes, operations)
            expect output_hash == own_hash
            output_state == expected
          }
          evaluate.Finalize { .. } -> {
            expect state.cursor == state.script_length
            expect state.stack_root == #""
            expect state.stack_depth == 0
            expect state.result == 0
            expect output_hash == step_05_validator_script_hash
            let expected: Data = step_05.State {
              policy_id: state.policy_id,
              direction: 1,
            }
            output_state == expected
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
