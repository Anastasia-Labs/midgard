use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/mint_authorization/evaluate
use midgard/fraud_proofs/mint_authorization/step_04
use midgard/fraud_proofs/mint_authorization/witness_scan as scan

validator main(
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<scan.Datum>,
    redeemer: scan.SpendRedeemer,
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
          scan.Advance { input_index, output_index, .. } |
          scan.Finalize { input_index, output_index } ->
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
          scan.Advance { chunk_reference_indices, .. } -> {
            expect state.item_index < state.item_count
            let bytes = scan.authenticate(
              state,
              evaluate.chunks(chunk_reference_indices, tx.reference_inputs),
            )
            let expected: Data = scan.advance(state, bytes, 16)
            expect output_hash == own_hash
            output_state == expected
          }
          scan.Finalize { .. } -> {
            expect state.item_index == state.item_count
            expect state.cursor == state.field_length
            expect output_hash == step_04_validator_script_hash
            let expected: Data = step_04.State {
              policy_id: state.policy_id,
              bad_tx_id: state.bad_tx_id,
              prior_ledger_root: state.prior_ledger_root,
              ref_cursor: 0,
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
