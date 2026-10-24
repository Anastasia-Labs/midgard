use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_outputs.{
  Datum, Finish, Scan, SpendRedeemer, indices,
}

validator main(
  receive_finish_script_hash: ScriptHash,
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
        let Pair(input_index, output_index) = indices(action)
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        when action is {
          Scan { outputs_opening, checkpoint_bytes, witnesses, .. } -> {
            let Pair(advanced, _) = receive.advance_outputs_v1(
              state,
              outputs_opening,
              checkpoint_bytes,
              witnesses,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let expected: Data = advanced
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          // Forward exact custody; the physical finish step independently
          // authenticates the actual field and proves checkpoint exhaustion.
          Finish { .. } -> {
            expect state.complete
            let expected: Data = state
            and {
              output_hash == receive_finish_script_hash,
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
