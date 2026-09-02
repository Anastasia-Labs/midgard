use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.{
  Args, Datum, SpendRedeemer,
}

validator main(
  accepted_spend_prefix_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
      ct.Continue(Args { input_index, output_index }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = input_state
        let expected: Data = accepted_reconstruction.initial_v1(
          bound,
          accepted_spend_prefix_script_hash,
        )
        and {
          output_hash == accepted_spend_prefix_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
