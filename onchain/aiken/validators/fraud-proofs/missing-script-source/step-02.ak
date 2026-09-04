use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_script_source/rule.{
  authenticate_trace_state_v1,
}
use midgard/fraud_proofs/missing_script_source/step_02.{Datum, SpendRedeemer}

validator main(
  step_03_validator_script_hash: ScriptHash,
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
      ct.Continue(args) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = input_state
        let expected: Data = authenticate_trace_state_v1(
          bound,
          args.trace_membership,
          args.machine_state,
          args.trace_proof,
        )
        and {
          output_hash == step_03_validator_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
