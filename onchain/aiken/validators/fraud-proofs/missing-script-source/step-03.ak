use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_script_source/rule.{
  authenticate_purpose_control_v1, authenticate_transaction_sources_v1,
}
use midgard/fraud_proofs/missing_script_source/step_03.{
  Args, Datum, SpendRedeemer,
}

validator main(
  step_04_validator_script_hash: ScriptHash,
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
        let _, _, _, input_state, next_script_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(authenticated_trace) = input_state
        let purpose = authenticate_purpose_control_v1(
          authenticated_trace,
          args.control,
          args.absolute_purpose_index,
          args.required_script_hash,
          args.purpose_subject,
          args.purpose_siblings,
        )
        let expected: Data = authenticate_transaction_sources_v1(
          purpose,
          args.transaction_source_count,
        )
        and {
          next_script_hash == step_04_validator_script_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
