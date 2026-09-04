use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/rule.{
  authenticate_execution_source_v1,
}
use midgard/fraud_proofs/execution_native_script_invalid/step_02.{
  Args, Datum, SpendRedeemer,
}

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
        let _, _, _, input_state, next_script_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = input_state
        let source = authenticate_execution_source_v1(
          bound,
          args.trace_membership,
          args.machine_state,
          args.trace_proof,
          args.control,
          args.purpose_kind,
          args.purpose_index,
          args.script_hash,
          args.purpose_subject,
          args.purpose_siblings,
          args.source_index,
          args.origin_kind,
          args.source_key,
          args.language_tag,
          args.total_length,
          args.item_commitment,
          args.source_siblings,
          args.redeemer_leaf,
          args.execution_siblings,
        )
        let expected: Data = source
        and {
          next_script_hash == step_03_validator_script_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
