use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as worker

/// Resolves the pending spent inputs against the event pre-state and the
/// source map. The walk resumes at the step that took them until the spend
/// field is exhausted, then goes to the mint field's grammar door.
validator main(
  mint_grammar_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<worker.PendingDatum>,
    redeemer: worker.SpendResolveRedeemer,
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
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(pending) = input_state
        let resolved = scan.resolve_spends_v1(pending, args.witnesses)
        let expected: Data = resolved
        and {
          output_hash == if resolved.complete {
            mint_grammar_script_hash
          } else {
            pending.resume_script_hash
          },
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
