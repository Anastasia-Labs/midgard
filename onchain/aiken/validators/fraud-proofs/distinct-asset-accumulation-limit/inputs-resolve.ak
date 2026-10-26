use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{
  InputsCross, InputsWalked,
}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  PendingDatum, ResolveRedeemer,
}

/// Counts a taken batch of spent inputs, each from its ledger descriptor
/// under the event pre-root. A crossing goes to the ranking step; otherwise
/// the walk returns to the taking step, or to the output field's grammar door
/// once the spend field is exhausted.
validator main(
  rank_hash: ScriptHash,
  outputs_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
) {
  spend(
    datum: Option<PendingDatum>,
    redeemer: ResolveRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(pending) = input_state
        when fold.resolve_inputs_v1(pending, args.witnesses) is {
          InputsCross(rank) -> {
            let expected: Data = rank
            output_hash == rank_hash && output_state == expected
          }
          InputsWalked(walked) -> {
            let expected: Data = walked
            and {
              output_hash == if walked.complete {
                outputs_grammar_hash
              } else {
                pending.resume_script_hash
              },
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
