use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_finalization as finalization
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_context_v1

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<finalization.FinalizationPending>>,
    redeemer: chain.ContextRedeemer,
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
        expect Some(step) = datum
        expect Some(pending) = step.data
        expect staged: chain.ContextStaged = pending.staged
        expect staged.context.stage == 10
        expect
          staged.context.language_tag != 128 && staged.context.purpose_kind == 2
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        expect Some(info) = script_context_v1.script_purpose_summary_v1(
          2,
          staged.context.script_hash,
          staged.context.subject,
          False,
        )
        let next: Data = finalization.finalized(pending, staged, info)
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) {
            hash == settle_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
