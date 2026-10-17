use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_observer as observer
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<observer.ObserverPending>>,
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
        expect staged.context.stage == 5
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let next: Data = chain.ContextVerified {
          staged: pending.staged,
          successor: observer.next_observer(staged, pending.opening),
        }
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
