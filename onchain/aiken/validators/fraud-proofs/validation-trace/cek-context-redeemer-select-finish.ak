use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_redeemer as redeemers
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine/machine_types.{CekRedeemerContextControlV1}

type PendingDatum =
  ct.StepDatum<redeemers.RedeemerInitialPending>

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<PendingDatum>,
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
        let selection = pending.selection
        expect staged: chain.ContextStaged = selection.staged
        expect staged.context.stage == 9
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let current = selection.control
        let next_redeemers = CekRedeemerContextControlV1 {
          ..current,
          active_scan_hash: pending.initial_hash,
          active_redeemer_leaf: selection.redeemer_leaf,
          active_purpose: when pending.purpose is {
            None -> current.active_purpose
            Some(summary) -> summary
          },
        }
        let next: Data = chain.ContextVerified {
          staged: selection.staged,
          successor: chain.ContinueContext {
            next: redeemers.context_successor(staged, next_redeemers),
          },
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
