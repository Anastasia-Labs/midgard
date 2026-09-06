use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_redeemer as redeemers
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_context_v1

type PendingDatum =
  ct.StepDatum<redeemers.RedeemerSelectionPending>

validator main(
  hash_script_hash: ScriptHash,
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
        expect staged: chain.ContextStaged = pending.staged
        expect staged.context.stage == 9
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let purpose = script_context_v1.script_purpose_summary_v1(
          pending.purpose_kind,
          pending.script_hash,
          pending.subject,
          staged.context.language_tag == 128,
        )
        when purpose is {
          None -> {
            expect pending.purpose_kind == 3 && staged.context.language_tag == 3
          }
          Some(_) -> Void
        }
        let next: Data = redeemers.RedeemerPurposePending {
          selection: pending,
          purpose,
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
            hash == hash_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
