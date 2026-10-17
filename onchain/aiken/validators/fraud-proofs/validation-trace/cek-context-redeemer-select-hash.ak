use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_redeemer as redeemers
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/validation_machine/shared.{purpose_kind_to_redeemer_tag}

type PendingDatum =
  ct.StepDatum<redeemers.RedeemerPurposePending>

validator main(
  finish_script_hash: ScriptHash,
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
        let mode = when pending.purpose is {
          None -> item.mode_descriptor
          Some(_) -> item.mode_data
        }
        let initial = item.initial_control_v1(
          mode,
          selection.item_index,
          selection.item_count,
          selection.total_length,
          selection.item_commitment,
          purpose_kind_to_redeemer_tag(selection.purpose_kind),
          selection.purpose_index,
        )
        let next: Data = redeemers.RedeemerInitialPending {
          selection,
          purpose: pending.purpose,
          initial_hash: item.hash_control_v1(initial),
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
            hash == finish_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
