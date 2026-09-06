use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_redeemer_normalization_v1.{
  CekContextItemStepVerified,
}

type VerifiedDatum =
  ct.StepDatum<CekContextItemStepVerified>

validator main(
  return_script_hashes: List<ScriptHash>,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<VerifiedDatum>,
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
        expect Some(verified) = step.data
        expect staged: chain.ContextStaged = verified.staged
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let context = staged.context
        let next_item = verified.next
        let route = if context.stage == 0 {
          if next_item.stage == item.stage_terminal {
            1
          } else {
            0
          }
        } else {
          expect context.stage == 9
          if next_item.stage != item.stage_terminal {
            2
          } else if next_item.mode == item.mode_descriptor {
            3
          } else {
            4
          }
        }
        expect Some(return_hash) = list.at(return_script_hashes, route)
        let next: Data = verified
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) { hash == return_hash && state == next },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
