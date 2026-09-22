use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_item.{ItemValueResult}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_redeemer_normalization_v1.{
  CekContextItemStepVerified,
}

type VerifiedDatum =
  ct.StepDatum<CekContextItemStepVerified>

validator main(
  finish_script_hash: ScriptHash,
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
        expect
          verified.next.stage == item.stage_terminal && verified.next.mode == item.mode_data
        expect Some(summary) = item.finalize_v1(verified.next)
        let verified_data: Data = verified
        let next: Data = ItemValueResult { verified: verified_data, summary }
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
