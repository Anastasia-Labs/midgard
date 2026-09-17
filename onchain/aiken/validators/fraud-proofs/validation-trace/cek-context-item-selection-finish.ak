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
use midgard/validation_machine/machine_types.{CekContextControlV1}
use midgard/validation_machine/witness.{
  hash_cek_redeemer_context_control_v1, initial_cek_redeemer_context_control_v1,
}

type VerifiedDatum =
  ct.StepDatum<CekContextItemStepVerified>

validator main(
  settle_script_hash: ScriptHash,
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
        expect context.stage == 0 && next_item.stage == item.stage_terminal
        let next_context = CekContextControlV1 {
          ..context,
          stage: 1,
          redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
            initial_cek_redeemer_context_control_v1(),
          ),
          execution_memory_limit: next_item.execution_memory,
          execution_cpu_limit: next_item.execution_steps,
        }
        let next: Data = chain.ContextVerified {
          staged: verified.staged,
          successor: chain.ContinueContext { next: next_context },
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
