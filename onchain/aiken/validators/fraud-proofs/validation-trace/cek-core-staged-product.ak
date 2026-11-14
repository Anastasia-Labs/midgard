use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_machine_v1 as machine
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_product_v1.{advance_product_v1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.CoreDatum>,
    redeemer: chain.LoopRedeemer,
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
        expect Some(bound) = step.data
        expect bound.group == 5
        expect bound.pre.mode == machine.mode_builtin
        expect bound.arm == 31
        chain.continue_loop(
          settle_script_hash,
          computation_thread_policy_id,
          step,
          action,
          3,
          own_out_ref,
          tx,
          fn(inner, from, end) {
            advance_product_v1(
              bound.arm,
              inner,
              action.witness,
              tx.reference_inputs,
              from,
              end,
            )
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
