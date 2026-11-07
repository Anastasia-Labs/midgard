use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_machine_v1 as machine
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.CoreDatum>,
    redeemer: chain.ArmRedeemer,
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
        expect bound.group == 1
        expect witness: machine.CoreStepWitnessV1 = action.witness
        expect
          if bound.pre.mode == machine.mode_lookup {
            machine.verify_lookup_step(bound.pre, bound.post, witness)
          } else if bound.pre.mode == machine.mode_return {
            machine.verify_return_step(bound.pre, bound.post, witness)
          } else if bound.pre.mode == machine.mode_case_select {
            machine.verify_case_select_step(bound.pre, bound.post, witness)
          } else if bound.pre.mode == machine.mode_case_apply {
            machine.verify_case_apply_step(bound.pre, bound.post, witness)
          } else {
            False
          }
        chain.continue_core(
          settle_script_hash,
          computation_thread_policy_id,
          step,
          action,
          0,
          bound.facts,
          own_out_ref,
          tx,
        )
      }
    }
  }

  else(_) {
    fail
  }
}
