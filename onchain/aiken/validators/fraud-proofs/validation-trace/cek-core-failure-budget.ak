use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1 as builtin_witness
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
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
        expect bound.group == 9
        expect bound.pre.mode == machine.mode_builtin
        expect Pair(38, [tag_data,
              arguments_data]) = builtin.un_constr_data(action.witness)
        expect tag: Int = tag_data
        expect arguments: List<builtin_witness.ValueWitnessV1> = arguments_data
        expect bound.arm == 38
        let budget = builtin_witness.authenticated_builtin_failure_budget(
          tag,
          arguments,
        )
        expect
          bound.post == machine.exact_state(
            bound.pre,
            machine.mode_halt_error,
            proof.hash_error_term_v1(),
            proof.empty_environment_root_v1,
            proof.empty_continuation_root_v1,
            machine.error_builtin_failure,
            budget.cpu,
            budget.memory,
          )
        chain.continue_core(
          settle_script_hash,
          computation_thread_policy_id,
          step,
          action,
          1,
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
