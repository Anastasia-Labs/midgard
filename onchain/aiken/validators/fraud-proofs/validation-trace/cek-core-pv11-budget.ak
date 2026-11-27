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

/// The budget arm of the protocol version 11 builtins (87 to 100): the second
/// hop of a success (core group 13, after the roots hop) and the first of a
/// failure (core group 14). Both go on to the protocol version 11 semantics
/// arm.
validator main(
  semantics_script_hash: ScriptHash,
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
        // The binder sets the group with `core_group`, from the bound witness
        // and a builtin-mode pre-state: group 13 is arm 30 and group 14 arm
        // 38, each over tags 87 to 100 alone.
        let success = bound.group == 13
        expect success || bound.group == 14
        expect [tag_data,
              arguments_data, ..] = builtin.un_constr_data(action.witness).2nd
        expect tag: Int = tag_data
        expect arguments: List<builtin_witness.ValueWitnessV1> = arguments_data
        expect
          if success {
            expect roots: chain.BuiltinRoots = bound.facts
            let budget = builtin_witness.pv11_direct_budget_v1(tag, arguments)
            bound.post == machine.exact_state(
              bound.pre,
              machine.mode_return,
              roots.result_root,
              proof.empty_environment_root_v1,
              bound.pre.continuation_root,
              0,
              budget.cpu,
              budget.memory,
            )
          } else {
            let budget = builtin_witness.pv11_failure_budget_v1(tag, arguments)
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
          }
        chain.continue_core(
          semantics_script_hash,
          computation_thread_policy_id,
          step,
          action,
          if success {
            1
          } else {
            0
          },
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
