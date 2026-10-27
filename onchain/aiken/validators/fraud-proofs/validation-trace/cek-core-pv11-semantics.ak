use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1 as builtin_witness
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}

/// The semantics arm of the protocol version 11 builtins (87 to 100), the
/// last hop before settlement: a success (core group 13) returns its result,
/// a failure (core group 14) is a known failure of its builtin.
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
        // As in the budget arm: group 13 is arm 30 and group 14 arm 38, each
        // over tags 87 to 100 alone (`core_group`).
        let success = bound.group == 13
        expect success || bound.group == 14
        expect [tag_data,
              arguments_data, ..rest] = builtin.un_constr_data(action.witness).2nd
        expect tag: Int = tag_data
        expect arguments: List<builtin_witness.ValueWitnessV1> = arguments_data
        expect
          if success {
            expect [result_data] = rest
            expect result: builtin_witness.ValueWitnessV1 = result_data
            builtin_witness.verify_pv11_semantics_v1(tag, arguments, result)
          } else {
            builtin_witness.verify_pv11_builtin_failure_v1(
              tag,
              bound.pre.focus_root,
              arguments,
            )
          }
        chain.continue_core(
          settle_script_hash,
          computation_thread_policy_id,
          step,
          action,
          if success {
            2
          } else {
            1
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
