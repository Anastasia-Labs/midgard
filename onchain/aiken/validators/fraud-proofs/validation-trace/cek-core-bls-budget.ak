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
  final_script_hash: ScriptHash,
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
        expect bound.group == 8
        expect bound.pre.mode == machine.mode_builtin
        expect
            Pair(37,
            [
              left_root_data, right_root_data, left_data, right_data,
              result_data,
            ])
          = builtin.un_constr_data(action.witness)
        expect left_root: ByteArray = left_root_data
        expect right_root: ByteArray = right_root_data
        expect result: builtin_witness.ValueWitnessV1 = result_data
        expect bound.arm == 37
        let arguments = [
          builtin_witness.BlsMillerLoopValue(left_root),
          builtin_witness.BlsMillerLoopValue(right_root),
        ]
        let Pair(root, count) = builtin_witness.arguments_root_v1(arguments)
        expect
          bound.pre.focus_root == proof.hash_builtin_value_v1(
            70,
            0,
            count,
            root,
          )
        let budget = builtin_witness.direct_builtin_budget_v1(70, arguments)
        expect
          bound.post == machine.exact_state(
            bound.pre,
            machine.mode_return,
            builtin_witness.result_root_v1(result),
            proof.empty_environment_root_v1,
            bound.pre.continuation_root,
            0,
            budget.cpu,
            budget.memory,
          )
        chain.continue_core(
          final_script_hash,
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
