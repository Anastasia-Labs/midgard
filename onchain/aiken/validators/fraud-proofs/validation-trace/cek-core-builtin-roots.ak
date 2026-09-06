use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1 as builtin_witness
use midgard/cek_cost_v1 as cost
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}

validator main(
  budget_script_hash: ScriptHash,
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
        expect
          if bound.arm == 30 {
            bound.group == 3 || bound.group == 4
          } else {
            bound.group == 5
          }
        expect bound.pre.mode == machine.mode_builtin
        let raw = chain.decode_builtin_success(bound.arm, action.witness)
        let item = if bound.arm == 31 {
          chain.BuiltinSuccess {
            ..raw,
            arguments: list.map(
              raw.arguments,
              builtin_witness.compact_semantic_value,
            ),
          }
        } else {
          raw
        }
        let Pair(arguments_root, arguments_count) = builtin_witness.arguments_root_v1(
          item.arguments,
        )
        let builtin_root = proof.hash_builtin_value_v1(
          item.tag,
          0,
          arguments_count,
          arguments_root,
        )
        expect arguments_count == cost.builtin_argument_count_v1(item.tag)
        expect bound.pre.focus_root == builtin_root
        let next_facts: Data = if bound.arm == 31 {
          let semantic: Data = chain.SemanticArguments {
            arguments_root,
            arguments_count,
            builtin_root,
            arguments: item.arguments,
          }
          semantic
        } else {
          let direct: Data = chain.BuiltinRoots {
            arguments_root,
            arguments_count,
            builtin_root,
            result_root: builtin_witness.result_root_v1(item.result),
          }
          direct
        }
        chain.continue_core(
          budget_script_hash,
          computation_thread_policy_id,
          step,
          action,
          0,
          next_facts,
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
