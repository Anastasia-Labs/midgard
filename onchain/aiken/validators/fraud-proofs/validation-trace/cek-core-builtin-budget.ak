use aiken/collection/list
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
  semantics_script_hashes: List<ScriptHash>,
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
        let item = if bound.arm == 31 {
          chain.semantic_item(bound, action.witness)
        } else {
          chain.decode_builtin_success(bound.arm, action.witness)
        }
        let facts = if bound.arm == 31 {
          expect semantic: chain.SemanticRoots = bound.facts
          semantic.roots
        } else {
          expect direct: chain.BuiltinRoots = bound.facts
          direct
        }
        let budget = builtin_witness.direct_builtin_budget_v1(
          item.tag,
          item.arguments,
        )
        expect
          bound.post == machine.exact_state(
            bound.pre,
            machine.mode_return,
            facts.result_root,
            proof.empty_environment_root_v1,
            bound.pre.continuation_root,
            0,
            budget.cpu,
            budget.memory,
          )
        let group = if bound.arm == 31 {
          chain.semantic_group(item.tag)
        } else {
          0
        }
        expect Some(semantics_script_hash) = list.at(
          semantics_script_hashes,
          group,
        )
        let next_facts = bound.facts
        chain.continue_core(
          semantics_script_hash,
          computation_thread_policy_id,
          step,
          action,
          if bound.arm == 31 {
            2
          } else {
            1
          },
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
