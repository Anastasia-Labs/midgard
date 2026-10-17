use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1 as builtin_witness
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
        expect bound.group == 5 && bound.arm == 31 && bound.pre.mode == 3
        let raw = chain.decode_builtin_success(bound.arm, action.witness)
        expect prior: chain.SemanticArguments = bound.facts
        let result = builtin_witness.compact_semantic_value(raw.result)
        let roots = chain.BuiltinRoots {
          arguments_root: prior.arguments_root,
          arguments_count: prior.arguments_count,
          builtin_root: prior.builtin_root,
          result_root: builtin_witness.result_root_v1(result),
        }
        let next_facts: Data = chain.SemanticRoots {
          roots,
          arguments: prior.arguments,
          result,
        }
        chain.continue_core(
          budget_script_hash,
          computation_thread_policy_id,
          step,
          action,
          1,
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
