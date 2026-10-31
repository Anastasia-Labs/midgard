use aiken/builtin
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
  kinds_script_hash: ScriptHash,
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
        expect bound.group == 10
        expect bound.pre.mode == machine.mode_builtin
        expect Pair(39, [tag_data,
              arguments_data]) = builtin.un_constr_data(action.witness)
        expect tag: Int = tag_data
        expect arguments: List<builtin_witness.RuntimeValueWitnessV1> = arguments_data
        expect bound.arm == 39
        expect
          builtin_witness.revealed_runtime_payload_bytes_v1(arguments) <= builtin_witness.max_direct_builtin_revealed_payload_bytes
        let Pair(root, count) = builtin_witness.runtime_arguments_root_v1(
          arguments,
        )
        expect count == cost.builtin_argument_count_v1(tag)
        expect
          bound.pre.focus_root == proof.hash_builtin_value_v1(
            tag,
            0,
            count,
            root,
          )
        expect
          bound.post == machine.exact_state(
            bound.pre,
            machine.mode_halt_error,
            proof.hash_error_term_v1(),
            proof.empty_environment_root_v1,
            proof.empty_continuation_root_v1,
            machine.error_builtin_failure,
            0,
            0,
          )
        chain.continue_core(
          kinds_script_hash,
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
