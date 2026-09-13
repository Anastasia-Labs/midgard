use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine/machine_types.{CekWitnessControlV1}
use midgard/validation_machine/witness as witness
use midgard/validation_resolution_v1 as resolution
use midgard/validation_trace_v1 as vt

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.VerifiedDatum>,
    redeemer: chain.ContextRedeemer,
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
        expect Some(verified) = step.data
        expect Pair(0, [bound_data,
              _, _, _]) = builtin.un_constr_data(verified.staged)
        expect bound: chain.ContextBound = bound_data
        expect prepared: resolution.PreparedValidationResolutionStateV1 = bound.prepared
        expect cek: CekWitnessControlV1 = bound.control
        let transition: Data = action.transition
        expect chain.auxiliary_is_bound(bound, action.auxiliary)
        expect
          resolution.hash_one_step_evidence(transition, action.auxiliary) == prepared.evidence_hash
        let next_work = when verified.successor is {
          chain.ContinueContext { next } ->
            witness.encode_cek_witness_v1(
              cek.control,
              witness.encode_cek_context_control_v1(next),
              cek.execution_cursor,
              cek.completed_cpu,
              cek.completed_memory,
              #"",
              0,
              0,
              next.program_envelope_hash,
            )
          chain.StartExecution {
            initial_state_hash,
            execution_cpu_limit,
            execution_memory_limit,
          } ->
            witness.encode_cek_witness_v1(
              cek.control,
              #"",
              cek.execution_cursor,
              cek.completed_cpu,
              cek.completed_memory,
              initial_state_hash,
              execution_cpu_limit,
              execution_memory_limit,
              cek.program_envelope_hash,
            )
        }
        let pre = prepared.resolution.pre_state
        let post = action.transition.claimed_successor
        expect and {
            post.phase == vt.Cek,
            post.execution_cpu == pre.execution_cpu,
            post.execution_memory == pre.execution_memory,
            post.work_root == vt.hash_work_witness(
              vt.Cek,
              pre.program_counter + 1,
              next_work,
            ),
          }
        let award: Data = resolution.winning_resolution()
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) {
            hash == award_script_hash && state == award
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
