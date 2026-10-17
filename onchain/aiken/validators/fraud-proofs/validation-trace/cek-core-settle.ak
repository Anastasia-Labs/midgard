use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine/cek
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/witness
use midgard/validation_resolution_v1 as resolution

pub type Action {
  Settle {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    witness: Data,
  }
}

pub type Redeemer =
  ct.StepRedeemer<Action>

validator main(
  award_script_hash: ScriptHash,
  arm_hop_counts: List<Int>,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.CoreDatum>,
    redeemer: Redeemer,
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
        expect prepared: resolution.PreparedValidationResolutionStateV1 = bound.prepared
        expect Some(hops) = list.at(arm_hop_counts, bound.group)
        expect bound.progress == hops
        expect chain.witness_is_bound(bound, action.witness)
        let pre_data: Data = bound.pre
        let post_data: Data = bound.post
        let core_step = builtin.constr_data(
          0,
          [pre_data, post_data, action.witness],
        )
        let auxiliary = builtin.constr_data(12, [core_step])
        let transition_data: Data = action.transition
        expect
          resolution.hash_one_step_evidence(transition_data, auxiliary) == prepared.evidence_hash
        let control = witness.cek_witness_control_v1(action.transition)
        expect
          cek.verify_cek_core_successor(
            prepared.resolution.pre_state,
            action.transition,
            bound.post,
            control,
          )
        let award: Data = resolution.winning_resolution()
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, output_hash, output_state) {
            output_hash == award_script_hash && output_state == award
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
