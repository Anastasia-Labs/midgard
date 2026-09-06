use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_machine_v1 as machine
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine/cek
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/witness as witness
use midgard/validation_resolution_v1 as resolution
use midgard/validation_semantic_v1.{Datum}
use midgard/validation_trace_v1.{Cek}

pub type ActionV1 {
  VerifyCoreStep {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    step: Data,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  arm_script_hashes: List<ScriptHash>,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
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
        expect Some(step_datum) = datum
        expect Some(prepared) = step_datum.data
        expect Pair(0, [pre_data,
              post_data, arm_witness]) = builtin.un_constr_data(action.step)
        expect pre: machine.MachineStateV1 = pre_data
        expect post: machine.MachineStateV1 = post_data
        let auxiliary = builtin.constr_data(12, [action.step])
        let transition_data: Data = action.transition
        let control = witness.cek_witness_control_v1(action.transition)
        expect and {
            resolution.prepared_resolution_is_well_formed(prepared),
            prepared.resolution.pre_state.phase == Cek,
            resolution.hash_one_step_evidence(transition_data, auxiliary) == prepared.evidence_hash,
            witness.cek_witness_is_well_formed_v1(
              prepared.resolution.pre_state,
              action.transition,
              control,
            ),
            witness.cek_control_is_core_step_v1(control),
            control.context_control_cbor == #"",
            cek.verify_cek_core_frame(
              prepared.resolution.pre_state,
              pre,
              post,
              control,
            ),
          }
        let group = chain.core_group(pre.mode, arm_witness)
        expect Some(next_hash) = list.at(arm_script_hashes, group)
        let prepared_data: Data = prepared
        let next: Data = chain.CoreBound {
          prepared: prepared_data,
          pre,
          post,
          witness_hash: blake2b_256(cbor.serialise(arm_witness)),
          arm: builtin.un_constr_data(arm_witness).1st,
          group,
          progress: 0,
          facts: builtin.constr_data(0, []),
        }
        continue(
          computation_thread_policy_id,
          step_datum,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, output_hash, output_state) {
            output_hash == next_hash && output_state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
