use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/native_script_scan_v1.{NativeScriptFrameV1}
use midgard/validation_machine/machine_types.{
  NativeScriptFrameWitness, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/phase_a_native_scripts.{
  verify_phase_a_native_frame_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{PhaseANativeScripts}

pub type ActionV1 {
  VerifyFrame {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    frame: NativeScriptFrameV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
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
      ct.Continue(VerifyFrame { input_index, output_index, transition, frame }) -> {
        let auxiliary = NativeScriptFrameWitness { frame }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          PhaseANativeScripts,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_phase_a_native_frame_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            frame,
          ),
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

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
