use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_machine_v1.{CoreStepEvidenceV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine_v1.{
  CekCoreStepWitness, ValidationOneStepWitnessV1,
  verify_cek_core_step_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{Cek}

pub type ActionV1 {
  VerifyCoreStep {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    step: CoreStepEvidenceV1,
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
      ct.Continue(VerifyCoreStep { input_index, output_index, transition, step }) -> {
        let auxiliary = CekCoreStepWitness { step }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          Cek,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_cek_core_step_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            step,
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
