use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine/cek.{verify_cek_execution_selection_semantics_v1}
use midgard/validation_machine/machine_types.{
  ValidationAuxiliaryWitnessV1, ValidationOneStepEvidenceV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_resolver_v1.{CekMaterialRouteV1, verify_cek_route_v1}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{Cek}

pub type ActionV1 {
  VerifyExecutionSelection {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: ValidationAuxiliaryWitnessV1,
    material_route: CekMaterialRouteV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  cek_program_material_script_hash: ScriptHash,
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
      ct.Continue(VerifyExecutionSelection {
        input_index,
        output_index,
        transition,
        auxiliary,
        material_route,
      }) -> {
        let auxiliary_data: Data = auxiliary
        let evidence = ValidationOneStepEvidenceV1 { transition, auxiliary }
        continue_winning(
          Cek,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          and {
            verify_cek_route_v1(
              evidence,
              material_route,
              tx.reference_inputs,
              cek_program_material_script_hash,
            ),
            verify_cek_execution_selection_semantics_v1(
              datum |> semantic_pre_state,
              transition,
              auxiliary,
            ),
          },
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
