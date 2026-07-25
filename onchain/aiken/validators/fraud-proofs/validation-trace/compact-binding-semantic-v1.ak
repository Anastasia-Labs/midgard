use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine_v1.{
  NoAuxiliaryWitness, ValidationOneStepEvidenceV1, ValidationOneStepWitnessV1,
  verify_compact_binding_semantics_v1,
}
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_resolution_v1.PreparedValidationResolutionStateV1>

pub type VerifyCompactBindingActionV1 {
  VerifyCompactBinding {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<VerifyCompactBindingActionV1>

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
      ct.Continue(VerifyCompactBinding { input_index, output_index, transition }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let evidence = ValidationOneStepEvidenceV1 {
          transition,
          auxiliary: NoAuxiliaryWitness,
        }
        let transition_data: Data = transition
        let auxiliary_data: Data = NoAuxiliaryWitness
        continue(
          computation_thread_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          fn(
            _input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            let expected_output_state: Data = validation_resolution_v1.winning_resolution()
            and {
              validation_resolution_v1.prepared_resolution_is_well_formed(state),
              state.resolution.pre_state.phase == validation_trace_v1.CompactBinding,
              validation_resolution_v1.hash_one_step_evidence(
                transition_data,
                auxiliary_data,
              ) == state.evidence_hash,
              verify_compact_binding_semantics_v1(
                state.resolution.pre_state,
                evidence,
              ),
              output_script_hash == award_script_hash,
              output_state == expected_output_state,
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
