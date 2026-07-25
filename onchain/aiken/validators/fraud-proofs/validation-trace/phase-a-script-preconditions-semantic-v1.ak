use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine_v1.{
  TransactionFieldPairPreimageWitness, ValidationOneStepEvidenceV1,
  ValidationOneStepWitnessV1, verify_phase_a_script_preconditions_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{PhaseAScriptPreconditions}

pub type ActionV1 {
  Verify {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    observers_cbor: ByteArray,
    redeemers_cbor: ByteArray,
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
      ct.Continue(Verify {
        input_index,
        output_index,
        transition,
        observers_cbor,
        redeemers_cbor,
      }) -> {
        let auxiliary = TransactionFieldPairPreimageWitness {
          first_field_index: 3,
          first_preimage_cbor: observers_cbor,
          second_field_index: 8,
          second_preimage_cbor: redeemers_cbor,
        }
        let auxiliary_data: Data = auxiliary
        let evidence = ValidationOneStepEvidenceV1 { transition, auxiliary }
        continue_winning(
          PhaseAScriptPreconditions,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_phase_a_script_preconditions_semantics_v1(
            datum |> semantic_pre_state,
            evidence,
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
