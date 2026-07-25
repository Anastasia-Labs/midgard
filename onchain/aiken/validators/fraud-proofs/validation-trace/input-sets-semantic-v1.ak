use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine_v1.{
  TransactionFieldPairPreimageWitness, ValidationOneStepEvidenceV1,
  ValidationOneStepWitnessV1, verify_input_sets_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{InputSets}

pub type ActionV1 {
  Verify {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    spend_inputs_cbor: ByteArray,
    reference_inputs_cbor: ByteArray,
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
        spend_inputs_cbor,
        reference_inputs_cbor,
      }) -> {
        let auxiliary = TransactionFieldPairPreimageWitness {
          first_field_index: 0,
          first_preimage_cbor: spend_inputs_cbor,
          second_field_index: 1,
          second_preimage_cbor: reference_inputs_cbor,
        }
        let auxiliary_data: Data = auxiliary
        let evidence = ValidationOneStepEvidenceV1 { transition, auxiliary }
        continue_winning(
          InputSets,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_input_sets_semantics_v1(datum |> semantic_pre_state, evidence),
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
