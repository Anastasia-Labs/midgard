use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_machine_v1.{
  TransactionFieldChunkWitness, ValidationOneStepWitnessV1,
  verify_script_sources_stage_zero_begin_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  VerifyBegin {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    field_index: Int,
    item_index: Int,
    carriage: FieldCarriageV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(VerifyBegin {
        input_index,
        output_index,
        transition,
        field_index,
        item_index,
        carriage,
      }) -> {
        let auxiliary = TransactionFieldChunkWitness {
          field_index,
          item_index,
          carriage,
        }
        let auxiliary_data: Data = auxiliary
        let door = MachineFieldDoorV1 {
          reference_inputs: tx.reference_inputs,
          certificate_policy_id: field_preimage_certificate_policy_id,
        }
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_script_sources_stage_zero_begin_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            door,
            field_index,
            item_index,
            carriage,
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
