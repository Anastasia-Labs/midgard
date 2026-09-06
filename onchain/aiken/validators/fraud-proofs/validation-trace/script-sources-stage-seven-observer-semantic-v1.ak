use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/validation_machine/machine_types.{
  TransactionFieldChunkWitness, ValidationOneStepWitnessV1,
}
use midgard/validation_script_sources_yield_v1 as yields
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_semantic_yield_v1
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  VerifyObserver {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    field_index: Int,
    item_index: Int,
    carriage: FieldCarriageV1,
    claimed_observer_hash: ByteArray,
    claimed_active_count: Int,
    item_yield_ref_input_index: Int,
    bound_yield_ref_input_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
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
      ct.Continue(VerifyObserver {
        input_index,
        output_index,
        transition,
        field_index,
        item_index,
        carriage,
        claimed_observer_hash,
        claimed_active_count,
        item_yield_ref_input_index,
        bound_yield_ref_input_index,
      }) -> {
        let auxiliary_data: Data = TransactionFieldChunkWitness {
          field_index,
          item_index,
          carriage,
        }
        let item_hash = validation_semantic_yield_v1.require_semantic_yield_v1(
          tx,
          reference_script_auth_policy_id,
          yields.observer_item_role,
          item_yield_ref_input_index,
        )
        let bound_hash = validation_semantic_yield_v1.require_semantic_yield_v1(
          tx,
          reference_script_auth_policy_id,
          yields.observer_bound_role,
          bound_yield_ref_input_index,
        )
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          bytearray.length(item_hash) == 28 && bytearray.length(bound_hash) == 28 && claimed_active_count > 0 && bytearray.length(
            claimed_observer_hash,
          ) == 28,
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
