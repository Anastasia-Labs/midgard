use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/fraud_proofs/validation_trace/phase_a_native_item_yield_v1.{
  SpendRedeemer, VerifyItem,
}
use midgard/fraud_proofs/validation_trace/phase_a_native_item_yield_v1 as item_yield
use midgard/state_queue_yield.{require_authenticated_zero_yield}
use midgard/validation_machine/machine_types.{TransactionFieldChunkWitness}
use midgard/validation_machine/phase_a_native_scripts.{
  verify_phase_a_native_item_dispatch_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{PhaseANativeScripts}

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
      ct.Continue(VerifyItem {
        input_index,
        output_index,
        transition,
        field_index,
        item_index,
        carriage,
        yield_to_ref_input_index,
        yield_kind,
      }) -> {
        let auxiliary = TransactionFieldChunkWitness {
          field_index,
          item_index,
          carriage,
        }
        let auxiliary_data: Data = auxiliary
        expect yield_kind == 0 || yield_kind == 1
        expect _yield_hash = require_authenticated_zero_yield(
          tx.reference_inputs,
          tx.withdrawals,
          tx.redeemers,
          reference_script_auth_policy_id,
          if yield_kind == 0 {
            item_yield.native_role
          } else {
            item_yield.foreign_role
          },
          yield_to_ref_input_index,
        )
        continue_winning(
          PhaseANativeScripts,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_phase_a_native_item_dispatch_v1(
            datum |> semantic_pre_state,
            transition,
            field_index,
            item_index,
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
