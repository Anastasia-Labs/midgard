use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/script_sources_raw_frame_v1 as raw
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_script_sources_yield_v1 as source_yield
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_semantic_yield_v1
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  VerifyNonOutput {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
    yield_role_index: Int,
    yield_ref_input_index: Int,
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
      ct.Continue(VerifyNonOutput {
        input_index,
        output_index,
        transition,
        auxiliary,
        yield_role_index,
        yield_ref_input_index,
      }) -> {
        let frame = raw.open_frame_v1(
          datum |> semantic_pre_state,
          transition,
          30,
          source_yield.middle_stage(yield_role_index),
        )
        let yield_hash = validation_semantic_yield_v1.require_semantic_yield_v1(
          tx,
          reference_script_auth_policy_id,
          source_yield.middle_role(yield_role_index),
          yield_ref_input_index,
        )
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary,
          frame.item_count == 30 && bytearray.length(yield_hash) == 28,
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
