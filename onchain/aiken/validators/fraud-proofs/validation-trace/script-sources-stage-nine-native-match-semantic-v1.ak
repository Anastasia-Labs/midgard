use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine/machine_types.{
  ScriptSourceScanWitness, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_late_stages.{
  verify_script_sources_stage_nine_native_match_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  VerifyNativeMatch {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    source_index: Int,
    origin_kind: Int,
    source_key: ByteArray,
    script_hash: ByteArray,
    script_total_length: Int,
    script_item_commitment: ByteArray,
    siblings: List<ByteArray>,
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
      ct.Continue(VerifyNativeMatch {
        input_index,
        output_index,
        transition,
        source_index,
        origin_kind,
        source_key,
        script_hash,
        script_total_length,
        script_item_commitment,
        siblings,
      }) -> {
        let auxiliary = ScriptSourceScanWitness {
          source_index,
          origin_kind,
          source_key,
          script_language_tag: 0,
          script_hash,
          script_total_length,
          script_item_commitment,
          siblings,
        }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_script_sources_stage_nine_native_match_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            source_index,
            origin_kind,
            source_key,
            script_hash,
            script_total_length,
            script_item_commitment,
            siblings,
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
