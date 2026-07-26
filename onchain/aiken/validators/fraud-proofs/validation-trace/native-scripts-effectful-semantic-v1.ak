use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine_v1.{
  NativeExecutionDescriptorWitness, ValidationOneStepWitnessV1,
  verify_native_scripts_effectful_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{NativeScripts}

pub type ActionV1 {
  VerifyEffectful {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    execution_index: Int,
    language_tag: Int,
    purpose_kind: Int,
    purpose_index: Int,
    script_hash: ByteArray,
    subject: ByteArray,
    purpose_siblings: List<ByteArray>,
    source_index: Int,
    origin_kind: Int,
    source_key: ByteArray,
    script_total_length: Int,
    script_item_commitment: ByteArray,
    source_siblings: List<ByteArray>,
    redeemer_leaf: ByteArray,
    execution_siblings: List<ByteArray>,
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
      ct.Continue(VerifyEffectful {
        input_index,
        output_index,
        transition,
        execution_index,
        language_tag,
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
        purpose_siblings,
        source_index,
        origin_kind,
        source_key,
        script_total_length,
        script_item_commitment,
        source_siblings,
        redeemer_leaf,
        execution_siblings,
      }) -> {
        let auxiliary = NativeExecutionDescriptorWitness {
          execution_index,
          language_tag,
          purpose_kind,
          purpose_index,
          script_hash,
          subject,
          purpose_siblings,
          source_index,
          origin_kind,
          source_key,
          script_total_length,
          script_item_commitment,
          source_siblings,
          redeemer_leaf,
          execution_siblings,
          first_chunk_proof: None,
          signer_peaks: [],
        }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          NativeScripts,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_native_scripts_effectful_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            execution_index,
            language_tag,
            purpose_kind,
            purpose_index,
            script_hash,
            subject,
            purpose_siblings,
            source_index,
            origin_kind,
            source_key,
            script_total_length,
            script_item_commitment,
            source_siblings,
            redeemer_leaf,
            execution_siblings,
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
