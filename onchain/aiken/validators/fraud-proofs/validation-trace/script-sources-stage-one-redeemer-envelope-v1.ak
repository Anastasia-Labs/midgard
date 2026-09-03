use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_sources_redeemer_normalization_v1 as normalization
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/shared.{structural_transition_is_valid}
use midgard/validation_resolution_v1
use midgard/validation_semantic_v1.{Datum}
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  BindEnvelope {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
    expected_next_item_control_hash: ByteArray,
    action_family: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  deployment_id: ByteArray,
  traversal_normalizer_script_hash: ScriptHash,
  outer_normalizer_script_hash: ScriptHash,
  fold_map_executor_script_hash: ScriptHash,
  finalize_frame_executor_script_hash: ScriptHash,
  settlement_script_hash: ScriptHash,
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
      ct.Continue(BindEnvelope {
        input_index,
        output_index,
        transition,
        auxiliary,
        expected_next_item_control_hash,
        action_family,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let transition_data: Data = transition
        continue(
          computation_thread_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          fn(
            input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(base) = input_state
            let semantic_executor_script_hash = if action_family == normalization.fold_map_family {
              fold_map_executor_script_hash
            } else {
              finalize_frame_executor_script_hash
            }
            when
              normalization.verify_raw_envelope_v1(
                base.resolution.pre_state,
                transition,
                auxiliary,
                expected_next_item_control_hash,
                action_family,
              )
            is {
              None -> False
              Some(facts) -> {
                let resolution_identity = normalization.resolution_identity_v1(
                  base,
                )
                let envelope_commitment = normalization.envelope_commitment_v1(
                  deployment_id,
                  base.evidence_hash,
                  resolution_identity,
                  action_family,
                  facts.canonical_auxiliary_hash,
                  facts.canonical_action_hash,
                  facts.current_pending_item_control_hash,
                  expected_next_item_control_hash,
                  facts.redeemer_count,
                  facts.redeemer_total_count,
                  input_script_hash,
                  traversal_normalizer_script_hash,
                  outer_normalizer_script_hash,
                  semantic_executor_script_hash,
                  settlement_script_hash,
                  base.resolution.pre_state.transaction_commitment,
                  base.resolution.pre_state.validation_context_hash,
                )
                let expected_output_state: Data = normalization.PreparedScriptSourcesRedeemerEnvelopeV1 {
                  version: normalization.version,
                  domain: normalization.envelope_domain,
                  deployment_id,
                  base,
                  resolution_identity,
                  action_family,
                  canonical_auxiliary_hash: facts.canonical_auxiliary_hash,
                  canonical_action_hash: facts.canonical_action_hash,
                  current_pending_item_control_hash: facts.current_pending_item_control_hash,
                  expected_next_item_control_hash,
                  redeemer_count: facts.redeemer_count,
                  redeemer_total_count: facts.redeemer_total_count,
                  envelope_binder_script_hash: input_script_hash,
                  traversal_normalizer_script_hash,
                  outer_normalizer_script_hash,
                  semantic_executor_script_hash,
                  settlement_script_hash,
                  envelope_commitment,
                }
                and {
                  validation_resolution_v1.prepared_resolution_is_well_formed(
                    base,
                  ),
                  base.resolution.pre_state.phase == ScriptSources,
                  validation_resolution_v1.hash_one_step_evidence(
                    transition_data,
                    auxiliary,
                  ) == base.evidence_hash,
                  structural_transition_is_valid(
                    base.resolution.pre_state,
                    transition,
                  ),
                  bytearray.length(deployment_id) == 32,
                  bytearray.length(input_script_hash) == 28,
                  bytearray.length(traversal_normalizer_script_hash) == 28,
                  bytearray.length(outer_normalizer_script_hash) == 28,
                  bytearray.length(fold_map_executor_script_hash) == 28,
                  bytearray.length(finalize_frame_executor_script_hash) == 28,
                  bytearray.length(settlement_script_hash) == 28,
                  output_script_hash == traversal_normalizer_script_hash,
                  output_state == expected_output_state,
                }
              }
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
