use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1
use midgard/script_sources_redeemer_normalization_v1 as normalization

pub type Datum =
  ct.StepDatum<normalization.TraversalNormalizedScriptSourcesRedeemerActionV1>

pub type ActionV1 {
  NormalizeOuter { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  deployment_id: ByteArray,
  expected_envelope_binder_script_hash: ScriptHash,
  expected_traversal_normalizer_script_hash: ScriptHash,
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
      ct.Continue(NormalizeOuter { input_index, output_index }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
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
            expect Some(state) = input_state
            let outer = state.unvalidated_outer_fields
            let current_item_control = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
              version: outer.version,
              mode: outer.mode,
              stage: outer.stage,
              item_index: outer.item_index,
              item_count: outer.item_count,
              total_length: outer.total_length,
              item_commitment: outer.item_commitment,
              expected_purpose_tag: outer.expected_purpose_tag,
              expected_pointer_index: outer.expected_pointer_index,
              purpose_tag: outer.purpose_tag,
              pointer_index: outer.pointer_index,
              data_offset: outer.data_offset,
              data_length: outer.data_length,
              execution_memory: outer.execution_memory,
              execution_steps: outer.execution_steps,
              traversal: Some(state.validated_traversal_control),
            }
            let next_item_control_hash_prefix_cbor = redeemer_item_proof_v1.stage_data_some_traversal_hash_prefix_v1(
              current_item_control,
            )
            let traversal_serialization_template = normalization.traversal_serialization_template_v1(
              state.validated_traversal_control,
              state.action_family,
            )
            let expected_output_state: Data = normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
              version: normalization.version,
              domain: normalization.outer_normalized_domain,
              deployment_id,
              base_provenance_identity: state.base_provenance_identity,
              envelope_binder_script_hash: state.envelope_binder_script_hash,
              traversal_normalizer_script_hash: state.traversal_normalizer_script_hash,
              outer_normalizer_script_hash: state.outer_normalizer_script_hash,
              semantic_executor_script_hash: state.semantic_executor_script_hash,
              settlement_script_hash: state.settlement_script_hash,
              action_family: state.action_family,
              canonical_action_hash: state.canonical_action_hash,
              authenticated_traversal_action_identity: state.authenticated_traversal_action_identity,
              current_pending_item_control_hash: state.current_pending_item_control_hash,
              expected_next_item_control_hash: state.expected_next_item_control_hash,
              redeemer_count: state.redeemer_count,
              redeemer_total_count: state.redeemer_total_count,
              next_item_control_hash_prefix_cbor,
              validated_traversal_control: state.validated_traversal_control,
              traversal_serialization_template,
            }
            and {
              normalization.traversal_normalized_state_is_bound_v1(
                state,
                deployment_id,
                expected_envelope_binder_script_hash,
                expected_traversal_normalizer_script_hash,
              ),
              bytearray.length(deployment_id) == 32,
              bytearray.length(expected_envelope_binder_script_hash) == 28,
              bytearray.length(expected_traversal_normalizer_script_hash) == 28,
              normalization.outer_normalizer_route_is_exact_v1(
                state,
                input_script_hash,
                output_script_hash,
              ),
              redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
                current_item_control,
              ),
              current_item_control.item_index == state.redeemer_count,
              current_item_control.item_count == state.redeemer_total_count,
              redeemer_item_proof_v1.hash_stage_data_outer_with_checked_traversal_v1(
                current_item_control,
                state.checked_traversal_control_cbor,
              ) == state.current_pending_item_control_hash,
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
