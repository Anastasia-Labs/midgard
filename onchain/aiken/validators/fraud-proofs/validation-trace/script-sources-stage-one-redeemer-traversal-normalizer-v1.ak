use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_data_traverse_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1.{RedeemerItemProofControlV1}
use midgard/script_sources_redeemer_normalization_v1 as normalization

pub type Datum =
  ct.StepDatum<normalization.PreparedScriptSourcesRedeemerEnvelopeV1>

pub type ActionV1 {
  NormalizeTraversal {
    input_index: Int,
    output_index: Int,
    auxiliary: Data,
    current_item_control: RedeemerItemProofControlV1,
    traversal_action: cek_data_traverse_v1.DataTraverseActionV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  deployment_id: ByteArray,
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
      ct.Continue(NormalizeTraversal {
        input_index,
        output_index,
        auxiliary,
        current_item_control,
        traversal_action,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let current_item_control_data: Data = current_item_control
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
            expect Some(traversal) = current_item_control.traversal
            let checked_traversal_control_cbor = cek_data_traverse_v1.encode_control_v1(
              traversal,
            )
            let unvalidated_outer_fields = normalization.UnvalidatedRedeemerItemOuterFieldsV1 {
              version: current_item_control.version,
              mode: current_item_control.mode,
              stage: current_item_control.stage,
              item_index: current_item_control.item_index,
              item_count: current_item_control.item_count,
              total_length: current_item_control.total_length,
              item_commitment: current_item_control.item_commitment,
              expected_purpose_tag: current_item_control.expected_purpose_tag,
              expected_pointer_index: current_item_control.expected_pointer_index,
              purpose_tag: current_item_control.purpose_tag,
              pointer_index: current_item_control.pointer_index,
              data_offset: current_item_control.data_offset,
              data_length: current_item_control.data_length,
              execution_memory: current_item_control.execution_memory,
              execution_steps: current_item_control.execution_steps,
            }
            let expected_output_state: Data = normalization.TraversalNormalizedScriptSourcesRedeemerActionV1 {
              version: normalization.version,
              domain: normalization.traversal_normalized_domain,
              deployment_id,
              base_provenance_identity: normalization.base_provenance_identity_v1(
                state,
              ),
              envelope_binder_script_hash: state.envelope_binder_script_hash,
              traversal_normalizer_script_hash: state.traversal_normalizer_script_hash,
              outer_normalizer_script_hash: state.outer_normalizer_script_hash,
              semantic_executor_script_hash: state.semantic_executor_script_hash,
              settlement_script_hash: state.settlement_script_hash,
              action_family: state.action_family,
              canonical_action_hash: state.canonical_action_hash,
              authenticated_traversal_action_identity: normalization.traversal_action_identity_v1(
                traversal_action,
              ),
              current_pending_item_control_hash: state.current_pending_item_control_hash,
              expected_next_item_control_hash: state.expected_next_item_control_hash,
              redeemer_count: state.redeemer_count,
              redeemer_total_count: state.redeemer_total_count,
              unvalidated_outer_fields,
              validated_traversal_control: traversal,
              checked_traversal_control_cbor,
            }
            and {
              normalization.envelope_state_is_bound_v1(
                state,
                deployment_id,
                state.envelope_binder_script_hash,
              ),
              bytearray.length(deployment_id) == 32,
              normalization.traversal_normalizer_route_is_exact_v1(
                state,
                input_script_hash,
                output_script_hash,
              ),
              normalization.canonical_auxiliary_hash_v1(auxiliary) == state.canonical_auxiliary_hash,
              normalization.canonical_action_hash_v1(
                auxiliary,
                state.action_family,
              ) == state.canonical_action_hash,
              normalization.narrow_action_is_bound_v1(
                current_item_control_data,
                traversal_action,
                state.action_family,
                state.canonical_action_hash,
              ),
              cek_data_traverse_v1.control_is_well_formed(traversal),
              traversal.stage == cek_data_traverse_v1.stage_fold,
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
