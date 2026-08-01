use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_data_traverse_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/redeemer_item_proof_v1
use midgard/script_sources_redeemer_normalization_v1 as normalization

pub type Datum =
  ct.StepDatum<normalization.OuterNormalizedScriptSourcesRedeemerActionV1>

pub type ActionV1 {
  ExecuteFinalizeFrame {
    input_index: Int,
    output_index: Int,
    traversal_action: cek_data_traverse_v1.DataTraverseActionV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  deployment_id: ByteArray,
  expected_envelope_binder_script_hash: ScriptHash,
  expected_traversal_normalizer_script_hash: ScriptHash,
  expected_outer_normalizer_script_hash: ScriptHash,
  expected_settlement_script_hash: ScriptHash,
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
      ct.Continue(ExecuteFinalizeFrame {
        input_index,
        output_index,
        traversal_action,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        expect cek_data_traverse_v1.FinalizeFrame { frame, parent } =
          traversal_action
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
            when
              cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
                state.validated_traversal_control.frame_root,
                state.validated_traversal_control.offset,
                state.validated_traversal_control.source_length,
                frame,
                parent,
              )
            is {
              None -> False
              Some(transition) -> {
                expect normalization.FinalizeFrameTemplate {
                  prefix_before_stage_cbor,
                  source_fields_before_frame_root_cbor,
                  suffix_before_result_cbor,
                } = state.traversal_serialization_template
                let checked_next_traversal_control_cbor = prefix_before_stage_cbor
                  |> bytearray.concat(cbor.serialise(transition.next_stage))
                  |> bytearray.concat(source_fields_before_frame_root_cbor)
                  |> bytearray.concat(
                      encode_definite_bytes(transition.next_frame_root),
                    )
                  |> bytearray.concat(suffix_before_result_cbor)
                  |> bytearray.concat(
                      cek_data_traverse_v1.encode_optional_summary_v1(
                        transition.next_result,
                      ),
                    )
                let actual_next_item_control_hash = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
                  state.next_item_control_hash_prefix_cbor,
                  checked_next_traversal_control_cbor,
                )
                let expected_output_state: Data = normalization.execution_attested_state_v1(
                  state,
                  actual_next_item_control_hash,
                )
                and {
                  bytearray.length(deployment_id) == 32,
                  bytearray.length(expected_envelope_binder_script_hash) == 28,
                  bytearray.length(expected_traversal_normalizer_script_hash)
                    == 28,
                  bytearray.length(expected_outer_normalizer_script_hash) == 28,
                  bytearray.length(expected_settlement_script_hash) == 28,
                  normalization.outer_normalized_state_is_bound_v1(
                    state,
                    normalization.finalize_frame_family,
                    deployment_id,
                    expected_envelope_binder_script_hash,
                    expected_traversal_normalizer_script_hash,
                    expected_outer_normalizer_script_hash,
                    input_script_hash,
                    expected_settlement_script_hash,
                  ),
                  normalization.semantic_executor_route_is_exact_v1(
                    state,
                    normalization.finalize_frame_family,
                    input_script_hash,
                    output_script_hash,
                    expected_settlement_script_hash,
                  ),
                  normalization.traversal_action_identity_is_bound_v1(
                    traversal_action,
                    normalization.finalize_frame_family,
                    state.authenticated_traversal_action_identity,
                  ),
                  state.validated_traversal_control.stage
                    == cek_data_traverse_v1.stage_fold,
                  actual_next_item_control_hash
                    == state.expected_next_item_control_hash,
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
