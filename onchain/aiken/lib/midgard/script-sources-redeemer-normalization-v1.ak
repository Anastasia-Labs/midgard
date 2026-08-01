use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_traverse_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/validation_machine_v1.{ValidationOneStepWitnessV1}
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub const version: Int = 1

pub const fold_map_family: Int = 0

pub const finalize_frame_family: Int = 1

pub const envelope_domain: ByteArray =
  #"4d696467617264536372697074536f757263657352656465656d6572456e76656c6f70655631"

pub const traversal_normalized_domain: ByteArray =
  #"4d696467617264536372697074536f757263657354726176657273616c4e6f726d616c697a65645631"

pub const outer_normalized_domain: ByteArray =
  #"4d696467617264536372697074536f75726365734f757465724e6f726d616c697a65645631"

pub const execution_attested_domain: ByteArray =
  #"4d696467617264536372697074536f757263657352656465656d6572457865637574696f6e41747465737465645631"

const resolution_identity_domain: ByteArray =
  #"4d696467617264536372697074536f75726365735265736f6c7574696f6e4964656e746974795631"

const auxiliary_identity_domain: ByteArray =
  #"4d696467617264536372697074536f7572636573417578696c696172794964656e746974795631"

const action_identity_domain: ByteArray =
  #"4d696467617264536372697074536f75726365734e6172726f77416374696f6e4964656e746974795631"

const traversal_action_identity_domain: ByteArray =
  #"4d696467617264536372697074536f757263657354726176657273616c416374696f6e4964656e746974795631"

const envelope_commitment_domain: ByteArray =
  #"4d696467617264536372697074536f757263657352656465656d6572456e76656c6f7065436f6d6d69746d656e745631"

const base_provenance_identity_domain: ByteArray =
  #"4d696467617264536372697074536f75726365734261736550726f76656e616e63654964656e746974795631"

pub type ScriptSourcesRedeemerEnvelopeFactsV1 {
  current_pending_item_control_hash: ByteArray,
  redeemer_count: Int,
  redeemer_total_count: Int,
  canonical_auxiliary_hash: ByteArray,
  canonical_action_hash: ByteArray,
}

pub type PreparedScriptSourcesRedeemerEnvelopeV1 {
  version: Int,
  domain: ByteArray,
  deployment_id: ByteArray,
  base: validation_resolution_v1.PreparedValidationResolutionStateV1,
  resolution_identity: ByteArray,
  action_family: Int,
  canonical_auxiliary_hash: ByteArray,
  canonical_action_hash: ByteArray,
  current_pending_item_control_hash: ByteArray,
  expected_next_item_control_hash: ByteArray,
  redeemer_count: Int,
  redeemer_total_count: Int,
  envelope_binder_script_hash: ScriptHash,
  traversal_normalizer_script_hash: ScriptHash,
  outer_normalizer_script_hash: ScriptHash,
  semantic_executor_script_hash: ScriptHash,
  settlement_script_hash: ScriptHash,
  envelope_commitment: ByteArray,
}

/// The first fifteen outer item-control fields. Stage 1 authenticates their
/// origin and action binding but deliberately does not trust their semantics.
pub type UnvalidatedRedeemerItemOuterFieldsV1 {
  version: Int,
  mode: Int,
  stage: Int,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  expected_purpose_tag: Int,
  expected_pointer_index: Int,
  purpose_tag: Int,
  pointer_index: Int,
  data_offset: Int,
  data_length: Int,
  execution_memory: Int,
  execution_steps: Int,
}

pub type TraversalNormalizedScriptSourcesRedeemerActionV1 {
  version: Int,
  domain: ByteArray,
  deployment_id: ByteArray,
  base_provenance_identity: ByteArray,
  envelope_binder_script_hash: ScriptHash,
  traversal_normalizer_script_hash: ScriptHash,
  outer_normalizer_script_hash: ScriptHash,
  semantic_executor_script_hash: ScriptHash,
  settlement_script_hash: ScriptHash,
  action_family: Int,
  canonical_action_hash: ByteArray,
  authenticated_traversal_action_identity: ByteArray,
  current_pending_item_control_hash: ByteArray,
  expected_next_item_control_hash: ByteArray,
  redeemer_count: Int,
  redeemer_total_count: Int,
  unvalidated_outer_fields: UnvalidatedRedeemerItemOuterFieldsV1,
  validated_traversal_control: cek_data_traverse_v1.DataTraverseControlV1,
  checked_traversal_control_cbor: ByteArray,
}

pub type FamilyTraversalSerializationTemplateV1 {
  FoldMapFrameRootTemplate {
    prefix_before_frame_root_cbor: ByteArray,
    suffix_after_frame_root_cbor: ByteArray,
  }
  FinalizeFrameTemplate {
    prefix_before_stage_cbor: ByteArray,
    source_fields_before_frame_root_cbor: ByteArray,
    suffix_before_result_cbor: ByteArray,
  }
}

pub type OuterNormalizedScriptSourcesRedeemerActionV1 {
  version: Int,
  domain: ByteArray,
  deployment_id: ByteArray,
  base_provenance_identity: ByteArray,
  envelope_binder_script_hash: ScriptHash,
  traversal_normalizer_script_hash: ScriptHash,
  outer_normalizer_script_hash: ScriptHash,
  semantic_executor_script_hash: ScriptHash,
  settlement_script_hash: ScriptHash,
  action_family: Int,
  canonical_action_hash: ByteArray,
  authenticated_traversal_action_identity: ByteArray,
  current_pending_item_control_hash: ByteArray,
  expected_next_item_control_hash: ByteArray,
  redeemer_count: Int,
  redeemer_total_count: Int,
  next_item_control_hash_prefix_cbor: ByteArray,
  validated_traversal_control: cek_data_traverse_v1.DataTraverseControlV1,
  traversal_serialization_template: FamilyTraversalSerializationTemplateV1,
}

pub type ScriptSourcesRedeemerExecutionAttestedStateV1 {
  version: Int,
  domain: ByteArray,
  deployment_id: ByteArray,
  base_provenance_identity: ByteArray,
  envelope_binder_script_hash: ScriptHash,
  traversal_normalizer_script_hash: ScriptHash,
  outer_normalizer_script_hash: ScriptHash,
  semantic_executor_script_hash: ScriptHash,
  settlement_script_hash: ScriptHash,
  action_family: Int,
  canonical_action_hash: ByteArray,
  authenticated_traversal_action_identity: ByteArray,
  current_pending_item_control_hash: ByteArray,
  expected_next_item_control_hash: ByteArray,
  actual_next_item_control_hash: ByteArray,
  redeemer_count: Int,
  redeemer_total_count: Int,
}

fn data_is_none(data: Data) -> Bool {
  let Pair(index, fields) = builtin.un_constr_data(data)
  and {
    index == 1,
    fields == [],
  }
}

fn raw_common_control_is_initial(
  resolved_item_peaks_data: Data,
  source_count_data: Data,
  replay_cursor_data: Data,
  spend_index_data: Data,
  purpose_count_data: Data,
  output_cursor_data: Data,
  output_count_data: Data,
  receive_scan_data: Data,
  source_total_count_data: Data,
) -> Bool {
  expect
      [
        receive_source_count_data, receive_source_peaks_data, receive_count_data,
        receive_previous_hash_data, receive_candidate_hash_data,
        receive_descriptor_peaks_data,
      ]
    = builtin.un_list_data(receive_scan_data)
  and {
    builtin.un_i_data(source_total_count_data) == builtin.un_i_data(
      source_count_data,
    ),
    builtin.un_list_data(resolved_item_peaks_data) == [],
    builtin.un_i_data(replay_cursor_data) == 0,
    builtin.un_i_data(spend_index_data) == 0,
    builtin.un_i_data(purpose_count_data) == 0,
    builtin.un_i_data(output_cursor_data) == 0,
    builtin.un_i_data(output_count_data) == 0,
    builtin.un_i_data(receive_source_count_data) == 0,
    builtin.un_list_data(receive_source_peaks_data) == [],
    builtin.un_i_data(receive_count_data) == 0,
    builtin.un_b_data(receive_previous_hash_data) == #"",
    builtin.un_b_data(receive_candidate_hash_data) == #"",
    builtin.un_list_data(receive_descriptor_peaks_data) == [],
  }
}

pub fn narrow_preimage_hash_v1(
  current_item_control_data: Data,
  traversal_action_data: Data,
) -> ByteArray {
  let narrow_preimage: Data = [current_item_control_data, traversal_action_data]
  action_identity_domain
    |> bytearray.concat(cbor.serialise(narrow_preimage))
    |> blake2b_256
}

pub fn canonical_auxiliary_hash_v1(auxiliary: Data) -> ByteArray {
  auxiliary_identity_domain
    |> bytearray.concat(cbor.serialise(auxiliary))
    |> blake2b_256
}

pub fn canonical_action_hash_v1(
  auxiliary: Data,
  action_family: Int,
) -> ByteArray {
  let Pair(auxiliary_tag, auxiliary_fields) = builtin.un_constr_data(auxiliary)
  expect [redeemer_control_data,
        current_item_control_data, item_witness_data] = auxiliary_fields
  let Pair(item_witness_tag, item_witness_fields) = builtin.un_constr_data(
    item_witness_data,
  )
  expect [item_action_data,
        chunk_proof_data, next_chunk_proof_data] = item_witness_fields
  let Pair(item_action_tag, item_action_fields) = builtin.un_constr_data(
    item_action_data,
  )
  expect [traversal_action_data] = item_action_fields
  let Pair(traversal_action_tag, traversal_action_fields) = builtin.un_constr_data(
    traversal_action_data,
  )
  let action_shape_is_exact = if action_family == fold_map_family {
    and {
      traversal_action_tag == 7,
      list.length(traversal_action_fields) == 6,
    }
  } else {
    and {
      action_family == finalize_frame_family,
      traversal_action_tag == 8,
      list.length(traversal_action_fields) == 2,
    }
  }
  expect and {
      // `validation_machine_v1.RedeemerItemStepWitness` -- the only
      // `ValidationAuxiliaryWitnessV1` constructor carrying
      // `(Option<CekRedeemerContextControlV1>, RedeemerItemProofControlV1,
      // RedeemerItemProofWitnessV1)`, and the one the machine itself matches for
      // this step (validation-machine-v1.ak:8613, :11150). It is declared 19th
      // in that enum (validation-machine-v1.ak:210), so its constructor index is
      // 18. The literal here was 22, which no genuine auxiliary can ever carry;
      // it was masked by the raw-envelope route trapping earlier. Pinned by
      // `raw_envelope_auxiliary_constructor_index_is_the_machine_witness`.
      auxiliary_tag == 18,
      item_witness_tag == 0,
      item_action_tag == 2,
      data_is_none(redeemer_control_data),
      data_is_none(chunk_proof_data),
      data_is_none(next_chunk_proof_data),
      action_shape_is_exact,
    }
  narrow_preimage_hash_v1(current_item_control_data, traversal_action_data)
}

pub fn action_matches_family_v1(
  action: cek_data_traverse_v1.DataTraverseActionV1,
  action_family: Int,
) -> Bool {
  when action is {
    cek_data_traverse_v1.FoldMap { .. } -> action_family == fold_map_family
    cek_data_traverse_v1.FinalizeFrame { .. } ->
      action_family == finalize_frame_family
    _ -> False
  }
}

pub fn narrow_action_is_bound_v1(
  current_item_control_data: Data,
  traversal_action: cek_data_traverse_v1.DataTraverseActionV1,
  expected_action_family: Int,
  expected_action_hash: ByteArray,
) -> Bool {
  let traversal_action_data: Data = traversal_action
  and {
    action_matches_family_v1(traversal_action, expected_action_family),
    narrow_preimage_hash_v1(current_item_control_data, traversal_action_data) == expected_action_hash,
  }
}

pub fn traversal_action_identity_v1(
  traversal_action: cek_data_traverse_v1.DataTraverseActionV1,
) -> ByteArray {
  let traversal_action_data: Data = traversal_action
  traversal_action_identity_domain
    |> bytearray.concat(cbor.serialise(traversal_action_data))
    |> blake2b_256
}

pub fn traversal_action_identity_is_bound_v1(
  traversal_action: cek_data_traverse_v1.DataTraverseActionV1,
  expected_action_family: Int,
  expected_traversal_action_identity: ByteArray,
) -> Bool {
  and {
    action_matches_family_v1(traversal_action, expected_action_family),
    bytearray.length(expected_traversal_action_identity) == 32,
    traversal_action_identity_v1(traversal_action) == expected_traversal_action_identity,
  }
}

pub fn verify_raw_envelope_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: Data,
  expected_next_item_control_hash: ByteArray,
  action_family: Int,
) -> Option<ScriptSourcesRedeemerEnvelopeFactsV1> {
  expect Some(control_data) = cbor.deserialise(transition.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolved_input_count_data, resolved_inputs_accumulator_data,
        signer_count_data, signer_frontier_commitment_data,
        resolved_item_peaks_data, stage_data, source_count_data,
        source_peaks_data, redeemer_count_data, redeemer_peaks_data,
        replay_cursor_data, replay_accumulator_data,
        replay_remaining_schedule_hash_data, spend_index_data,
        purpose_count_data, purpose_peaks_data, output_cursor_data,
        output_count_data, output_peaks_data, output_total_count_data,
        receive_scan_data, source_total_count_data, redeemer_total_count_data,
        observer_scan_data, mint_fold_data, resolution_schedule_hash_data,
        pending_item_control_hash_data,
      ]
    = builtin.un_list_data(control_data)
  let work_witness_cbor = transition.work_witness_cbor
  let compact_cbor = builtin.un_b_data(compact_cbor_data)
  let witness_set_compact_cbor = builtin.un_b_data(
    witness_set_compact_cbor_data,
  )
  let field_preimage_lengths_cbor = builtin.un_b_data(
    field_preimage_lengths_cbor_data,
  )
  let context_cbor = builtin.un_b_data(context_cbor_data)
  let current_pending_item_control_hash = builtin.un_b_data(
    pending_item_control_hash_data,
  )
  // The bytes the machine actually commits for a stage-1 pending redeemer item
  // are the *canonical* ones produced by
  // `validation_machine_v1.encode_script_sources_redeemer_item_witness`
  // (validation-machine-v1.ak:7337, dispatched from
  // `exact_script_sources_control:7462`): a definite-length 31-item array,
  // `encode_definite_array_header(31)` = `98 1f`, holding the thirty
  // `encode_script_sources_witness` base items followed by
  // `encode_definite_bytes(<32-byte pending item control hash>)`.
  //
  // The previous reconstruction rebuilt those bytes with
  // `cbor.serialise(<Data list of the thirty base items>)`. `cbor.serialise` is
  // Plutus `serialiseData`, which emits the INDEFINITE array header `9f` (159)
  // for any non-empty list and chunks every bytestring longer than 64 bytes as
  // `5f..ff`. Feeding that to `decode_definite_array_header_at` hit the
  // decoder's final branch, `expect tag == 154` (codec.ak:334), so the raw
  // envelope route trapped on *every* input; the companion conjunct
  // `cbor.serialise(control_data) == transition.work_witness_cbor` demanded the
  // same non-canonical encoding and could never hold either.
  //
  // The successor witness differs from this step's witness only in that final
  // 34-byte item, so the shared canonical prefix is recovered by dropping it
  // from the hash-pinned witness rather than by re-serialising decoded `Data`:
  // `structural_transition_is_valid` (validation-machine-v1.ak:479-483) binds
  // `transition.work_witness_cbor` to `pre.work_root`, and the tail/head checks
  // in `valid` below pin the canonical framing that `serialiseData` cannot
  // produce.
  let pending_prefix = builtin.slice_bytearray(
    0,
    bytearray.length(work_witness_cbor) - 34,
    work_witness_cbor,
  )
  // Rebuilt exactly as `encode_script_sources_witness` builds its first eight
  // items (validation-machine-v1.ak:1062-1078), only with the 31-item header
  // that `encode_script_sources_redeemer_item_witness` substitutes. These eight
  // carry every bytestring long enough for `serialiseData` to chunk, so an
  // indefinite-form witness fails here as well as at the array header.
  let canonical_prefix_head = encode_definite_array_header(31)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(
        cbor.serialise(builtin.un_i_data(resolved_input_count_data)),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          builtin.un_b_data(resolved_inputs_accumulator_data),
        ),
      )
    |> bytearray.concat(cbor.serialise(builtin.un_i_data(signer_count_data)))
    |> bytearray.concat(
        encode_definite_bytes(
          builtin.un_b_data(signer_frontier_commitment_data),
        ),
      )
  let expected_work_witness_cbor = pending_prefix
    |> bytearray.concat(
        encode_definite_bytes(current_pending_item_control_hash),
      )
  let next_work_witness_cbor = pending_prefix
    |> bytearray.concat(encode_definite_bytes(expected_next_item_control_hash))
  let redeemer_count = builtin.un_i_data(redeemer_count_data)
  let redeemer_total_count = builtin.un_i_data(redeemer_total_count_data)
  let canonical_auxiliary_hash = canonical_auxiliary_hash_v1(auxiliary)
  let canonical_action_hash = canonical_action_hash_v1(auxiliary, action_family)
  let valid = and {
    bytearray.length(current_pending_item_control_hash) == 32,
    bytearray.length(expected_next_item_control_hash) == 32,
    builtin.slice_bytearray(
      0,
      bytearray.length(canonical_prefix_head),
      pending_prefix,
    ) == canonical_prefix_head,
    work_witness_cbor == expected_work_witness_cbor,
    action_family == fold_map_family || action_family == finalize_frame_family,
    builtin.un_i_data(stage_data) == 1,
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    builtin.un_i_data(resolved_input_count_data) >= 0,
    bytearray.length(builtin.un_b_data(resolved_inputs_accumulator_data)) == 32,
    builtin.un_i_data(signer_count_data) >= 0,
    bytearray.length(builtin.un_b_data(signer_frontier_commitment_data)) == 32,
    bytearray.length(builtin.un_b_data(replay_accumulator_data)) == 32,
    bytearray.length(builtin.un_b_data(replay_remaining_schedule_hash_data)) == 32,
    builtin.un_list_data(purpose_peaks_data) == [],
    builtin.un_list_data(output_peaks_data) == [],
    builtin.un_i_data(output_total_count_data) == 0,
    bytearray.length(builtin.un_b_data(resolution_schedule_hash_data)) == 32,
    raw_common_control_is_initial(
      resolved_item_peaks_data,
      source_count_data,
      replay_cursor_data,
      spend_index_data,
      purpose_count_data,
      output_cursor_data,
      output_count_data,
      receive_scan_data,
      source_total_count_data,
    ),
    redeemer_count >= 0,
    redeemer_total_count > redeemer_count,
    redeemer_total_count <= 16_384,
    transition.claimed_successor.phase == validation_trace_v1.ScriptSources,
    transition.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_work_witness_cbor,
    ),
  }
  if valid {
    Some(
      ScriptSourcesRedeemerEnvelopeFactsV1 {
        current_pending_item_control_hash,
        redeemer_count,
        redeemer_total_count,
        canonical_auxiliary_hash,
        canonical_action_hash,
      },
    )
  } else {
    None
  }
}

pub fn resolution_identity_v1(
  base: validation_resolution_v1.PreparedValidationResolutionStateV1,
) -> ByteArray {
  let resolution_data: Data = base.resolution
  resolution_identity_domain
    |> bytearray.concat(cbor.serialise(resolution_data))
    |> blake2b_256
}

pub fn envelope_commitment_v1(
  deployment_id: ByteArray,
  base_evidence_hash: ByteArray,
  resolution_identity: ByteArray,
  action_family: Int,
  canonical_auxiliary_hash: ByteArray,
  canonical_action_hash: ByteArray,
  current_pending_item_control_hash: ByteArray,
  expected_next_item_control_hash: ByteArray,
  redeemer_count: Int,
  redeemer_total_count: Int,
  envelope_binder_script_hash: ScriptHash,
  traversal_normalizer_script_hash: ScriptHash,
  outer_normalizer_script_hash: ScriptHash,
  semantic_executor_script_hash: ScriptHash,
  settlement_script_hash: ScriptHash,
  transaction_commitment: ByteArray,
  validation_context_hash: ByteArray,
) -> ByteArray {
  envelope_commitment_domain
    |> bytearray.concat(encode_definite_array_header(18))
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(encode_definite_bytes(deployment_id))
    |> bytearray.concat(encode_definite_bytes(base_evidence_hash))
    |> bytearray.concat(encode_definite_bytes(resolution_identity))
    |> bytearray.concat(cbor.serialise(action_family))
    |> bytearray.concat(encode_definite_bytes(canonical_auxiliary_hash))
    |> bytearray.concat(encode_definite_bytes(canonical_action_hash))
    |> bytearray.concat(
        encode_definite_bytes(current_pending_item_control_hash),
      )
    |> bytearray.concat(encode_definite_bytes(expected_next_item_control_hash))
    |> bytearray.concat(cbor.serialise(redeemer_count))
    |> bytearray.concat(cbor.serialise(redeemer_total_count))
    |> bytearray.concat(encode_definite_bytes(envelope_binder_script_hash))
    |> bytearray.concat(encode_definite_bytes(traversal_normalizer_script_hash))
    |> bytearray.concat(encode_definite_bytes(outer_normalizer_script_hash))
    |> bytearray.concat(encode_definite_bytes(semantic_executor_script_hash))
    |> bytearray.concat(encode_definite_bytes(settlement_script_hash))
    |> bytearray.concat(encode_definite_bytes(transaction_commitment))
    |> bytearray.concat(encode_definite_bytes(validation_context_hash))
    |> blake2b_256
}

pub fn envelope_state_is_bound_v1(
  state: PreparedScriptSourcesRedeemerEnvelopeV1,
  expected_deployment_id: ByteArray,
  expected_binder_script_hash: ScriptHash,
) -> Bool {
  and {
    state.version == version,
    state.domain == envelope_domain,
    bytearray.length(state.deployment_id) == 32,
    state.deployment_id == expected_deployment_id,
    state.base.version == validation_resolution_v1.prepared_resolution_version,
    bytearray.length(state.base.evidence_hash) == 32,
    state.resolution_identity == resolution_identity_v1(state.base),
    state.action_family == fold_map_family || state.action_family == finalize_frame_family,
    bytearray.length(state.canonical_auxiliary_hash) == 32,
    bytearray.length(state.canonical_action_hash) == 32,
    bytearray.length(state.current_pending_item_control_hash) == 32,
    bytearray.length(state.expected_next_item_control_hash) == 32,
    state.redeemer_count >= 0,
    state.redeemer_total_count > state.redeemer_count,
    state.redeemer_total_count <= 16_384,
    bytearray.length(state.envelope_binder_script_hash) == 28,
    state.envelope_binder_script_hash == expected_binder_script_hash,
    bytearray.length(state.traversal_normalizer_script_hash) == 28,
    bytearray.length(state.outer_normalizer_script_hash) == 28,
    bytearray.length(state.semantic_executor_script_hash) == 28,
    bytearray.length(state.settlement_script_hash) == 28,
    bytearray.length(state.envelope_commitment) == 32,
    state.envelope_commitment == envelope_commitment_v1(
      state.deployment_id,
      state.base.evidence_hash,
      state.resolution_identity,
      state.action_family,
      state.canonical_auxiliary_hash,
      state.canonical_action_hash,
      state.current_pending_item_control_hash,
      state.expected_next_item_control_hash,
      state.redeemer_count,
      state.redeemer_total_count,
      state.envelope_binder_script_hash,
      state.traversal_normalizer_script_hash,
      state.outer_normalizer_script_hash,
      state.semantic_executor_script_hash,
      state.settlement_script_hash,
      state.base.resolution.pre_state.transaction_commitment,
      state.base.resolution.pre_state.validation_context_hash,
    ),
  }
}

pub fn base_provenance_identity_v1(
  state: PreparedScriptSourcesRedeemerEnvelopeV1,
) -> ByteArray {
  base_provenance_identity_domain
    |> bytearray.concat(encode_definite_array_header(3))
    |> bytearray.concat(encode_definite_bytes(state.base.evidence_hash))
    |> bytearray.concat(encode_definite_bytes(state.resolution_identity))
    |> bytearray.concat(encode_definite_bytes(state.envelope_commitment))
    |> blake2b_256
}

pub fn traversal_normalized_state_is_bound_v1(
  state: TraversalNormalizedScriptSourcesRedeemerActionV1,
  expected_deployment_id: ByteArray,
  expected_binder_script_hash: ScriptHash,
  expected_traversal_normalizer_script_hash: ScriptHash,
) -> Bool {
  and {
    state.version == version,
    state.domain == traversal_normalized_domain,
    bytearray.length(state.deployment_id) == 32,
    state.deployment_id == expected_deployment_id,
    bytearray.length(state.base_provenance_identity) == 32,
    state.envelope_binder_script_hash == expected_binder_script_hash,
    state.traversal_normalizer_script_hash == expected_traversal_normalizer_script_hash,
    bytearray.length(state.outer_normalizer_script_hash) == 28,
    bytearray.length(state.semantic_executor_script_hash) == 28,
    bytearray.length(state.settlement_script_hash) == 28,
    state.action_family == fold_map_family || state.action_family == finalize_frame_family,
    bytearray.length(state.canonical_action_hash) == 32,
    bytearray.length(state.authenticated_traversal_action_identity) == 32,
    bytearray.length(state.current_pending_item_control_hash) == 32,
    bytearray.length(state.expected_next_item_control_hash) == 32,
    state.redeemer_count >= 0,
    state.redeemer_total_count > state.redeemer_count,
    state.redeemer_total_count <= 16_384,
    bytearray.length(state.checked_traversal_control_cbor) > 0,
  }
}

pub fn traversal_normalizer_route_is_exact_v1(
  state: PreparedScriptSourcesRedeemerEnvelopeV1,
  observed_input_script_hash: ScriptHash,
  observed_output_script_hash: ScriptHash,
) -> Bool {
  and {
    observed_input_script_hash == state.traversal_normalizer_script_hash,
    observed_output_script_hash == state.outer_normalizer_script_hash,
  }
}

pub fn outer_normalizer_route_is_exact_v1(
  state: TraversalNormalizedScriptSourcesRedeemerActionV1,
  observed_input_script_hash: ScriptHash,
  observed_output_script_hash: ScriptHash,
) -> Bool {
  and {
    observed_input_script_hash == state.outer_normalizer_script_hash,
    observed_output_script_hash == state.semantic_executor_script_hash,
  }
}

pub fn outer_normalized_state_is_bound_v1(
  state: OuterNormalizedScriptSourcesRedeemerActionV1,
  expected_action_family: Int,
  expected_deployment_id: ByteArray,
  expected_envelope_binder_script_hash: ScriptHash,
  expected_traversal_normalizer_script_hash: ScriptHash,
  expected_outer_normalizer_script_hash: ScriptHash,
  observed_semantic_executor_script_hash: ScriptHash,
  expected_settlement_script_hash: ScriptHash,
) -> Bool {
  and {
    state.version == version,
    state.domain == outer_normalized_domain,
    bytearray.length(state.deployment_id) == 32,
    state.deployment_id == expected_deployment_id,
    bytearray.length(state.base_provenance_identity) == 32,
    bytearray.length(state.envelope_binder_script_hash) == 28,
    state.envelope_binder_script_hash == expected_envelope_binder_script_hash,
    bytearray.length(state.traversal_normalizer_script_hash) == 28,
    state.traversal_normalizer_script_hash == expected_traversal_normalizer_script_hash,
    bytearray.length(state.outer_normalizer_script_hash) == 28,
    state.outer_normalizer_script_hash == expected_outer_normalizer_script_hash,
    bytearray.length(state.semantic_executor_script_hash) == 28,
    state.semantic_executor_script_hash == observed_semantic_executor_script_hash,
    bytearray.length(state.settlement_script_hash) == 28,
    state.settlement_script_hash == expected_settlement_script_hash,
    or {
      expected_action_family == fold_map_family,
      expected_action_family == finalize_frame_family,
    },
    state.action_family == expected_action_family,
    bytearray.length(state.canonical_action_hash) == 32,
    bytearray.length(state.authenticated_traversal_action_identity) == 32,
    bytearray.length(state.current_pending_item_control_hash) == 32,
    bytearray.length(state.expected_next_item_control_hash) == 32,
    state.redeemer_count >= 0,
    state.redeemer_total_count > state.redeemer_count,
    state.redeemer_total_count <= 16_384,
    bytearray.length(state.next_item_control_hash_prefix_cbor) > 0,
    state.validated_traversal_control.stage == cek_data_traverse_v1.stage_fold,
    state.traversal_serialization_template == traversal_serialization_template_v1(
      state.validated_traversal_control,
      expected_action_family,
    ),
  }
}

pub fn semantic_executor_route_is_exact_v1(
  state: OuterNormalizedScriptSourcesRedeemerActionV1,
  expected_action_family: Int,
  observed_input_script_hash: ScriptHash,
  observed_output_script_hash: ScriptHash,
  expected_settlement_script_hash: ScriptHash,
) -> Bool {
  and {
    state.action_family == expected_action_family,
    observed_input_script_hash == state.semantic_executor_script_hash,
    state.settlement_script_hash == expected_settlement_script_hash,
    observed_output_script_hash == state.settlement_script_hash,
  }
}

pub fn execution_attestation_is_bound_to_envelope_v1(
  state: ScriptSourcesRedeemerExecutionAttestedStateV1,
  envelope: PreparedScriptSourcesRedeemerEnvelopeV1,
  expected_deployment_id: ByteArray,
  expected_envelope_binder_script_hash: ScriptHash,
  expected_traversal_normalizer_script_hash: ScriptHash,
  expected_outer_normalizer_script_hash: ScriptHash,
  expected_fold_map_executor_script_hash: ScriptHash,
  expected_finalize_frame_executor_script_hash: ScriptHash,
  observed_settlement_script_hash: ScriptHash,
) -> Bool {
  let selected_executor_is_exact = if state.action_family == fold_map_family {
    and {
      state.semantic_executor_script_hash == expected_fold_map_executor_script_hash,
      envelope.semantic_executor_script_hash == expected_fold_map_executor_script_hash,
    }
  } else if state.action_family == finalize_frame_family {
    and {
      state.semantic_executor_script_hash == expected_finalize_frame_executor_script_hash,
      envelope.semantic_executor_script_hash == expected_finalize_frame_executor_script_hash,
    }
  } else {
    False
  }
  and {
    state.version == version,
    state.domain == execution_attested_domain,
    bytearray.length(state.deployment_id) == 32,
    state.deployment_id == expected_deployment_id,
    envelope.deployment_id == expected_deployment_id,
    validation_resolution_v1.prepared_resolution_is_well_formed(envelope.base),
    envelope_state_is_bound_v1(
      envelope,
      expected_deployment_id,
      expected_envelope_binder_script_hash,
    ),
    bytearray.length(state.base_provenance_identity) == 32,
    state.base_provenance_identity == base_provenance_identity_v1(envelope),
    bytearray.length(state.envelope_binder_script_hash) == 28,
    state.envelope_binder_script_hash == expected_envelope_binder_script_hash,
    envelope.envelope_binder_script_hash == expected_envelope_binder_script_hash,
    bytearray.length(state.traversal_normalizer_script_hash) == 28,
    state.traversal_normalizer_script_hash == expected_traversal_normalizer_script_hash,
    envelope.traversal_normalizer_script_hash == expected_traversal_normalizer_script_hash,
    bytearray.length(state.outer_normalizer_script_hash) == 28,
    state.outer_normalizer_script_hash == expected_outer_normalizer_script_hash,
    envelope.outer_normalizer_script_hash == expected_outer_normalizer_script_hash,
    bytearray.length(state.semantic_executor_script_hash) == 28,
    selected_executor_is_exact,
    bytearray.length(state.settlement_script_hash) == 28,
    state.settlement_script_hash == observed_settlement_script_hash,
    envelope.settlement_script_hash == observed_settlement_script_hash,
    state.action_family == envelope.action_family,
    bytearray.length(state.canonical_action_hash) == 32,
    state.canonical_action_hash == envelope.canonical_action_hash,
    bytearray.length(state.authenticated_traversal_action_identity) == 32,
    bytearray.length(state.current_pending_item_control_hash) == 32,
    state.current_pending_item_control_hash == envelope.current_pending_item_control_hash,
    bytearray.length(state.expected_next_item_control_hash) == 32,
    state.expected_next_item_control_hash == envelope.expected_next_item_control_hash,
    bytearray.length(state.actual_next_item_control_hash) == 32,
    state.actual_next_item_control_hash == state.expected_next_item_control_hash,
    state.redeemer_count == envelope.redeemer_count,
    state.redeemer_total_count == envelope.redeemer_total_count,
  }
}

pub fn execution_attestation_settlement_is_exact_v1(
  state: ScriptSourcesRedeemerExecutionAttestedStateV1,
  envelope: PreparedScriptSourcesRedeemerEnvelopeV1,
  expected_deployment_id: ByteArray,
  expected_envelope_binder_script_hash: ScriptHash,
  expected_traversal_normalizer_script_hash: ScriptHash,
  expected_outer_normalizer_script_hash: ScriptHash,
  expected_fold_map_executor_script_hash: ScriptHash,
  expected_finalize_frame_executor_script_hash: ScriptHash,
  observed_settlement_script_hash: ScriptHash,
  expected_award_script_hash: ScriptHash,
  observed_output_script_hash: ScriptHash,
  output_state: Data,
) -> Bool {
  let expected_output_state: Data = validation_resolution_v1.winning_resolution()
  and {
    bytearray.length(expected_award_script_hash) == 28,
    execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      expected_deployment_id,
      expected_envelope_binder_script_hash,
      expected_traversal_normalizer_script_hash,
      expected_outer_normalizer_script_hash,
      expected_fold_map_executor_script_hash,
      expected_finalize_frame_executor_script_hash,
      observed_settlement_script_hash,
    ),
    observed_output_script_hash == expected_award_script_hash,
    output_state == expected_output_state,
  }
}

pub fn execution_attested_state_v1(
  state: OuterNormalizedScriptSourcesRedeemerActionV1,
  actual_next_item_control_hash: ByteArray,
) -> ScriptSourcesRedeemerExecutionAttestedStateV1 {
  ScriptSourcesRedeemerExecutionAttestedStateV1 {
    version,
    domain: execution_attested_domain,
    deployment_id: state.deployment_id,
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
    actual_next_item_control_hash,
    redeemer_count: state.redeemer_count,
    redeemer_total_count: state.redeemer_total_count,
  }
}

pub fn traversal_serialization_template_v1(
  traversal: cek_data_traverse_v1.DataTraverseControlV1,
  action_family: Int,
) -> FamilyTraversalSerializationTemplateV1 {
  if action_family == fold_map_family {
    FoldMapFrameRootTemplate {
      prefix_before_frame_root_cbor: encode_definite_array_header(10)
        |> bytearray.concat(cbor.serialise(traversal.version))
        |> bytearray.concat(cbor.serialise(traversal.stage))
        |> bytearray.concat(cbor.serialise(traversal.source_start))
        |> bytearray.concat(cbor.serialise(traversal.source_length))
        |> bytearray.concat(cbor.serialise(traversal.offset)),
      suffix_after_frame_root_cbor: #"d87a80d87a80d87a80d87a80",
    }
  } else {
    expect action_family == finalize_frame_family
    FinalizeFrameTemplate {
      prefix_before_stage_cbor: encode_definite_array_header(10)
        |> bytearray.concat(cbor.serialise(traversal.version)),
      source_fields_before_frame_root_cbor: cbor.serialise(
        traversal.source_start,
      )
        |> bytearray.concat(cbor.serialise(traversal.source_length))
        |> bytearray.concat(cbor.serialise(traversal.offset)),
      suffix_before_result_cbor: #"d87a80d87a80d87a80",
    }
  }
}
