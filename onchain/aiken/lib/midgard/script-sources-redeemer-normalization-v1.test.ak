use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_frame_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact
use midgard/redeemer_item_proof_v1
use midgard/script_sources_redeemer_normalization_v1 as normalization
use midgard/validation_machine_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

const hash_a =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const hash_b =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const script_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const script_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const script_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const script_d = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const script_e = #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const script_f = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const script_g = #"12121212121212121212121212121212121212121212121212121212"

const zero =
  #"0000000000000000000000000000000000000000000000000000000000000000"

fn fold_control() -> cek_data_traverse_v1.DataTraverseControlV1 {
  cek_data_traverse_v1.DataTraverseControlV1 {
    version: cek_data_traverse_v1.version,
    stage: cek_data_traverse_v1.stage_fold,
    source_start: 4,
    source_length: 10,
    offset: 10,
    frame_root: hash_a,
    pending_large_expected_children: None,
    integer: None,
    bytes: None,
    result: None,
  }
}

fn finalize_control() -> cek_data_traverse_v1.DataTraverseControlV1 {
  cek_data_traverse_v1.DataTraverseControlV1 {
    version: cek_data_traverse_v1.version,
    stage: cek_data_traverse_v1.stage_fold,
    source_start: 6,
    source_length: 12,
    offset: 12,
    frame_root: hash_b,
    pending_large_expected_children: None,
    integer: None,
    bytes: None,
    result: None,
  }
}

fn outer_control(
  traversal: cek_data_traverse_v1.DataTraverseControlV1,
) -> redeemer_item_proof_v1.RedeemerItemProofControlV1 {
  redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    version: redeemer_item_proof_v1.version,
    mode: redeemer_item_proof_v1.mode_data,
    stage: redeemer_item_proof_v1.stage_data,
    item_index: 0,
    item_count: 1,
    total_length: traversal.source_start + traversal.source_length + 6,
    item_commitment: hash_a,
    expected_purpose_tag: 0,
    expected_pointer_index: 0,
    purpose_tag: 0,
    pointer_index: 0,
    data_offset: traversal.source_start,
    data_length: traversal.source_length,
    execution_memory: 10,
    execution_steps: 20,
    traversal: Some(traversal),
  }
}

fn unvalidated_outer_fields(
  control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
) -> normalization.UnvalidatedRedeemerItemOuterFieldsV1 {
  normalization.UnvalidatedRedeemerItemOuterFieldsV1 {
    version: control.version,
    mode: control.mode,
    stage: control.stage,
    item_index: control.item_index,
    item_count: control.item_count,
    total_length: control.total_length,
    item_commitment: control.item_commitment,
    expected_purpose_tag: control.expected_purpose_tag,
    expected_pointer_index: control.expected_pointer_index,
    purpose_tag: control.purpose_tag,
    pointer_index: control.pointer_index,
    data_offset: control.data_offset,
    data_length: control.data_length,
    execution_memory: control.execution_memory,
    execution_steps: control.execution_steps,
  }
}

fn traversal_normalized_state(
  control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
) -> normalization.TraversalNormalizedScriptSourcesRedeemerActionV1 {
  expect Some(traversal) = control.traversal
  normalization.TraversalNormalizedScriptSourcesRedeemerActionV1 {
    version: normalization.version,
    domain: normalization.traversal_normalized_domain,
    deployment_id: hash_a,
    base_provenance_identity: hash_b,
    envelope_binder_script_hash: script_a,
    traversal_normalizer_script_hash: script_b,
    outer_normalizer_script_hash: script_c,
    semantic_executor_script_hash: script_d,
    settlement_script_hash: script_e,
    action_family: normalization.fold_map_family,
    canonical_action_hash: hash_a,
    authenticated_traversal_action_identity: hash_b,
    current_pending_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      control,
    ),
    expected_next_item_control_hash: hash_b,
    redeemer_count: 0,
    redeemer_total_count: 1,
    unvalidated_outer_fields: unvalidated_outer_fields(control),
    validated_traversal_control: traversal,
    checked_traversal_control_cbor: cek_data_traverse_v1.encode_control_v1(
      traversal,
    ),
  }
}

fn example_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root: hash_a, cbor_length: 1, memory: 4 }
}

fn fold_map_action() -> cek_data_traverse_v1.DataTraverseActionV1 {
  cek_data_traverse_v1.FoldMap {
    frame: cek_data_frame_v1.initial_map_frame_v1(#"", 0),
    pair_index: 0,
    key: example_summary(),
    value: example_summary(),
    key_siblings: [],
    value_siblings: [],
  }
}

fn finalize_frame_action() -> cek_data_traverse_v1.DataTraverseActionV1 {
  cek_data_traverse_v1.FinalizeFrame {
    frame: cek_data_frame_v1.initial_map_frame_v1(#"", 0),
    parent: None,
  }
}

fn map_key_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 {
    root: hash_a,
    cbor_length: 1,
    memory: 4,
  }
}

fn map_value_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 {
    root: hash_b,
    cbor_length: 2,
    memory: 5,
  }
}

fn nontrivial_map_frame() -> cek_data_frame_v1.DataFrameV1 {
  let initial = cek_data_frame_v1.initial_map_frame_v1(#"", 2)
  expect Some(with_key) = cek_data_frame_v1.append_child_v1(
    initial,
    map_key_summary(),
  )
  expect Some(with_pair) = cek_data_frame_v1.append_child_v1(
    with_key,
    map_value_summary(),
  )
  with_pair
}

fn nontrivial_fold_map_action() -> cek_data_traverse_v1.DataTraverseActionV1 {
  let frame = nontrivial_map_frame()
  let key = map_key_summary()
  let value = map_value_summary()
  cek_data_traverse_v1.FoldMap {
    frame,
    pair_index: 0,
    key,
    value,
    key_siblings: [cek_data_frame_v1.child_leaf_hash_v1(1, value)],
    value_siblings: [cek_data_frame_v1.child_leaf_hash_v1(0, key)],
  }
}

fn nontrivial_fold_map_control() -> cek_data_traverse_v1.DataTraverseControlV1 {
  let frame = nontrivial_map_frame()
  cek_data_traverse_v1.DataTraverseControlV1 {
    ..fold_control(),
    frame_root: cek_data_frame_v1.hash_frame_v1(frame),
  }
}

fn outer_normalized_fold_map_state() -> normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
  let traversal = nontrivial_fold_map_control()
  let control = outer_control(traversal)
  let action = nontrivial_fold_map_action()
  expect Some(next_traversal) = cek_data_traverse_v1.step_v1(
    traversal,
    None,
    action,
  )
  let next_control = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    ..control,
    traversal: Some(next_traversal),
  }
  normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    version: normalization.version,
    domain: normalization.outer_normalized_domain,
    deployment_id: hash_a,
    base_provenance_identity: hash_b,
    envelope_binder_script_hash: script_a,
    traversal_normalizer_script_hash: script_b,
    outer_normalizer_script_hash: script_c,
    semantic_executor_script_hash: script_d,
    settlement_script_hash: script_e,
    action_family: normalization.fold_map_family,
    canonical_action_hash: hash_a,
    authenticated_traversal_action_identity: normalization.traversal_action_identity_v1(
      action,
    ),
    current_pending_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      control,
    ),
    expected_next_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      next_control,
    ),
    redeemer_count: 0,
    redeemer_total_count: 1,
    next_item_control_hash_prefix_cbor: redeemer_item_proof_v1.stage_data_some_traversal_hash_prefix_v1(
      control,
    ),
    validated_traversal_control: traversal,
    traversal_serialization_template: normalization.traversal_serialization_template_v1(
      traversal,
      normalization.fold_map_family,
    ),
  }
}

fn terminal_finalize_frame() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.initial_list_frame_v1(#"", 0)
}

fn terminal_finalize_action() -> cek_data_traverse_v1.DataTraverseActionV1 {
  cek_data_traverse_v1.FinalizeFrame {
    frame: terminal_finalize_frame(),
    parent: None,
  }
}

fn terminal_finalize_control() -> cek_data_traverse_v1.DataTraverseControlV1 {
  let frame = terminal_finalize_frame()
  cek_data_traverse_v1.DataTraverseControlV1 {
    version: cek_data_traverse_v1.version,
    stage: cek_data_traverse_v1.stage_fold,
    source_start: 6,
    source_length: 12,
    offset: 12,
    frame_root: cek_data_frame_v1.hash_frame_v1(frame),
    pending_large_expected_children: None,
    integer: None,
    bytes: None,
    result: None,
  }
}

fn append_parent_frame() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.initial_list_frame_v1(#"", 2)
}

fn parent_append_finalize_frame() -> cek_data_frame_v1.DataFrameV1 {
  cek_data_frame_v1.initial_list_frame_v1(
    cek_data_frame_v1.hash_frame_v1(append_parent_frame()),
    0,
  )
}

fn parent_append_finalize_action() -> cek_data_traverse_v1.DataTraverseActionV1 {
  cek_data_traverse_v1.FinalizeFrame {
    frame: parent_append_finalize_frame(),
    parent: Some(append_parent_frame()),
  }
}

fn parent_append_finalize_control() -> cek_data_traverse_v1.DataTraverseControlV1 {
  let frame = parent_append_finalize_frame()
  cek_data_traverse_v1.DataTraverseControlV1 {
    version: cek_data_traverse_v1.version,
    stage: cek_data_traverse_v1.stage_fold,
    source_start: 6,
    source_length: 12,
    offset: 6,
    frame_root: cek_data_frame_v1.hash_frame_v1(frame),
    pending_large_expected_children: None,
    integer: None,
    bytes: None,
    result: None,
  }
}

fn outer_normalized_finalize_state(
  traversal: cek_data_traverse_v1.DataTraverseControlV1,
  action: cek_data_traverse_v1.DataTraverseActionV1,
) -> normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
  let control = outer_control(traversal)
  expect Some(next_traversal) = cek_data_traverse_v1.step_v1(
    traversal,
    None,
    action,
  )
  let next_control = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    ..control,
    traversal: Some(next_traversal),
  }
  normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    version: normalization.version,
    domain: normalization.outer_normalized_domain,
    deployment_id: hash_a,
    base_provenance_identity: hash_b,
    envelope_binder_script_hash: script_a,
    traversal_normalizer_script_hash: script_b,
    outer_normalizer_script_hash: script_c,
    semantic_executor_script_hash: script_f,
    settlement_script_hash: script_e,
    action_family: normalization.finalize_frame_family,
    canonical_action_hash: hash_a,
    authenticated_traversal_action_identity: normalization.traversal_action_identity_v1(
      action,
    ),
    current_pending_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      control,
    ),
    expected_next_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      next_control,
    ),
    redeemer_count: 0,
    redeemer_total_count: 1,
    next_item_control_hash_prefix_cbor: redeemer_item_proof_v1.stage_data_some_traversal_hash_prefix_v1(
      control,
    ),
    validated_traversal_control: traversal,
    traversal_serialization_template: normalization.traversal_serialization_template_v1(
      traversal,
      normalization.finalize_frame_family,
    ),
  }
}

fn terminal_finalize_state() -> normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
  outer_normalized_finalize_state(
    terminal_finalize_control(),
    terminal_finalize_action(),
  )
}

fn parent_append_finalize_state() -> normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
  outer_normalized_finalize_state(
    parent_append_finalize_control(),
    parent_append_finalize_action(),
  )
}

fn assemble_finalize_transition_cbor(
  state: normalization.OuterNormalizedScriptSourcesRedeemerActionV1,
  transition: cek_data_traverse_v1.FinalizeFrameTransitionV1,
) -> ByteArray {
  expect normalization.FinalizeFrameTemplate {
    prefix_before_stage_cbor,
    source_fields_before_frame_root_cbor,
    suffix_before_result_cbor,
  } = state.traversal_serialization_template
  prefix_before_stage_cbor
    |> bytearray.concat(cbor.serialise(transition.next_stage))
    |> bytearray.concat(source_fields_before_frame_root_cbor)
    |> bytearray.concat(encode_definite_bytes(transition.next_frame_root))
    |> bytearray.concat(suffix_before_result_cbor)
    |> bytearray.concat(
        cek_data_traverse_v1.encode_optional_summary_v1(
          transition.next_result,
        ),
      )
}

fn settlement_pre_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: hash_a,
    transaction_id: hash_b,
    transaction_commitment: hash_a,
    validation_context_hash: hash_b,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: hash_a,
    phase: validation_trace_v1.ScriptSources,
    program_counter: 1,
    work_root: hash_b,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: hash_a,
  }
}

fn settlement_prepared_resolution() -> validation_resolution_v1.PreparedValidationResolutionStateV1 {
  validation_resolution_v1.PreparedValidationResolutionStateV1 {
    version: validation_resolution_v1.prepared_resolution_version,
    resolution: validation_resolution_v1.ValidationResolutionStateV1 {
      version: validation_resolution_v1.resolution_version,
      pre_state: settlement_pre_state(),
      operator_successor_hash: hash_a,
      challenger_successor_hash: hash_b,
    },
    evidence_hash: hash_a,
  }
}

fn settlement_envelope(
  state: normalization.OuterNormalizedScriptSourcesRedeemerActionV1,
) -> normalization.PreparedScriptSourcesRedeemerEnvelopeV1 {
  let base = settlement_prepared_resolution()
  let resolution_identity = normalization.resolution_identity_v1(base)
  let envelope_commitment = normalization.envelope_commitment_v1(
    state.deployment_id,
    base.evidence_hash,
    resolution_identity,
    state.action_family,
    hash_b,
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
    base.resolution.pre_state.transaction_commitment,
    base.resolution.pre_state.validation_context_hash,
  )
  normalization.PreparedScriptSourcesRedeemerEnvelopeV1 {
    version: normalization.version,
    domain: normalization.envelope_domain,
    deployment_id: state.deployment_id,
    base,
    resolution_identity,
    action_family: state.action_family,
    canonical_auxiliary_hash: hash_b,
    canonical_action_hash: state.canonical_action_hash,
    current_pending_item_control_hash: state.current_pending_item_control_hash,
    expected_next_item_control_hash: state.expected_next_item_control_hash,
    redeemer_count: state.redeemer_count,
    redeemer_total_count: state.redeemer_total_count,
    envelope_binder_script_hash: state.envelope_binder_script_hash,
    traversal_normalizer_script_hash: state.traversal_normalizer_script_hash,
    outer_normalizer_script_hash: state.outer_normalizer_script_hash,
    semantic_executor_script_hash: state.semantic_executor_script_hash,
    settlement_script_hash: state.settlement_script_hash,
    envelope_commitment,
  }
}

fn settlement_attestation(
  state: normalization.OuterNormalizedScriptSourcesRedeemerActionV1,
) -> normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
  let envelope = settlement_envelope(state)
  let bound_state =
    normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
      ..state,
      base_provenance_identity: normalization.base_provenance_identity_v1(
        envelope,
      ),
    }
  normalization.execution_attested_state_v1(
    bound_state,
    bound_state.expected_next_item_control_hash,
  )
}

fn settlement_is_exact(
  state: normalization.ScriptSourcesRedeemerExecutionAttestedStateV1,
  envelope: normalization.PreparedScriptSourcesRedeemerEnvelopeV1,
  output_script_hash: ByteArray,
  output_state: Data,
) -> Bool {
  normalization.execution_attestation_settlement_is_exact_v1(
    state,
    envelope,
    hash_a,
    script_a,
    script_b,
    script_c,
    script_d,
    script_f,
    script_e,
    script_g,
    output_script_hash,
    output_state,
  )
}

test fold_map_two_hop_encoding_and_hash_equal_checked_generic_path() {
  let traversal = fold_control()
  let control = outer_control(traversal)
  let checked_traversal = cek_data_traverse_v1.encode_control_v1(traversal)
  let assembled_control = redeemer_item_proof_v1.stage_data_outer_control_prefix_v1(
    control,
  )
    |> bytearray.concat(#"d8799f")
    |> bytearray.concat(checked_traversal)
    |> bytearray.concat(#"ff")
  let authenticated_hash_prefix = redeemer_item_proof_v1.stage_data_some_traversal_hash_prefix_v1(
    control,
  )
  and {
    cek_data_traverse_v1.control_is_well_formed(traversal),
    redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(control),
    redeemer_item_proof_v1.control_is_well_formed(control),
    assembled_control == redeemer_item_proof_v1.encode_control_v1(control),
    redeemer_item_proof_v1.hash_stage_data_outer_with_checked_traversal_v1(
      control,
      checked_traversal,
    ) == redeemer_item_proof_v1.hash_control_v1(control),
    redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
      authenticated_hash_prefix,
      checked_traversal,
    ) == redeemer_item_proof_v1.hash_control_v1(control),
    redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
      bytearray.concat(authenticated_hash_prefix, #"00"),
      checked_traversal,
    ) != redeemer_item_proof_v1.hash_control_v1(control),
  }
}

test finalize_frame_two_hop_encoding_and_hash_equal_checked_generic_path() {
  let traversal = finalize_control()
  let control = outer_control(traversal)
  let checked_traversal = cek_data_traverse_v1.encode_control_v1(traversal)
  let assembled_control = redeemer_item_proof_v1.stage_data_outer_control_prefix_v1(
    control,
  )
    |> bytearray.concat(#"d8799f")
    |> bytearray.concat(checked_traversal)
    |> bytearray.concat(#"ff")
  let authenticated_hash_prefix = redeemer_item_proof_v1.stage_data_some_traversal_hash_prefix_v1(
    control,
  )
  and {
    cek_data_traverse_v1.control_is_well_formed(traversal),
    redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(control),
    redeemer_item_proof_v1.control_is_well_formed(control),
    assembled_control == redeemer_item_proof_v1.encode_control_v1(control),
    redeemer_item_proof_v1.hash_stage_data_outer_with_checked_traversal_v1(
      control,
      checked_traversal,
    ) == redeemer_item_proof_v1.hash_control_v1(control),
    redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
      authenticated_hash_prefix,
      checked_traversal,
    ) == redeemer_item_proof_v1.hash_control_v1(control),
    redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
      bytearray.concat(authenticated_hash_prefix, #"00"),
      checked_traversal,
    ) != redeemer_item_proof_v1.hash_control_v1(control),
  }
}

test traversal_hop_rejects_malformed_traversal() {
  let traversal = fold_control()
  and {
    !cek_data_traverse_v1.control_is_well_formed(
      cek_data_traverse_v1.DataTraverseControlV1 {
        ..traversal,
        frame_root: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      },
    ),
    !cek_data_traverse_v1.control_is_well_formed(
      cek_data_traverse_v1.DataTraverseControlV1 {
        ..traversal,
        stage: cek_data_traverse_v1.stage_head,
      },
    ),
  }
}

test outer_hop_rejects_source_stage_mode_index_and_count_mutations() {
  let control = outer_control(fold_control())
  and {
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        data_offset: control.data_offset + 1,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        stage: redeemer_item_proof_v1.stage_tail,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        mode: redeemer_item_proof_v1.mode_descriptor,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        item_index: -1,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        item_count: control.item_index,
      },
    ),
  }
}

test outer_hop_rejects_descriptor_and_execution_unit_mutations() {
  let control = outer_control(fold_control())
  and {
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        purpose_tag: 2,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        expected_pointer_index: 1,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        total_length: control.data_offset + control.data_length + 20,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        execution_memory: -1,
      },
    ),
    !redeemer_item_proof_v1.stage_data_outer_fields_are_well_formed_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..control,
        execution_steps: -1,
      },
    ),
  }
}

test outer_hop_rejects_wrong_checked_encoding_and_current_hash() {
  let traversal = fold_control()
  let control = outer_control(traversal)
  let checked_traversal = cek_data_traverse_v1.encode_control_v1(traversal)
  let current_hash = redeemer_item_proof_v1.hash_control_v1(control)
  and {
    redeemer_item_proof_v1.hash_stage_data_outer_with_checked_traversal_v1(
      control,
      checked_traversal,
    ) == current_hash,
    redeemer_item_proof_v1.hash_stage_data_outer_with_checked_traversal_v1(
      control,
      checked_traversal |> bytearray.concat(#"00"),
    ) != current_hash,
    redeemer_item_proof_v1.hash_stage_data_outer_with_checked_traversal_v1(
      control,
      checked_traversal,
    ) != hash_b,
  }
}

test narrow_action_commitment_rejects_wrong_family_and_preimage() {
  let control = outer_control(fold_control())
  let control_data: Data = control
  let fold_action = fold_map_action()
  let finalize_action = finalize_frame_action()
  let action_data: Data = fold_action
  let expected_hash = normalization.narrow_preimage_hash_v1(
    control_data,
    action_data,
  )
  and {
    normalization.narrow_action_is_bound_v1(
      control_data,
      fold_action,
      normalization.fold_map_family,
      expected_hash,
    ),
    !normalization.narrow_action_is_bound_v1(
      control_data,
      fold_action,
      normalization.finalize_frame_family,
      expected_hash,
    ),
    !normalization.narrow_action_is_bound_v1(
      control_data,
      finalize_action,
      normalization.fold_map_family,
      expected_hash,
    ),
    !normalization.narrow_action_is_bound_v1(
      control_data,
      fold_action,
      normalization.fold_map_family,
      hash_b,
    ),
  }
}

test fold_map_action_only_identity_rejects_field_sibling_family_and_hash_mutations() {
  let action = fold_map_action()
  let action_identity = normalization.traversal_action_identity_v1(action)
  expect cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  } = action
  let wrong_index = cek_data_traverse_v1.FoldMap {
    frame,
    pair_index: pair_index + 1,
    key,
    value,
    key_siblings,
    value_siblings,
  }
  let wrong_key = cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key: cek_data_v1.DataSummaryV1 { ..key, root: hash_b },
    value,
    key_siblings,
    value_siblings,
  }
  let wrong_sibling = cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings: [hash_b],
    value_siblings,
  }
  let wrong_value_sibling = cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings: [hash_b],
  }
  and {
    normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.fold_map_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      wrong_index,
      normalization.fold_map_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      wrong_key,
      normalization.fold_map_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      wrong_sibling,
      normalization.fold_map_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      wrong_value_sibling,
      normalization.fold_map_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.finalize_frame_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.fold_map_family,
      hash_b,
    ),
  }
}

test finalize_frame_action_only_identity_rejects_frame_parent_family_and_hash_mutations() {
  let action = finalize_frame_action()
  let action_identity = normalization.traversal_action_identity_v1(action)
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  let wrong_frame = cek_data_traverse_v1.FinalizeFrame {
    frame: cek_data_frame_v1.initial_map_frame_v1(#"", 2),
    parent,
  }
  let wrong_parent = cek_data_traverse_v1.FinalizeFrame {
    frame,
    parent: Some(cek_data_frame_v1.initial_list_frame_v1(#"", 0)),
  }
  and {
    normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.finalize_frame_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      wrong_frame,
      normalization.finalize_frame_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      wrong_parent,
      normalization.finalize_frame_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.fold_map_family,
      action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.finalize_frame_family,
      hash_b,
    ),
  }
}

test predecessor_deployment_and_route_mutations_reject() {
  let control = outer_control(fold_control())
  let state = traversal_normalized_state(control)
  and {
    normalization.traversal_normalized_state_is_bound_v1(
      state,
      hash_a,
      script_a,
      script_b,
    ),
    !normalization.traversal_normalized_state_is_bound_v1(
      state,
      hash_b,
      script_a,
      script_b,
    ),
    !normalization.traversal_normalized_state_is_bound_v1(
      state,
      hash_a,
      script_e,
      script_b,
    ),
    !normalization.traversal_normalized_state_is_bound_v1(
      state,
      hash_a,
      script_a,
      script_e,
    ),
    normalization.outer_normalizer_route_is_exact_v1(state, script_c, script_d),
    !normalization.outer_normalizer_route_is_exact_v1(state, script_b, script_d),
    !normalization.outer_normalizer_route_is_exact_v1(state, script_c, script_e),
  }
}

test family_templates_are_exact_and_corruption_changes_encoding() {
  let traversal = fold_control()
  let checked = cek_data_traverse_v1.encode_control_v1(traversal)
  expect normalization.FoldMapFrameRootTemplate {
    prefix_before_frame_root_cbor,
    suffix_after_frame_root_cbor,
  }
    = normalization.traversal_serialization_template_v1(
    traversal,
    normalization.fold_map_family,
  )
  let assembled = prefix_before_frame_root_cbor
    |> bytearray.concat(encode_definite_bytes(traversal.frame_root))
    |> bytearray.concat(suffix_after_frame_root_cbor)
  let finalize_traversal = finalize_control()
  let finalize_checked = cek_data_traverse_v1.encode_control_v1(
    finalize_traversal,
  )
  expect normalization.FinalizeFrameTemplate {
    prefix_before_stage_cbor,
    source_fields_before_frame_root_cbor,
    suffix_before_result_cbor,
  }
    = normalization.traversal_serialization_template_v1(
    finalize_traversal,
    normalization.finalize_frame_family,
  )
  let finalize_assembled = prefix_before_stage_cbor
    |> bytearray.concat(cbor.serialise(finalize_traversal.stage))
    |> bytearray.concat(source_fields_before_frame_root_cbor)
    |> bytearray.concat(encode_definite_bytes(finalize_traversal.frame_root))
    |> bytearray.concat(suffix_before_result_cbor)
    |> bytearray.concat(#"d87a80")
  and {
    assembled == checked,
    bytearray.concat(assembled, #"00") != checked,
    finalize_assembled == finalize_checked,
    bytearray.concat(finalize_assembled, #"00") != finalize_checked,
  }
}

test fold_map_shared_core_matches_generic_step_with_nontrivial_membership() {
  let control = nontrivial_fold_map_control()
  let action = nontrivial_fold_map_action()
  expect cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  } = action
  expect Some(next_frame_root) = cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
    control.frame_root,
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    control,
    None,
    action,
  )
  and {
    key_siblings != [],
    value_siblings != [],
    generic_next
      == cek_data_traverse_v1.DataTraverseControlV1 {
          ..control,
          frame_root: next_frame_root,
        },
    generic_next.frame_root == next_frame_root,
  }
}

test fold_map_authenticated_template_equals_checked_generic_next_encoding() {
  let state = outer_normalized_fold_map_state()
  let action = nontrivial_fold_map_action()
  expect cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  } = action
  expect Some(next_frame_root) = cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
    state.validated_traversal_control.frame_root,
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  )
  expect normalization.FoldMapFrameRootTemplate {
    prefix_before_frame_root_cbor,
    suffix_after_frame_root_cbor,
  } = state.traversal_serialization_template
  let assembled = prefix_before_frame_root_cbor
    |> bytearray.concat(encode_definite_bytes(next_frame_root))
    |> bytearray.concat(suffix_after_frame_root_cbor)
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    state.validated_traversal_control,
    None,
    action,
  )
  assembled == cek_data_traverse_v1.encode_control_v1(generic_next)
}

test fold_map_authenticated_prefix_hash_equals_generic_outer_and_expected_hash() {
  let state = outer_normalized_fold_map_state()
  let action = nontrivial_fold_map_action()
  expect cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  } = action
  expect Some(next_frame_root) = cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
    state.validated_traversal_control.frame_root,
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  )
  expect normalization.FoldMapFrameRootTemplate {
    prefix_before_frame_root_cbor,
    suffix_after_frame_root_cbor,
  } = state.traversal_serialization_template
  let assembled = prefix_before_frame_root_cbor
    |> bytearray.concat(encode_definite_bytes(next_frame_root))
    |> bytearray.concat(suffix_after_frame_root_cbor)
  let actual = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
    state.next_item_control_hash_prefix_cbor,
    assembled,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    state.validated_traversal_control,
    None,
    action,
  )
  let current_outer = outer_control(state.validated_traversal_control)
  let next_outer = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    ..current_outer,
    traversal: Some(generic_next),
  }
  and {
    actual == redeemer_item_proof_v1.hash_control_v1(next_outer),
    actual == state.expected_next_item_control_hash,
  }
}

test fold_map_shared_core_rejects_stage_frame_hash_index_key_and_value_mutations() {
  let control = nontrivial_fold_map_control()
  let action = nontrivial_fold_map_action()
  expect cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  } = action
  let wrong_key = cek_data_v1.DataSummaryV1 { ..key, root: hash_b }
  let wrong_value = cek_data_v1.DataSummaryV1 { ..value, root: hash_a }
  and {
    control.stage == cek_data_traverse_v1.stage_fold,
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      control.frame_root,
      frame,
      pair_index,
      key,
      value,
      key_siblings,
      value_siblings,
    ) != None,
    cek_data_traverse_v1.step_v1(
      cek_data_traverse_v1.DataTraverseControlV1 {
        ..control,
        stage: cek_data_traverse_v1.stage_close,
      },
      None,
      action,
    ) == None,
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      hash_a,
      frame,
      pair_index,
      key,
      value,
      key_siblings,
      value_siblings,
    ) == None,
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      control.frame_root,
      cek_data_frame_v1.initial_map_frame_v1(#"", 0),
      pair_index,
      key,
      value,
      key_siblings,
      value_siblings,
    ) == None,
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      control.frame_root,
      frame,
      pair_index + 1,
      key,
      value,
      key_siblings,
      value_siblings,
    ) == None,
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      control.frame_root,
      frame,
      pair_index,
      wrong_key,
      value,
      key_siblings,
      value_siblings,
    ) == None,
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      control.frame_root,
      frame,
      pair_index,
      key,
      wrong_value,
      key_siblings,
      value_siblings,
    ) == None,
  }
}

test fold_map_shared_core_rejects_each_membership_sibling_path_mutation() {
  let control = nontrivial_fold_map_control()
  let action = nontrivial_fold_map_action()
  expect cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  } = action
  and {
    key_siblings != [],
    value_siblings != [],
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      control.frame_root,
      frame,
      pair_index,
      key,
      value,
      [hash_a],
      value_siblings,
    ) == None,
    cek_data_traverse_v1.prevalidated_fold_map_next_frame_root_v1(
      control.frame_root,
      frame,
      pair_index,
      key,
      value,
      key_siblings,
      [hash_a],
    ) == None,
  }
}

test fold_map_executor_rebind_rejects_wrong_family_identity_and_action() {
  let state = outer_normalized_fold_map_state()
  let action = nontrivial_fold_map_action()
  expect cek_data_traverse_v1.FoldMap {
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  } = action
  let wrong_action = cek_data_traverse_v1.FoldMap {
    frame,
    pair_index: pair_index + 1,
    key,
    value,
    key_siblings,
    value_siblings,
  }
  and {
    normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.fold_map_family,
      state.authenticated_traversal_action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      wrong_action,
      normalization.fold_map_family,
      state.authenticated_traversal_action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.finalize_frame_family,
      state.authenticated_traversal_action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      action,
      normalization.fold_map_family,
      hash_a,
    ),
  }
}

test fold_map_normalized_state_rejects_deployment_predecessor_and_route_mutations() {
  let state = outer_normalized_fold_map_state()
  and {
    normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_b,
      script_a,
      script_b,
      script_c,
      script_d,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_a,
      script_e,
      script_b,
      script_c,
      script_d,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_a,
      script_a,
      script_e,
      script_c,
      script_d,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_a,
      script_a,
      script_b,
      script_e,
      script_d,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_e,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_d,
    ),
    normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.fold_map_family,
      script_d,
      script_e,
      script_e,
    ),
    !normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.fold_map_family,
      script_c,
      script_e,
      script_e,
    ),
    !normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.fold_map_family,
      script_d,
      script_d,
      script_e,
    ),
    !normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.fold_map_family,
      script_d,
      script_e,
      script_d,
    ),
  }
}

test fold_map_hash_boundary_rejects_expected_prefix_and_template_mutations() {
  let state = outer_normalized_fold_map_state()
  let action = nontrivial_fold_map_action()
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    state.validated_traversal_control,
    None,
    action,
  )
  let checked_next = cek_data_traverse_v1.encode_control_v1(generic_next)
  let actual = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
    state.next_item_control_hash_prefix_cbor,
    checked_next,
  )
  let wrong_prefix_hash = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
    bytearray.concat(state.next_item_control_hash_prefix_cbor, #"00"),
    checked_next,
  )
  expect normalization.FoldMapFrameRootTemplate {
    prefix_before_frame_root_cbor,
    suffix_after_frame_root_cbor,
  } = state.traversal_serialization_template
  let wrong_template_state = normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    ..state,
    traversal_serialization_template: normalization.FoldMapFrameRootTemplate {
      prefix_before_frame_root_cbor: bytearray.concat(
        prefix_before_frame_root_cbor,
        #"00",
      ),
      suffix_after_frame_root_cbor,
    },
  }
  let wrong_expected_state = normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    ..state,
    expected_next_item_control_hash: hash_a,
  }
  and {
    actual == state.expected_next_item_control_hash,
    wrong_prefix_hash != state.expected_next_item_control_hash,
    !normalization.outer_normalized_state_is_bound_v1(
      wrong_template_state,
      normalization.fold_map_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_e,
    ),
    actual != wrong_expected_state.expected_next_item_control_hash,
  }
}

test common_execution_attested_output_is_exact_and_minimal_for_fold_map() {
  let state = outer_normalized_fold_map_state()
  let expected = normalization.execution_attested_state_v1(
    state,
    state.expected_next_item_control_hash,
  )
  let wrong = normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
    ..expected,
    actual_next_item_control_hash: hash_a,
  }
  let expected_data: Data = expected
  let wrong_data: Data = wrong
  and {
    expected.version == normalization.version,
    expected.domain == normalization.execution_attested_domain,
    expected.deployment_id == state.deployment_id,
    expected.base_provenance_identity == state.base_provenance_identity,
    expected.action_family == normalization.fold_map_family,
    expected.actual_next_item_control_hash
      == expected.expected_next_item_control_hash,
    expected_data != wrong_data,
  }
}

test finalize_shared_core_matches_generic_terminal_completion() {
  let control = terminal_finalize_control()
  let action = terminal_finalize_action()
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  expect Some(transition) = cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
    control.frame_root,
    control.offset,
    control.source_length,
    frame,
    parent,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    control,
    None,
    action,
  )
  and {
    transition.next_stage == cek_data_traverse_v1.stage_terminal,
    transition.next_frame_root == #"",
    transition.next_result != None,
    generic_next
      == cek_data_traverse_v1.DataTraverseControlV1 {
          ..control,
          stage: transition.next_stage,
          frame_root: transition.next_frame_root,
          pending_large_expected_children: None,
          integer: None,
          bytes: None,
          result: transition.next_result,
        },
  }
}

test finalize_shared_core_matches_generic_authenticated_parent_append() {
  let control = parent_append_finalize_control()
  let action = parent_append_finalize_action()
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  expect Some(parent_frame) = parent
  expect Some(summary) = cek_data_frame_v1.finalized_summary_v1(frame)
  expect Some(next_parent) = cek_data_frame_v1.append_child_v1(
    parent_frame,
    summary,
  )
  expect Some(transition) = cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
    control.frame_root,
    control.offset,
    control.source_length,
    frame,
    parent,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    control,
    None,
    action,
  )
  and {
    parent_frame.expected_children == 2,
    parent_frame.child_count == 0,
    next_parent.child_count == 1,
    transition.next_stage == cek_data_traverse_v1.stage_head,
    transition.next_frame_root
      == cek_data_frame_v1.hash_frame_v1(next_parent),
    transition.next_result == None,
    generic_next
      == cek_data_traverse_v1.DataTraverseControlV1 {
          ..control,
          stage: transition.next_stage,
          frame_root: transition.next_frame_root,
          pending_large_expected_children: None,
          integer: None,
          bytes: None,
          result: transition.next_result,
        },
  }
}

test finalize_terminal_template_equals_checked_generic_next_encoding() {
  let state = terminal_finalize_state()
  let action = terminal_finalize_action()
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  expect Some(transition) = cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
    state.validated_traversal_control.frame_root,
    state.validated_traversal_control.offset,
    state.validated_traversal_control.source_length,
    frame,
    parent,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    state.validated_traversal_control,
    None,
    action,
  )
  assemble_finalize_transition_cbor(state, transition)
    == cek_data_traverse_v1.encode_control_v1(generic_next)
}

test finalize_parent_append_template_equals_checked_generic_next_encoding() {
  let state = parent_append_finalize_state()
  let action = parent_append_finalize_action()
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  expect Some(transition) = cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
    state.validated_traversal_control.frame_root,
    state.validated_traversal_control.offset,
    state.validated_traversal_control.source_length,
    frame,
    parent,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    state.validated_traversal_control,
    None,
    action,
  )
  assemble_finalize_transition_cbor(state, transition)
    == cek_data_traverse_v1.encode_control_v1(generic_next)
}

test finalize_terminal_authenticated_prefix_hash_equals_generic_and_expected() {
  let state = terminal_finalize_state()
  let action = terminal_finalize_action()
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  expect Some(transition) = cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
    state.validated_traversal_control.frame_root,
    state.validated_traversal_control.offset,
    state.validated_traversal_control.source_length,
    frame,
    parent,
  )
  let assembled = assemble_finalize_transition_cbor(state, transition)
  let actual = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
    state.next_item_control_hash_prefix_cbor,
    assembled,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    state.validated_traversal_control,
    None,
    action,
  )
  let current_outer = outer_control(state.validated_traversal_control)
  let next_outer = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    ..current_outer,
    traversal: Some(generic_next),
  }
  and {
    actual == redeemer_item_proof_v1.hash_control_v1(next_outer),
    actual == state.expected_next_item_control_hash,
  }
}

test finalize_parent_append_authenticated_prefix_hash_equals_generic_and_expected() {
  let state = parent_append_finalize_state()
  let action = parent_append_finalize_action()
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  expect Some(transition) = cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
    state.validated_traversal_control.frame_root,
    state.validated_traversal_control.offset,
    state.validated_traversal_control.source_length,
    frame,
    parent,
  )
  let assembled = assemble_finalize_transition_cbor(state, transition)
  let actual = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
    state.next_item_control_hash_prefix_cbor,
    assembled,
  )
  expect Some(generic_next) = cek_data_traverse_v1.step_v1(
    state.validated_traversal_control,
    None,
    action,
  )
  let current_outer = outer_control(state.validated_traversal_control)
  let next_outer = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    ..current_outer,
    traversal: Some(generic_next),
  }
  and {
    actual == redeemer_item_proof_v1.hash_control_v1(next_outer),
    actual == state.expected_next_item_control_hash,
  }
}

test finalize_terminal_rejects_frame_nonfinalized_parent_and_offset_mutations() {
  let control = terminal_finalize_control()
  let frame = terminal_finalize_frame()
  let nonfinalized = cek_data_frame_v1.initial_list_frame_v1(#"", 1)
  and {
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset,
      control.source_length,
      frame,
      None,
    ) != None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      hash_a,
      control.offset,
      control.source_length,
      frame,
      None,
    ) == None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      cek_data_frame_v1.hash_frame_v1(nonfinalized),
      control.offset,
      control.source_length,
      nonfinalized,
      None,
    ) == None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset,
      control.source_length,
      frame,
      Some(append_parent_frame()),
    ) == None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset - 1,
      control.source_length,
      frame,
      None,
    ) == None,
  }
}

test finalize_parent_append_rejects_absent_wrong_malformed_and_full_parent() {
  let control = parent_append_finalize_control()
  let frame = parent_append_finalize_frame()
  let parent = append_parent_frame()
  let malformed_parent = cek_data_frame_v1.DataFrameV1 {
    ..parent,
    expected_children: -1,
  }
  let full_parent = cek_data_frame_v1.initial_list_frame_v1(#"", 0)
  let full_parent_child = cek_data_frame_v1.initial_list_frame_v1(
    cek_data_frame_v1.hash_frame_v1(full_parent),
    0,
  )
  and {
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset,
      control.source_length,
      frame,
      Some(parent),
    ) != None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset,
      control.source_length,
      frame,
      None,
    ) == None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset,
      control.source_length,
      frame,
      Some(cek_data_frame_v1.initial_list_frame_v1(#"", 1)),
    ) == None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset,
      control.source_length,
      frame,
      Some(malformed_parent),
    ) == None,
    cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
      cek_data_frame_v1.hash_frame_v1(full_parent_child),
      control.offset,
      control.source_length,
      full_parent_child,
      Some(full_parent),
    ) == None,
  }
}

test finalize_executor_rebind_rejects_action_family_and_identity_mutations() {
  let terminal_state = terminal_finalize_state()
  let terminal_action = terminal_finalize_action()
  let parent_action = parent_append_finalize_action()
  and {
    normalization.traversal_action_identity_is_bound_v1(
      terminal_action,
      normalization.finalize_frame_family,
      terminal_state.authenticated_traversal_action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      parent_action,
      normalization.finalize_frame_family,
      terminal_state.authenticated_traversal_action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      terminal_action,
      normalization.fold_map_family,
      terminal_state.authenticated_traversal_action_identity,
    ),
    !normalization.traversal_action_identity_is_bound_v1(
      terminal_action,
      normalization.finalize_frame_family,
      hash_a,
    ),
  }
}

test finalize_normalized_state_rejects_stage_domain_deployment_and_route_mutations() {
  let state = terminal_finalize_state()
  let wrong_stage = normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    ..state,
    validated_traversal_control: cek_data_traverse_v1.DataTraverseControlV1 {
      ..state.validated_traversal_control,
      stage: cek_data_traverse_v1.stage_close,
    },
  }
  let wrong_domain = normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    ..state,
    domain: normalization.traversal_normalized_domain,
  }
  and {
    normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.fold_map_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      wrong_stage,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      wrong_domain,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.finalize_frame_family,
      hash_b,
      script_a,
      script_b,
      script_c,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.finalize_frame_family,
      hash_a,
      script_e,
      script_b,
      script_c,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_e,
      script_c,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_b,
      script_e,
      script_f,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_e,
    ),
    !normalization.outer_normalized_state_is_bound_v1(
      state,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_f,
      script_d,
    ),
    normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.finalize_frame_family,
      script_f,
      script_e,
      script_e,
    ),
    !normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.fold_map_family,
      script_f,
      script_e,
      script_e,
    ),
    !normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.finalize_frame_family,
      script_d,
      script_e,
      script_e,
    ),
    !normalization.semantic_executor_route_is_exact_v1(
      state,
      normalization.finalize_frame_family,
      script_f,
      script_d,
      script_e,
    ),
  }
}

test finalize_hash_boundary_rejects_expected_prefix_template_and_result_mutations() {
  let state = terminal_finalize_state()
  let action = terminal_finalize_action()
  expect cek_data_traverse_v1.FinalizeFrame { frame, parent } = action
  expect Some(transition) = cek_data_traverse_v1.prevalidated_finalize_frame_transition_v1(
    state.validated_traversal_control.frame_root,
    state.validated_traversal_control.offset,
    state.validated_traversal_control.source_length,
    frame,
    parent,
  )
  let assembled = assemble_finalize_transition_cbor(state, transition)
  let actual = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
    state.next_item_control_hash_prefix_cbor,
    assembled,
  )
  let wrong_prefix_hash = redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
    bytearray.concat(state.next_item_control_hash_prefix_cbor, #"00"),
    assembled,
  )
  expect Some(exact_summary) = transition.next_result
  let wrong_result_cbor = cek_data_traverse_v1.encode_optional_summary_v1(
    Some(cek_data_v1.DataSummaryV1 { ..exact_summary, root: hash_a }),
  )
  expect normalization.FinalizeFrameTemplate {
    prefix_before_stage_cbor,
    source_fields_before_frame_root_cbor,
    suffix_before_result_cbor,
  } = state.traversal_serialization_template
  let wrong_result_assembled = prefix_before_stage_cbor
    |> bytearray.concat(cbor.serialise(transition.next_stage))
    |> bytearray.concat(source_fields_before_frame_root_cbor)
    |> bytearray.concat(encode_definite_bytes(transition.next_frame_root))
    |> bytearray.concat(suffix_before_result_cbor)
    |> bytearray.concat(wrong_result_cbor)
  let wrong_template_state = normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    ..state,
    traversal_serialization_template: normalization.FinalizeFrameTemplate {
      prefix_before_stage_cbor: bytearray.concat(prefix_before_stage_cbor, #"00"),
      source_fields_before_frame_root_cbor,
      suffix_before_result_cbor,
    },
  }
  let wrong_expected_state = normalization.OuterNormalizedScriptSourcesRedeemerActionV1 {
    ..state,
    expected_next_item_control_hash: hash_a,
  }
  and {
    actual == state.expected_next_item_control_hash,
    wrong_prefix_hash != state.expected_next_item_control_hash,
    redeemer_item_proof_v1.hash_stage_data_from_authenticated_prefix_v1(
      state.next_item_control_hash_prefix_cbor,
      wrong_result_assembled,
    ) != state.expected_next_item_control_hash,
    !normalization.outer_normalized_state_is_bound_v1(
      wrong_template_state,
      normalization.finalize_frame_family,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_f,
      script_e,
    ),
    actual != wrong_expected_state.expected_next_item_control_hash,
  }
}

test common_execution_attested_output_is_exact_for_finalize_and_shared_with_fold_map() {
  let finalize_state = terminal_finalize_state()
  let fold_map_state = outer_normalized_fold_map_state()
  let finalize_output = normalization.execution_attested_state_v1(
    finalize_state,
    finalize_state.expected_next_item_control_hash,
  )
  let fold_map_output = normalization.execution_attested_state_v1(
    fold_map_state,
    fold_map_state.expected_next_item_control_hash,
  )
  let wrong_domain = normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
    ..finalize_output,
    domain: normalization.outer_normalized_domain,
  }
  let wrong_family = normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
    ..finalize_output,
    action_family: normalization.fold_map_family,
  }
  let wrong_actual = normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
    ..finalize_output,
    actual_next_item_control_hash: hash_a,
  }
  let finalize_output_data: Data = finalize_output
  let wrong_domain_data: Data = wrong_domain
  let wrong_family_data: Data = wrong_family
  let wrong_actual_data: Data = wrong_actual
  and {
    finalize_output.domain == normalization.execution_attested_domain,
    fold_map_output.domain == normalization.execution_attested_domain,
    finalize_output.action_family == normalization.finalize_frame_family,
    fold_map_output.action_family == normalization.fold_map_family,
    finalize_output.semantic_executor_script_hash == script_f,
    fold_map_output.semantic_executor_script_hash == script_d,
    finalize_output.actual_next_item_control_hash
      == finalize_output.expected_next_item_control_hash,
    finalize_output_data != wrong_domain_data,
    finalize_output_data != wrong_family_data,
    finalize_output_data != wrong_actual_data,
  }
}

test execution_attestation_settlement_accepts_both_bound_families() {
  let fold_outer = outer_normalized_fold_map_state()
  let fold_envelope = settlement_envelope(fold_outer)
  let fold_state = settlement_attestation(fold_outer)
  let finalize_outer = terminal_finalize_state()
  let finalize_envelope = settlement_envelope(finalize_outer)
  let finalize_state = settlement_attestation(finalize_outer)
  let winning_output: Data = validation_resolution_v1.winning_resolution()
  and {
    validation_resolution_v1.prepared_resolution_is_well_formed(
      fold_envelope.base,
    ),
    validation_resolution_v1.prepared_resolution_is_well_formed(
      finalize_envelope.base,
    ),
    settlement_is_exact(
      fold_state,
      fold_envelope,
      script_g,
      winning_output,
    ),
    settlement_is_exact(
      finalize_state,
      finalize_envelope,
      script_g,
      winning_output,
    ),
  }
}

test execution_attestation_settlement_rejects_envelope_provenance_and_domain_mutations() {
  let outer = outer_normalized_fold_map_state()
  let envelope = settlement_envelope(outer)
  let state = settlement_attestation(outer)
  let wrong_envelope =
    normalization.PreparedScriptSourcesRedeemerEnvelopeV1 {
      ..envelope,
      canonical_auxiliary_hash: hash_a,
    }
  let wrong_base =
    normalization.PreparedScriptSourcesRedeemerEnvelopeV1 {
      ..envelope,
      base: validation_resolution_v1.PreparedValidationResolutionStateV1 {
        ..envelope.base,
        evidence_hash: hash_b,
      },
    }
  let wrong_provenance =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      base_provenance_identity: hash_a,
    }
  let wrong_domain =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      domain: normalization.outer_normalized_domain,
    }
  and {
    normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      wrong_envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      wrong_base,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      wrong_provenance,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      wrong_domain,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
  }
}

test execution_attestation_settlement_rejects_deployment_family_and_route_mutations() {
  let outer = outer_normalized_fold_map_state()
  let envelope = settlement_envelope(outer)
  let state = settlement_attestation(outer)
  let wrong_family =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      action_family: normalization.finalize_frame_family,
    }
  let wrong_executor =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      semantic_executor_script_hash: script_f,
    }
  and {
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      hash_b,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      hash_a,
      script_f,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      hash_a,
      script_a,
      script_f,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_f,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_f,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      state,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_d,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      wrong_family,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
    !normalization.execution_attestation_is_bound_to_envelope_v1(
      wrong_executor,
      envelope,
      hash_a,
      script_a,
      script_b,
      script_c,
      script_d,
      script_f,
      script_e,
    ),
  }
}

test execution_attestation_settlement_rejects_copied_hash_count_and_output_mutations() {
  let outer = terminal_finalize_state()
  let envelope = settlement_envelope(outer)
  let state = settlement_attestation(outer)
  let wrong_action =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      canonical_action_hash: hash_b,
    }
  let wrong_identity =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      authenticated_traversal_action_identity: #"",
    }
  let wrong_current =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      current_pending_item_control_hash: #"",
    }
  let wrong_expected =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      expected_next_item_control_hash: #"",
    }
  let wrong_actual =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      actual_next_item_control_hash: #"",
    }
  let wrong_count =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      redeemer_count: state.redeemer_count + 1,
    }
  let wrong_total =
    normalization.ScriptSourcesRedeemerExecutionAttestedStateV1 {
      ..state,
      redeemer_total_count: state.redeemer_total_count + 1,
    }
  let winning_output: Data = validation_resolution_v1.winning_resolution()
  let wrong_output: Data =
    validation_resolution_v1.WinningValidationResolutionStateV1 { version: 2 }
  and {
    !settlement_is_exact(
      wrong_action,
      envelope,
      script_g,
      winning_output,
    ),
    !settlement_is_exact(
      wrong_identity,
      envelope,
      script_g,
      winning_output,
    ),
    !settlement_is_exact(
      wrong_current,
      envelope,
      script_g,
      winning_output,
    ),
    !settlement_is_exact(
      wrong_expected,
      envelope,
      script_g,
      winning_output,
    ),
    !settlement_is_exact(
      wrong_actual,
      envelope,
      script_g,
      winning_output,
    ),
    !settlement_is_exact(
      wrong_count,
      envelope,
      script_g,
      winning_output,
    ),
    !settlement_is_exact(
      wrong_total,
      envelope,
      script_g,
      winning_output,
    ),
    !settlement_is_exact(state, envelope, script_f, winning_output),
    !settlement_is_exact(state, envelope, script_g, wrong_output),
  }
}

// ---------------------------------------------------------------------------
// VM-DEFECT-6 regression cluster: `verify_raw_envelope_v1` canonical framing.
//
// The raw-envelope route had zero coverage, which is why a reconstruction that
// trapped on every input survived. These fixtures build a *legitimate* stage-1
// pending redeemer-item witness with the machine's own canonical encoder
// (`validation_machine_v1.encode_script_sources_redeemer_item_witness`) and
// pair the positive with the two encoding controls GOAL_SPEC §3 invariant 9
// requires: a Plutus `serialiseData` re-encoding of the very same control, and
// a mutated successor commitment.
// ---------------------------------------------------------------------------

const raw_compact_cbor =
  #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const raw_witness_set_compact_cbor =
  #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"

const raw_field_preimage_lengths_cbor =
  #"c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3"

const raw_context_cbor =
  #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4"

fn raw_envelope_control() -> validation_machine_v1.ScriptSourcesControlV1 {
  validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: raw_compact_cbor,
    witness_set_compact_cbor: raw_witness_set_compact_cbor,
    field_preimage_lengths_cbor: raw_field_preimage_lengths_cbor,
    context_cbor: raw_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: zero,
    signer_count: 0,
    signer_frontier_commitment: zero,
    resolved_item_peaks: [],
    stage: 1,
    source_count: 0,
    source_peaks: [],
    redeemer_count: 0,
    redeemer_peaks: [],
    replay_cursor: 0,
    replay_accumulator: zero,
    replay_remaining_schedule_hash: zero,
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: [],
    output_cursor: 0,
    output_count: 0,
    output_peaks: [],
    output_total_count: 0,
    receive_scan: validation_machine_v1.empty_receive_purpose_scan_control(),
    source_total_count: 0,
    redeemer_total_count: 3,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery: validation_machine_v1.empty_script_discovery_control(),
    output_proof: None,
    pending_source_cbor: hash_a,
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
    resolution_schedule_hash: zero,
  }
}

/// The canonical stage-1 pending redeemer-item witness the machine commits:
/// `encode_definite_array_header(31)` over the thirty base items plus
/// `encode_definite_bytes(<pending item control hash>)`.
fn raw_envelope_witness_cbor(pending_item_control_hash: ByteArray) -> ByteArray {
  validation_machine_v1.encode_script_sources_redeemer_item_witness(
    raw_envelope_control(),
    pending_item_control_hash,
  )
}

/// The Plutus `serialiseData` re-encoding of the *same* control: indefinite
/// `9f..ff` array header and `5f..ff`-chunked bytestrings over 64 bytes. This
/// is the shape the pre-fix reconstruction demanded.
fn raw_envelope_serialise_data_witness_cbor(
  pending_item_control_hash: ByteArray,
) -> ByteArray {
  expect Some(control_data) =
    cbor.deserialise(raw_envelope_witness_cbor(pending_item_control_hash))
  cbor.serialise(control_data)
}

fn raw_envelope_pre_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..settlement_pre_state(),
    transaction_commitment: compact.native_tx_proof_commitment_v1(
      raw_compact_cbor,
      raw_witness_set_compact_cbor,
      raw_field_preimage_lengths_cbor,
    ),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      raw_context_cbor,
    ),
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      1,
      raw_envelope_witness_cbor(hash_a),
    ),
  }
}

fn raw_envelope_transition(
  work_witness_cbor: ByteArray,
  successor_work_witness_cbor: ByteArray,
) -> validation_machine_v1.ValidationOneStepWitnessV1 {
  validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      ..raw_envelope_pre_state(),
      program_counter: 2,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        2,
        successor_work_witness_cbor,
      ),
    },
  }
}

fn raw_envelope_item_control() -> redeemer_item_proof_v1.RedeemerItemProofControlV1 {
  redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    version: 1,
    mode: redeemer_item_proof_v1.mode_data,
    stage: redeemer_item_proof_v1.stage_data,
    item_index: 0,
    item_count: 3,
    total_length: 8,
    item_commitment: hash_b,
    expected_purpose_tag: 0,
    expected_pointer_index: 0,
    purpose_tag: 0,
    pointer_index: 0,
    data_offset: 4,
    data_length: 4,
    execution_memory: 0,
    execution_steps: 0,
    traversal: None,
  }
}

fn raw_envelope_auxiliary(
  action: cek_data_traverse_v1.DataTraverseActionV1,
) -> Data {
  let auxiliary: validation_machine_v1.ValidationAuxiliaryWitnessV1 =
    validation_machine_v1.RedeemerItemStepWitness {
      redeemer_control: None,
      control: raw_envelope_item_control(),
      witness: redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
        action: redeemer_item_proof_v1.RedeemerItemTraverseData { action },
        chunk_proof: None,
        next_chunk_proof: None,
      },
    }
  let auxiliary_data: Data = auxiliary
  auxiliary_data
}

test raw_envelope_auxiliary_constructor_index_is_the_machine_witness() {
  let Pair(index, fields) =
    builtin.un_constr_data(raw_envelope_auxiliary(fold_map_action()))
  and {
    index == 18,
    list.length(fields) == 3,
  }
}

test raw_envelope_accepts_canonical_stage_one_pending_witness() {
  let witness_cbor = raw_envelope_witness_cbor(hash_a)
  let canonical_successor_cbor = raw_envelope_witness_cbor(hash_b)
  let auxiliary = raw_envelope_auxiliary(fold_map_action())
  let facts =
    normalization.verify_raw_envelope_v1(
      raw_envelope_pre_state(),
      raw_envelope_transition(witness_cbor, canonical_successor_cbor),
      auxiliary,
      hash_b,
      normalization.fold_map_family,
    )
  expect Some(observed) = facts
  and {
    // The canonical witness really is definite-length framed, so the shape the
    // pre-fix code demanded is a genuinely different byte string.
    bytearray.take(witness_cbor, 2) == encode_definite_array_header(31),
    witness_cbor != raw_envelope_serialise_data_witness_cbor(hash_a),
    // The successor commitment the module derives must be byte-identical to the
    // machine's own canonical encoder for the next pending hash -- this is the
    // property the serialiseData reconstruction could never satisfy.
    canonical_successor_cbor == bytearray.concat(
      bytearray.take(witness_cbor, bytearray.length(witness_cbor) - 32),
      hash_b,
    ),
    observed.current_pending_item_control_hash == hash_a,
    observed.redeemer_count == 0,
    observed.redeemer_total_count == 3,
    observed.canonical_auxiliary_hash == normalization.canonical_auxiliary_hash_v1(
      auxiliary,
    ),
    observed.canonical_action_hash == normalization.canonical_action_hash_v1(
      auxiliary,
      normalization.fold_map_family,
    ),
  }
}

test raw_envelope_rejects_serialise_data_witness_and_mutated_successor() {
  let witness_cbor = raw_envelope_witness_cbor(hash_a)
  let canonical_successor_cbor = raw_envelope_witness_cbor(hash_b)
  let serialise_data_cbor = raw_envelope_serialise_data_witness_cbor(hash_a)
  let auxiliary = raw_envelope_auxiliary(fold_map_action())
  let verify =
    fn(
      transition: validation_machine_v1.ValidationOneStepWitnessV1,
      expected_next_item_control_hash: ByteArray,
    ) {
      normalization.verify_raw_envelope_v1(
        raw_envelope_pre_state(),
        transition,
        auxiliary,
        expected_next_item_control_hash,
        normalization.fold_map_family,
      )
    }
  // Same control, `serialiseData` framing: still deserialises to the same
  // thirty-one items, so the module reaches its framing checks and must reject.
  expect Some(serialise_data_items) = cbor.deserialise(serialise_data_cbor)
  let serialise_data_successor_cbor =
    raw_envelope_serialise_data_witness_cbor(hash_b)
  // A successor commitment over anything other than the canonical prefix with
  // the expected next hash spliced in.
  let mutated_successor_cbor =
    bytearray.concat(
      bytearray.take(
        canonical_successor_cbor,
        bytearray.length(canonical_successor_cbor) - 32,
      ),
      hash_a,
    )
  and {
    list.length(builtin.un_list_data(serialise_data_items)) == 31,
    serialise_data_cbor != witness_cbor,
    bytearray.take(serialise_data_cbor, 1) == #"9f",
    verify(
      raw_envelope_transition(serialise_data_cbor, serialise_data_successor_cbor),
      hash_b,
    ) == None,
    verify(
      raw_envelope_transition(serialise_data_cbor, canonical_successor_cbor),
      hash_b,
    ) == None,
    verify(
      raw_envelope_transition(witness_cbor, mutated_successor_cbor),
      hash_b,
    ) == None,
    verify(
      raw_envelope_transition(witness_cbor, canonical_successor_cbor),
      hash_a,
    ) == None,
    verify(
      raw_envelope_transition(witness_cbor, canonical_successor_cbor),
      bytearray.take(hash_b, 31),
    ) == None,
    verify(
      raw_envelope_transition(witness_cbor, canonical_successor_cbor),
      hash_b,
    ) != None,
  }
}
