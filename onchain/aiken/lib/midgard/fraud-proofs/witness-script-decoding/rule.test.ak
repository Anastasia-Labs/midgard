use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/witness_script_decoding/rule
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  RejectionReasonV1, WitnessNativeScriptDepthLimit, WitnessNativeScriptMalformed,
  WitnessNativeScriptNodeLimit, WitnessScriptHeaderMalformed,
}
use midgard/validation_merkle_v1

const accepted_tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const witness_hash =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const final_hash = #"22222222222222222222222222222222222222222222222222222222"

const scan_hash = #"33333333333333333333333333333333333333333333333333333333"

fn accepted_subject() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: accepted_tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn forced_header_subject() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: accepted_tx_id,
    source_key: #"01",
    rejection_reason: Some(WitnessScriptHeaderMalformed { script_index: 0 }),
  }
}

fn proof(item: ByteArray) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: rule.field_index,
    item_index: 0,
    total_length: bytearray.length(item),
    chunk_index: 0,
    chunk: item,
    frontier: validation_merkle_v1.append_leaf(
      0,
      [],
      bounded_item_v1.hash_chunk(rule.field_index, 0, 0, item),
    ),
    siblings: [],
  }
}

test binds_the_exact_forced_reason_and_coordinate() {
  let bound = rule.bind_subject_v1(forced_header_subject(), witness_hash, 0)
  bound.accused_class == rule.result_header_malformed
}

test refuses_a_substituted_forced_reason_coordinate() fail {
  expect _bound = rule.bind_subject_v1(forced_header_subject(), witness_hash, 1)
  True
}

test refuses_another_typed_reason() fail {
  let subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: accepted_tx_id,
    source_key: #"01",
    rejection_reason: Some(WitnessNativeScriptMalformed { script_index: 0 }),
  }
  let bound = rule.bind_subject_v1(subject, witness_hash, 0)
  expect bound.accused_class == rule.result_header_malformed
}

test distinguishes_header_payload_and_non_native_results() {
  let bound = rule.bind_subject_v1(accepted_subject(), witness_hash, 0)
  let header = rule.authenticate_item_v1(
    bound,
    fixture.malformed_wrapper_item_v1(),
    scan_hash,
  )
  let payload = rule.authenticate_item_v1(
    bound,
    fixture.malformed_payload_item_v1(),
    scan_hash,
  )
  let plutus = rule.authenticate_item_v1(
    bound,
    fixture.plutus_script_item_v1(),
    scan_hash,
  )
  and {
    header.result_class == rule.result_header_malformed,
    payload.result_class == rule.result_pending,
    plutus.result_class == rule.result_no_fault,
    rule.state_is_authentic_v1(header),
    rule.state_is_authentic_v1(payload),
    rule.state_is_authentic_v1(plutus),
  }
}

test frozen_scan_reaches_exact_terminal_through_authenticated_chunk() {
  let item = fixture.signature_script_item_v1()
  let bound = rule.bind_subject_v1(accepted_subject(), witness_hash, 0)
  let state = rule.authenticate_item_v1(bound, item, scan_hash)
  let control = native_script_scan_v1.decode_structure_control_v1(
    state.control_cbor,
  )
  let window = rule.authenticated_window_v1(state, control, proof(item), None)
  expect scan_engine.ScanAdvancedV1 { control: terminal } = scan_engine.budgeted_scan_v1(
    control,
    Some(window),
    [],
    2,
  )
  native_script_scan_v1.structure_terminal_is_exact_v1(terminal)
}

test frozen_scan_preserves_native_malformed_class() {
  let item = fixture.malformed_payload_item_v1()
  let bound = rule.bind_subject_v1(accepted_subject(), witness_hash, 0)
  let state = rule.authenticate_item_v1(bound, item, scan_hash)
  let control = native_script_scan_v1.decode_structure_control_v1(
    state.control_cbor,
  )
  let window = rule.authenticated_window_v1(state, control, proof(item), None)
  expect scan_engine.ScanRefusedV1 { refusal_class } = scan_engine.budgeted_scan_v1(
    control,
    Some(window),
    [],
    1,
  )
  rule.mapped_refusal_class_v1(refusal_class) == rule.result_native_malformed
}

test refuses_a_substituted_item_chunk() fail {
  let item = fixture.signature_script_item_v1()
  let bound = rule.bind_subject_v1(accepted_subject(), witness_hash, 0)
  let state = rule.authenticate_item_v1(bound, item, scan_hash)
  let control = native_script_scan_v1.decode_structure_control_v1(
    state.control_cbor,
  )
  let substituted = proof(fixture.malformed_payload_item_v1())
  expect _window = rule.authenticated_window_v1(
    state,
    control,
    substituted,
    None,
  )
  True
}

test checkpoint_binds_the_next_expected_script() {
  let bound = rule.bind_subject_v1(accepted_subject(), witness_hash, 0)
  let state = rule.authenticate_item_v1(
    bound,
    fixture.signature_script_item_v1(),
    scan_hash,
  )
  and {
    rule.state_is_authentic_v1(state),
    state.checkpoint_hash != rule.checkpoint_v1(
      state.bound,
      state.total_length,
      state.item_commitment,
      state.control_cbor,
      final_hash,
    ),
  }
}

test closes_both_verdict_directions_with_exact_polarity() {
  let accepted_bound = rule.bind_subject_v1(accepted_subject(), witness_hash, 0)
  let malformed = rule.authenticate_item_v1(
    accepted_bound,
    fixture.malformed_wrapper_item_v1(),
    scan_hash,
  )
  let accepted_closed = rule.closed_state_v1(
    malformed,
    malformed.result_class,
    final_hash,
  )
  let forced_bound = rule.bind_subject_v1(
    forced_header_subject(),
    witness_hash,
    0,
  )
  let valid = rule.authenticate_item_v1(
    forced_bound,
    fixture.signature_script_item_v1(),
    scan_hash,
  )
  let forced_closed = rule.closed_state_v1(
    valid,
    rule.result_no_fault,
    final_hash,
  )
  and {
    rule.terminal_contradiction_v1(accepted_closed),
    rule.terminal_contradiction_v1(forced_closed),
  }
}

test honest_verdicts_do_not_convict() {
  let accepted_bound = rule.bind_subject_v1(accepted_subject(), witness_hash, 0)
  let valid = rule.authenticate_item_v1(
    accepted_bound,
    fixture.plutus_script_item_v1(),
    scan_hash,
  )
  let accepted_closed = rule.closed_state_v1(
    valid,
    rule.result_no_fault,
    final_hash,
  )
  let forced_bound = rule.bind_subject_v1(
    forced_header_subject(),
    witness_hash,
    0,
  )
  let malformed = rule.authenticate_item_v1(
    forced_bound,
    fixture.malformed_wrapper_item_v1(),
    scan_hash,
  )
  let forced_closed = rule.closed_state_v1(
    malformed,
    malformed.result_class,
    final_hash,
  )
  and {
    !rule.terminal_contradiction_v1(accepted_closed),
    !rule.terminal_contradiction_v1(forced_closed),
  }
}

/// Family parity at the frozen engine's exact node boundary: consuming the
/// 16,384th node is admissible; only the adjacent node is a class-2 family
/// result (`WitnessNativeScriptNodeLimit`).
test exact_node_boundary_is_admitted() {
  let node = fixture.signature_node_bytes_v1()
  let control = native_script_scan_v1.NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(node),
    stack_root: #"",
    stack_depth: 0,
    node_count: native_script_scan_v1.max_native_script_nodes - 1,
  }
  expect scan_engine.ScanAdvancedV1 { control: terminal } = scan_engine.budgeted_scan_v1(
    control,
    Some(scan_engine.ScanWindowV1 { bytes: node, start_offset: 0 }),
    [],
    2,
  )
  and {
    terminal.node_count == native_script_scan_v1.max_native_script_nodes,
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
  }
}

test adjacent_node_over_bound_is_refused() {
  let node = fixture.signature_node_bytes_v1()
  let control = native_script_scan_v1.NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(node),
    stack_root: #"",
    stack_depth: 0,
    node_count: native_script_scan_v1.max_native_script_nodes,
  }
  expect scan_engine.ScanRefusedV1 { refusal_class } = scan_engine.budgeted_scan_v1(
    control,
    Some(scan_engine.ScanWindowV1 { bytes: node, start_offset: 0 }),
    [],
    1,
  )
  rule.mapped_refusal_class_v1(refusal_class) == rule.result_node_limit
}

/// As above for nesting depth: a container may enter depth 16,384, while the
/// adjacent push is deterministically classified as the depth-limit reason.
test exact_depth_boundary_is_admitted() {
  let payload = bytearray.drop(fixture.all_of_two_signatures_item_v1(), 4)
  let occupied_root = native_script_scan_v1.hash_frame_v1(
    native_script_scan_v1.NativeScriptFrameV1 {
      tail: #"",
      kind: native_script_scan_v1.all_node,
      child_count: 2,
      remaining: 2,
      valid_count: 0,
      required: 0,
    },
  )
  let control = native_script_scan_v1.NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(payload),
    stack_root: occupied_root,
    stack_depth: native_script_scan_v1.max_native_script_depth - 1,
    node_count: 1,
  }
  expect scan_engine.ScanAdvancedV1 { control: next } = scan_engine.budgeted_scan_v1(
    control,
    Some(scan_engine.ScanWindowV1 { bytes: payload, start_offset: 0 }),
    [],
    1,
  )
  next.stack_depth == native_script_scan_v1.max_native_script_depth
}

test adjacent_depth_over_bound_is_refused() {
  let payload = bytearray.drop(fixture.all_of_two_signatures_item_v1(), 4)
  let occupied_root = native_script_scan_v1.hash_frame_v1(
    native_script_scan_v1.NativeScriptFrameV1 {
      tail: #"",
      kind: native_script_scan_v1.all_node,
      child_count: 2,
      remaining: 2,
      valid_count: 0,
      required: 0,
    },
  )
  let control = native_script_scan_v1.NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(payload),
    stack_root: occupied_root,
    stack_depth: native_script_scan_v1.max_native_script_depth,
    node_count: 1,
  }
  expect scan_engine.ScanRefusedV1 { refusal_class } = scan_engine.budgeted_scan_v1(
    control,
    Some(scan_engine.ScanWindowV1 { bytes: payload, start_offset: 0 }),
    [],
    1,
  )
  rule.mapped_refusal_class_v1(refusal_class) == rule.result_depth_limit
}

fn forced_subject_with(reason: RejectionReasonV1) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: accepted_tx_id,
    source_key: #"01",
    rejection_reason: Some(reason),
  }
}

/// `[0, h'']` decodes its wrapper, so it is not a header fault; canonical
/// validation opens the token scan at the item end and refuses, which is the
/// structural class. Step 02 must fix that class itself: the frozen control
/// cannot be initialised over an empty payload region.
test empty_tag0_payload_is_the_native_malformed_class() {
  let item = fixture.empty_payload_item_v1()
  let accepted = rule.authenticate_item_v1(
    rule.bind_subject_v1(accepted_subject(), witness_hash, 0),
    item,
    scan_hash,
  )
  let honest = rule.authenticate_item_v1(
    rule.bind_subject_v1(
      forced_subject_with(WitnessNativeScriptMalformed { script_index: 0 }),
      witness_hash,
      0,
    ),
    item,
    scan_hash,
  )
  let wrong_class = rule.authenticate_item_v1(
    rule.bind_subject_v1(forced_header_subject(), witness_hash, 0),
    item,
    scan_hash,
  )
  and {
    accepted.result_class == rule.result_native_malformed,
    accepted.control_cbor == #"",
    rule.state_is_authentic_v1(accepted),
    rule.terminal_contradiction_v1(
      rule.closed_state_v1(accepted, accepted.result_class, final_hash),
    ),
    !rule.terminal_contradiction_v1(
      rule.closed_state_v1(honest, honest.result_class, final_hash),
    ),
    rule.terminal_contradiction_v1(
      rule.closed_state_v1(wrong_class, wrong_class.result_class, final_hash),
    ),
  }
}

/// Every forced arm binds its own class; the terminal predicate convicts
/// exactly when the authenticated decoder result is a different class.
test binds_every_accused_class_and_contradicts_only_a_different_result() {
  let reasons = [
    Pair(
      WitnessScriptHeaderMalformed { script_index: 2 },
      rule.result_header_malformed,
    ),
    Pair(
      WitnessNativeScriptMalformed { script_index: 2 },
      rule.result_native_malformed,
    ),
    Pair(
      WitnessNativeScriptNodeLimit { script_index: 2 },
      rule.result_node_limit,
    ),
    Pair(
      WitnessNativeScriptDepthLimit { script_index: 2 },
      rule.result_depth_limit,
    ),
  ]
  list.all(
    reasons,
    fn(entry) {
      let Pair(reason, accused) = entry
      let bound = rule.bind_subject_v1(
        forced_subject_with(reason),
        witness_hash,
        2,
      )
      let state = rule.authenticate_item_v1(
        bound,
        fixture.signature_script_item_v1(),
        scan_hash,
      )
      let other = if accused == rule.result_node_limit {
        rule.result_depth_limit
      } else {
        rule.result_node_limit
      }
      and {
        bound.accused_class == accused,
        rule.terminal_contradiction_v1(
          rule.closed_state_v1(state, rule.result_no_fault, final_hash),
        ),
        rule.terminal_contradiction_v1(
          rule.closed_state_v1(state, other, final_hash),
        ),
        !rule.terminal_contradiction_v1(
          rule.closed_state_v1(state, accused, final_hash),
        ),
      }
    },
  )
}

test refuses_a_negative_coordinate() fail {
  expect _bound = rule.bind_subject_v1(accepted_subject(), witness_hash, -1)
  True
}

test refuses_a_substituted_witness_set_hash_width() fail {
  expect _bound = rule.bind_subject_v1(
    accepted_subject(),
    bytearray.drop(witness_hash, 1),
    0,
  )
  True
}

/// A checkpoint that does not recompute over the carried fields is refused
/// before any verdict is read.
test refuses_a_malformed_checkpoint() fail {
  let state = rule.closed_state_v1(
    rule.authenticate_item_v1(
      rule.bind_subject_v1(accepted_subject(), witness_hash, 0),
      fixture.malformed_wrapper_item_v1(),
      scan_hash,
    ),
    rule.result_header_malformed,
    final_hash,
  )
  let tampered = rule.WitnessScriptScanStateV1 {
    ..state,
    checkpoint_hash: witness_hash,
  }
  expect rule.terminal_contradiction_v1(tampered)
  True
}

/// Rewriting the next expected script (the successor) without recomputing
/// the checkpoint is refused: the successor is part of the commitment.
test refuses_a_substituted_successor_in_the_checkpoint() fail {
  let state = rule.authenticate_item_v1(
    rule.bind_subject_v1(accepted_subject(), witness_hash, 0),
    fixture.signature_script_item_v1(),
    scan_hash,
  )
  let rerouted = rule.WitnessScriptScanStateV1 {
    ..state,
    next_expected_script_hash: final_hash,
  }
  expect rule.state_is_authentic_v1(rerouted)
  True
}

/// A non-native language is a successful decoder result: an accepted block
/// carrying it is honest and the closed state never convicts.
test closed_non_native_result_is_honest_for_an_accepted_block() {
  let state = rule.closed_state_v1(
    rule.authenticate_item_v1(
      rule.bind_subject_v1(accepted_subject(), witness_hash, 0),
      fixture.plutus_script_item_v1(),
      scan_hash,
    ),
    rule.result_no_fault,
    final_hash,
  )
  !rule.terminal_contradiction_v1(state)
}
