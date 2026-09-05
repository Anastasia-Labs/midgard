use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as fixture
use midgard/fraud_proofs/output_reference_script_decoding/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  OutputReferenceScriptMalformed, OutputReferenceScriptNodeLimit,
}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const scan_hash = #"11111111111111111111111111111111111111111111111111111111"

const final_hash = #"22222222222222222222222222222222222222222222222222222222"

fn accepted_subject() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn forced_subject(index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(
      OutputReferenceScriptMalformed { output_index: index },
    ),
  }
}

fn closed_state(
  subject: substrate.VerdictSubjectV1,
  accused: Int,
  result: Int,
) -> rule.ReferenceScriptScanStateV1 {
  let bound = rule.BoundOutputV1 {
    subject,
    output_index: 0,
    accused_class: accused,
  }
  let commitment = bounded_item_v1.from_bytes(
    rule.outputs_field_index,
    0,
    fixture.signature_script_item_v1(),
  )
  rule.ReferenceScriptScanStateV1 {
    bound,
    total_length: bytearray.length(fixture.signature_script_item_v1()),
    item_commitment: commitment,
    control_cbor: #"",
    next_expected_script_hash: final_hash,
    checkpoint_hash: rule.checkpoint_v1(
      bound,
      bytearray.length(fixture.signature_script_item_v1()),
      commitment,
      #"",
      final_hash,
    ),
    result_class: result,
  }
}

test binds_exact_forced_reason_and_coordinate() {
  let bound = rule.bind_output_v1(forced_subject(2), 2)
  bound.accused_class == rule.result_malformed
}

test refuses_substituted_output_coordinate() fail {
  expect _ = rule.bind_output_v1(forced_subject(2), 1)
  True
}

test refuses_another_typed_reason() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..forced_subject(0),
    rejection_reason: Some(OutputReferenceScriptNodeLimit { output_index: 0 }),
  }
  let bound = rule.bind_output_v1(subject, 0)
  expect bound.accused_class == rule.result_malformed
  True
}

test closes_wrongful_acceptance_and_decodable_wrongful_rejection() {
  and {
    rule.terminal_contradiction_v1(
      closed_state(
        accepted_subject(),
        rule.result_pending,
        rule.result_malformed,
      ),
    ),
    rule.terminal_contradiction_v1(
      closed_state(
        forced_subject(0),
        rule.result_malformed,
        rule.result_no_fault,
      ),
    ),
  }
}

test honest_polarities_do_not_convict() {
  and {
    !rule.terminal_contradiction_v1(
      closed_state(
        accepted_subject(),
        rule.result_pending,
        rule.result_no_fault,
      ),
    ),
    !rule.terminal_contradiction_v1(
      closed_state(
        forced_subject(0),
        rule.result_malformed,
        rule.result_malformed,
      ),
    ),
  }
}

test checkpoint_refuses_successor_substitution() {
  let state = closed_state(
    accepted_subject(),
    rule.result_pending,
    rule.result_malformed,
  )
  state.checkpoint_hash != rule.checkpoint_v1(
    state.bound,
    state.total_length,
    state.item_commitment,
    state.control_cbor,
    scan_hash,
  )
}

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
  terminal.node_count == native_script_scan_v1.max_native_script_nodes && native_script_scan_v1.structure_terminal_is_exact_v1(
    terminal,
  )
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

test exact_and_adjacent_depth_boundary() {
  let payload = bytearray.drop(fixture.all_of_two_signatures_item_v1(), 4)
  let frame = native_script_scan_v1.NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.all_node,
    child_count: 2,
    remaining: 2,
    valid_count: 0,
    required: 0,
  }
  let root = native_script_scan_v1.hash_frame_v1(frame)
  let exact = native_script_scan_v1.NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(payload),
    stack_root: root,
    stack_depth: native_script_scan_v1.max_native_script_depth - 1,
    node_count: 1,
  }
  let adjacent = native_script_scan_v1.NativeScriptStructureControlV1 {
    ..exact,
    stack_depth: native_script_scan_v1.max_native_script_depth,
  }
  expect scan_engine.ScanAdvancedV1 { control: next } = scan_engine.budgeted_scan_v1(
    exact,
    Some(scan_engine.ScanWindowV1 { bytes: payload, start_offset: 0 }),
    [],
    1,
  )
  expect scan_engine.ScanRefusedV1 { refusal_class } = scan_engine.budgeted_scan_v1(
    adjacent,
    Some(scan_engine.ScanWindowV1 { bytes: payload, start_offset: 0 }),
    [],
    1,
  )
  next.stack_depth == native_script_scan_v1.max_native_script_depth && rule.mapped_refusal_class_v1(
    refusal_class,
  ) == rule.result_depth_limit
}

// ## Bind twin on real canonical outputs
//
// : the
// canonical output map with a reference script as its last (optional) field.
// The descriptor scan below is self-validating: a non-canonical hand-assembled
// output never reaches , so a wrong fixture fails the test
// rather than silently binding.

const output_head =
  #"a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a003"

fn canonical_output_with_script(item: ByteArray) -> ByteArray {
  bytearray.concat(output_head, item)
}

fn descriptor_state_for(output: ByteArray) -> rule.OutputDescriptorStateV1 {
  let bound = rule.bind_output_v1(accepted_subject(), 0)
  scan_to_terminal(rule.initial_output_scan_v1(bound, output), output, 64)
}

fn scan_to_terminal(
  state: rule.OutputDescriptorStateV1,
  output: ByteArray,
  fuel: Int,
) -> rule.OutputDescriptorStateV1 {
  if state.outcome != rule.output_scanning {
    state
  } else {
    expect fuel > 0
    scan_to_terminal(
      rule.advance_output_scan_v1(state, output),
      output,
      fuel - 1,
    )
  }
}

test empty_native_payload_closes_malformed_at_bind() {
  let output = canonical_output_with_script(fixture.empty_payload_item_v1())
  let state = descriptor_state_for(output)
  expect state.outcome == rule.output_canonical
  let scan = rule.bind_reference_script_v1(state, output, scan_hash)
  and {
    scan.result_class == rule.result_malformed,
    scan.control_cbor == #"",
    scan.total_length == 3,
    scan.next_expected_script_hash == scan_hash,
  }
}

test signature_native_payload_binds_pending() {
  let item = fixture.signature_script_item_v1()
  let output = canonical_output_with_script(item)
  let state = descriptor_state_for(output)
  expect state.outcome == rule.output_canonical
  let scan = rule.bind_reference_script_v1(state, output, scan_hash)
  let expected = native_script_scan_v1.encode_structure_control_v1(
    native_script_scan_v1.initial_structure_control_v1(4, 32),
  )
  and {
    scan.result_class == rule.result_pending,
    scan.control_cbor == expected,
    scan.total_length == bytearray.length(item),
    scan.item_commitment == bounded_item_v1.from_bytes(
      rule.outputs_field_index,
      0,
      item,
    ),
  }
}

test plutus_payload_closes_no_fault_at_bind() {
  let output = canonical_output_with_script(fixture.plutus_script_item_v1())
  let scan = rule.bind_reference_script_v1(
    descriptor_state_for(output),
    output,
    scan_hash,
  )
  scan.result_class == rule.result_no_fault && scan.control_cbor == #""
}

test window_refuses_substituted_chunk_coordinate() fail {
  let item = fixture.signature_script_item_v1()
  let output = canonical_output_with_script(item)
  let scan = rule.bind_reference_script_v1(
    descriptor_state_for(output),
    output,
    scan_hash,
  )
  let proof = fixture.single_chunk_proof_v1(0, item)
  let substituted = bounded_item_v1.ChunkProofV1 { ..proof, item_index: 1 }
  expect _ = rule.authenticated_window_v1(
    scan,
    native_script_scan_v1.decode_structure_control_v1(scan.control_cbor),
    substituted,
    None,
  )
  True
}

test window_admits_the_exact_chunk() {
  let item = fixture.signature_script_item_v1()
  let output = canonical_output_with_script(item)
  let scan = rule.bind_reference_script_v1(
    descriptor_state_for(output),
    output,
    scan_hash,
  )
  let window = rule.authenticated_window_v1(
    scan,
    native_script_scan_v1.decode_structure_control_v1(scan.control_cbor),
    fixture.single_chunk_proof_v1(0, item),
    None,
  )
  window.bytes == item && window.start_offset == 0
}

test tampered_checkpoint_is_not_authentic() {
  let state = closed_state(
    accepted_subject(),
    rule.result_pending,
    rule.result_malformed,
  )
  and {
    rule.scan_state_is_authentic_v1(state),
    !rule.scan_state_is_authentic_v1(
      rule.ReferenceScriptScanStateV1 { ..state, total_length: 1 },
    ),
    !rule.scan_state_is_authentic_v1(
      rule.ReferenceScriptScanStateV1 { ..state, checkpoint_hash: tx_id },
    ),
    !rule.scan_state_is_authentic_v1(
      rule.ReferenceScriptScanStateV1 { ..state, control_cbor: #"00" },
    ),
  }
}
