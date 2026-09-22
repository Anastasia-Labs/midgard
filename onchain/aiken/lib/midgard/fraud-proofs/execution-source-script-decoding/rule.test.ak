use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/execution_source_script_decoding/rule
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  ExecutionNativeScriptDepthLimit, ExecutionNativeScriptMalformed,
  ExecutionNativeScriptNodeLimit, RejectionReasonV1,
}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

const h28 = #"22222222222222222222222222222222222222222222222222222222"

const scan_hash = #"33333333333333333333333333333333333333333333333333333333"

const final_hash = #"44444444444444444444444444444444444444444444444444444444"

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

fn forced_subject(reason: RejectionReasonV1) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"00",
    rejection_reason: Some(reason),
  }
}

fn item_proof(item: ByteArray) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 6,
    item_index: 0,
    total_length: bytearray.length(item),
    chunk_index: 0,
    chunk: item,
    frontier: validation_merkle_v1.append_leaf(
      0,
      [],
      bounded_item_v1.hash_chunk(6, 0, 0, item),
    ),
    siblings: [],
  }
}

fn direct_source(
  subject: substrate.VerdictSubjectV1,
  item: ByteArray,
) -> rule.AuthenticatedExecutionSourceV1 {
  rule.AuthenticatedExecutionSourceV1 {
    // The real bind, so a forced subject's accused class is exactly the one
    // its typed reason names (malformed 0, node limit 1, depth limit 2).
    bound: rule.bind_execution_v1(subject, h32, 1, 0),
    prior_ledger_root: h32,
    source_index: 0,
    origin_kind: 0,
    source_key: #"00",
    language_tag: 0,
    script_hash: h28,
    total_length: bytearray.length(item),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item),
  }
}

fn control_for(
  purpose_leaf: ByteArray,
  source_leaf: ByteArray,
  execution_leaf: ByteArray,
) -> vm_machine_types.NativeScriptsControlV1 {
  let purpose = validation_merkle_v1.build_frontier([purpose_leaf])
  let source = validation_merkle_v1.build_frontier([source_leaf])
  let execution = validation_merkle_v1.build_frontier([execution_leaf])
  vm_machine_types.NativeScriptsControlV1 {
    compact_cbor: #"",
    witness_set_compact_cbor: #"",
    field_preimage_lengths_cbor: #"",
    context_cbor: #"",
    resolved_input_count: 0,
    resolved_inputs_accumulator: h32,
    spend_input_count: 0,
    resolved_item_peaks: [],
    signer_count: 0,
    signer_frontier_commitment: h32,
    source_count: 1,
    source_peaks: source.peaks,
    redeemer_count: 0,
    redeemer_peaks: [],
    purpose_count: 1,
    purpose_peaks: purpose.peaks,
    output_count: 0,
    output_peaks: [],
    output_descriptor_peaks: [],
    mint_count: 0,
    mint_peaks: [],
    execution_count: 1,
    execution_peaks: execution.peaks,
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: h32,
  }
}

fn authenticated_source() -> rule.AuthenticatedExecutionSourceV1 {
  let item = fixture.signature_script_item_v1()
  let item_commitment = bounded_item_v1.from_bytes(6, 0, item)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h28, #"aa")
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    0,
    h28,
    bytearray.length(item),
    item_commitment,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    0,
    purpose_leaf,
    source_leaf,
    #"",
  )
  let control = control_for(purpose_leaf, source_leaf, execution_leaf)
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let state = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: blake2b_256(cbor.serialise(event_key)),
    transaction_id: tx_id,
    transaction_commitment: h32,
    validation_context_hash: h32,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: h32,
    phase: validation_trace_v1.NativeScripts,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.NativeScripts,
      7,
      vm_witness.encode_native_scripts_control_v1(control),
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    ledger_delta_root: h32,
  }
  let state_hash = validation_trace_v1.hash_machine_state(state)
  let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_leaf_hash(state_hash),
    step_count: 0,
    initial_state_hash: state_hash,
    terminal_state_hash: state_hash,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
  }
  let phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(event_key),
      cbor.serialise(descriptor),
      [],
    ),
  )
  let membership = transition_trace.RootMembershipProof {
    domain: transition_trace.ValidationTracesRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ValidationTracesRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: event_key,
    value: descriptor,
    proof: [],
  }
  let bound = rule.bind_execution_v1(accepted_subject(), membership.root, 1, 0)
  rule.authenticate_execution_source_v1(
    bound,
    membership,
    state,
    validation_trace_v1.ValidationTraceProof {
      state_index: 0,
      state_hash,
      siblings: [],
    },
    control,
    0,
    0,
    h28,
    #"aa",
    [],
    0,
    0,
    #"00",
    0,
    bytearray.length(item),
    item_commitment,
    [],
    #"",
    [],
  )
}

test authenticates_the_exact_execution_and_source_frontiers() {
  let source = authenticated_source()
  and {
    source.bound.execution_index == 0,
    source.source_index == 0,
    source.item_commitment == bounded_item_v1.from_bytes(
      6,
      0,
      fixture.signature_script_item_v1(),
    ),
  }
}

test binds_exact_reason_and_refuses_coordinate_substitution() fail {
  rule.bind_execution_v1(
    forced_subject(ExecutionNativeScriptMalformed { execution_index: 0 }),
    h32,
    1,
    1,
  ) == rule.bind_execution_v1(accepted_subject(), h32, 1, 0)
}

test header_and_payload_malformed_map_to_the_same_execution_class() {
  let header_item = fixture.malformed_wrapper_item_v1()
  let payload_item = fixture.malformed_payload_item_v1()
  let header = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), header_item),
    item_proof(header_item),
    scan_hash,
  )
  let payload = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), payload_item),
    item_proof(payload_item),
    scan_hash,
  )
  and {
    header.result_class == rule.result_malformed,
    payload.result_class == rule.result_pending,
  }
}

/// The bind is the canonical decoder's twin: a tag-0 script with an empty
/// payload (`[0, h'']`) is refused by canonical validation, so the exact item
/// closes malformed at bind for an accepted subject — it never aborts and it
/// never opens a scan over a zero-length region. A non-native wrapper is no
/// fault of this family, and a decodable native wrapper opens the scan.
test empty_native_payload_closes_malformed_at_bind() {
  let empty = fixture.empty_payload_item_v1()
  let plutus = fixture.plutus_script_item_v1()
  let native = fixture.signature_script_item_v1()
  let bind = fn(item: ByteArray) {
    rule.bind_exact_item_v1(
      direct_source(accepted_subject(), item),
      item_proof(item),
      scan_hash,
    )
  }
  let closed = rule.closed_state_v1(
    bind(empty),
    rule.result_malformed,
    final_hash,
  )
  and {
    bind(empty).result_class == rule.result_malformed,
    bind(empty).control_cbor == #"",
    rule.terminal_contradiction_v1(closed),
    bind(plutus).result_class == rule.result_no_fault,
    bind(native).result_class == rule.result_pending,
  }
}

test scan_resume_changes_checkpoint_and_reaches_exact_terminal() {
  let item = fixture.signature_script_item_v1()
  let state = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), item),
    item_proof(item),
    scan_hash,
  )
  let control = native_script_scan_v1.decode_structure_control_v1(
    state.control_cbor,
  )
  let window = rule.authenticated_window_v1(
    state,
    control,
    item_proof(item),
    None,
  )
  expect scan_engine.ScanAdvancedV1 { control: terminal } = scan_engine.budgeted_scan_v1(
    control,
    Some(window),
    [],
    2,
  )
  let closed = rule.closed_state_v1(state, rule.result_no_fault, final_hash)
  and {
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    closed.checkpoint_hash != state.checkpoint_hash,
    rule.state_is_authentic_v1(closed),
  }
}

test refuses_an_adjacent_chunk_for_a_single_chunk_item() fail {
  let item = fixture.signature_script_item_v1()
  let proof = item_proof(item)
  let state = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), item),
    proof,
    scan_hash,
  )
  let control = native_script_scan_v1.decode_structure_control_v1(
    state.control_cbor,
  )
  expect _window = rule.authenticated_window_v1(
    state,
    control,
    proof,
    Some(proof),
  )
  True
}

test refuses_a_substituted_source_item_chunk() fail {
  let item = fixture.signature_script_item_v1()
  let state = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), item),
    item_proof(item),
    scan_hash,
  )
  let control = native_script_scan_v1.decode_structure_control_v1(
    state.control_cbor,
  )
  expect _window = rule.authenticated_window_v1(
    state,
    control,
    item_proof(fixture.malformed_payload_item_v1()),
    None,
  )
  True
}

test exact_node_boundary_advances_and_adjacent_node_refuses() {
  let node = fixture.signature_node_bytes_v1()
  let at_boundary = native_script_scan_v1.NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(node),
    stack_root: #"",
    stack_depth: 0,
    node_count: native_script_scan_v1.max_native_script_nodes - 1,
  }
  let adjacent = native_script_scan_v1.NativeScriptStructureControlV1 {
    ..at_boundary,
    node_count: native_script_scan_v1.max_native_script_nodes,
  }
  expect scan_engine.ScanAdvancedV1 { control: advanced } = scan_engine.budgeted_scan_v1(
    at_boundary,
    Some(scan_engine.ScanWindowV1 { bytes: node, start_offset: 0 }),
    [],
    1,
  )
  and {
    advanced.node_count == native_script_scan_v1.max_native_script_nodes,
    scan_engine.budgeted_scan_v1(
      adjacent,
      Some(scan_engine.ScanWindowV1 { bytes: node, start_offset: 0 }),
      [],
      1,
    ) == scan_engine.ScanRefusedV1 {
      refusal_class: scan_engine.refusal_class_node_limit,
    },
  }
}

test exact_depth_boundary_advances_and_adjacent_depth_refuses() {
  let payload = bytearray.drop(fixture.all_of_two_signatures_item_v1(), 4)
  let at_boundary = native_script_scan_v1.NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(payload),
    stack_root: h32,
    stack_depth: native_script_scan_v1.max_native_script_depth - 1,
    node_count: 1,
  }
  let adjacent = native_script_scan_v1.NativeScriptStructureControlV1 {
    ..at_boundary,
    stack_depth: native_script_scan_v1.max_native_script_depth,
  }
  expect scan_engine.ScanAdvancedV1 { control: advanced } = scan_engine.budgeted_scan_v1(
    at_boundary,
    Some(scan_engine.ScanWindowV1 { bytes: payload, start_offset: 0 }),
    [],
    1,
  )
  and {
    advanced.stack_depth == native_script_scan_v1.max_native_script_depth,
    scan_engine.budgeted_scan_v1(
      adjacent,
      Some(scan_engine.ScanWindowV1 { bytes: payload, start_offset: 0 }),
      [],
      1,
    ) == scan_engine.ScanRefusedV1 {
      refusal_class: scan_engine.refusal_class_depth_limit,
    },
  }
}

test malformed_acceptance_and_decodable_wrongful_rejection_close() {
  let malformed_item = fixture.malformed_wrapper_item_v1()
  let accepted = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), malformed_item),
    item_proof(malformed_item),
    scan_hash,
  )
    |> rule.closed_state_v1(rule.result_malformed, final_hash)
  let valid_item = fixture.signature_script_item_v1()
  let forced = rule.bind_exact_item_v1(
    direct_source(
      forced_subject(ExecutionNativeScriptMalformed { execution_index: 0 }),
      valid_item,
    ),
    item_proof(valid_item),
    scan_hash,
  )
    |> rule.closed_state_v1(rule.result_no_fault, final_hash)
  and {
    rule.terminal_contradiction_v1(accepted),
    rule.terminal_contradiction_v1(forced),
  }
}

test honest_results_do_not_convict() {
  let valid_item = fixture.plutus_script_item_v1()
  let accepted = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), valid_item),
    item_proof(valid_item),
    scan_hash,
  )
    |> rule.closed_state_v1(rule.result_no_fault, final_hash)
  let bad_item = fixture.malformed_wrapper_item_v1()
  let forced = rule.bind_exact_item_v1(
    direct_source(
      forced_subject(ExecutionNativeScriptMalformed { execution_index: 0 }),
      bad_item,
    ),
    item_proof(bad_item),
    scan_hash,
  )
    |> rule.closed_state_v1(rule.result_malformed, final_hash)
  and {
    !rule.terminal_contradiction_v1(accepted),
    !rule.terminal_contradiction_v1(forced),
  }
}

/// `all [sig, sig]` cut after two primitive steps lands in frame stage. The
/// resumed segment still reads its remaining tokens from the chunk holding
/// the checkpointed cursor (frame steps never move it), so step 04 accepts
/// a window there exactly as the frozen engine's own thread does; without
/// one the fold stalls after the frame step instead of reaching the terminal.
test frame_stage_segment_reads_tokens_through_the_cursor_window() {
  let item = fixture.all_of_two_signatures_item_v1()
  let opened = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), item),
    item_proof(item),
    scan_hash,
  )
  let control = native_script_scan_v1.decode_structure_control_v1(
    opened.control_cbor,
  )
  expect scan_engine.ScanAdvancedV1 { control: mid } = scan_engine.budgeted_scan_v1(
    control,
    Some(rule.authenticated_window_v1(opened, control, item_proof(item), None)),
    [],
    2,
  )
  expect mid.stage == native_script_scan_v1.structure_stage_frame
  let resumed = rule.advanced_state_v1(opened, mid, scan_hash)
  let frame = native_script_scan_v1.NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.all_node,
    child_count: 2,
    remaining: 2,
    valid_count: 0,
    required: 0,
  }
  let frames = [
    frame,
    native_script_scan_v1.NativeScriptFrameV1 { ..frame, remaining: 1 },
  ]
  expect scan_engine.ScanAdvancedV1 { control: terminal } = scan_engine.budgeted_scan_v1(
    mid,
    Some(rule.authenticated_window_v1(resumed, mid, item_proof(item), None)),
    frames,
    4,
  )
  expect scan_engine.ScanAdvancedV1 { control: stalled } = scan_engine.budgeted_scan_v1(
    mid,
    None,
    frames,
    4,
  )
  and {
    resumed.checkpoint_hash != opened.checkpoint_hash,
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    terminal.node_count == 3,
    stalled.stage == native_script_scan_v1.structure_stage_token,
    stalled.node_count == 2,
  }
}

/// Every reason arm in the wrongful-rejection direction: a decodable item
/// scans to the terminal, so an operator's node-limit or depth-limit
/// rejection contradicts the canonical result exactly as a malformed one.
test forced_node_and_depth_accusations_convict_on_a_decodable_item() {
  let item = fixture.signature_script_item_v1()
  let closed = fn(reason: RejectionReasonV1) {
    rule.bind_exact_item_v1(
      direct_source(forced_subject(reason), item),
      item_proof(item),
      scan_hash,
    )
      |> rule.closed_state_v1(rule.result_no_fault, final_hash)
  }
  let node = closed(ExecutionNativeScriptNodeLimit { execution_index: 0 })
  let depth = closed(ExecutionNativeScriptDepthLimit { execution_index: 0 })
  and {
    node.source.bound.accused_class == rule.result_node_limit,
    depth.source.bound.accused_class == rule.result_depth_limit,
    rule.terminal_contradiction_v1(node),
    rule.terminal_contradiction_v1(depth),
  }
}

/// The exact-code twin: a node-limit rejection is honest only when the scan
/// result is the node-limit class, and likewise for depth. Any other class,
/// including the sibling bound, is a wrong rejection code and convicts.
test honest_node_and_depth_rejections_do_not_convict() {
  let item = fixture.all_of_two_signatures_item_v1()
  let closed = fn(reason: RejectionReasonV1, result: Int) {
    rule.bind_exact_item_v1(
      direct_source(forced_subject(reason), item),
      item_proof(item),
      scan_hash,
    )
      |> rule.closed_state_v1(result, final_hash)
  }
  let node = ExecutionNativeScriptNodeLimit { execution_index: 0 }
  let depth = ExecutionNativeScriptDepthLimit { execution_index: 0 }
  and {
    !rule.terminal_contradiction_v1(closed(node, rule.result_node_limit)),
    !rule.terminal_contradiction_v1(closed(depth, rule.result_depth_limit)),
    rule.terminal_contradiction_v1(closed(node, rule.result_depth_limit)),
    rule.terminal_contradiction_v1(closed(depth, rule.result_node_limit)),
    rule.terminal_contradiction_v1(closed(node, rule.result_malformed)),
  }
}

/// Every reason arm in the wrongful-acceptance direction: an accepted block
/// whose execution source refuses with any of the three classes is convicted,
/// and the engine's refusal classes map one-to-one onto them.
test accepted_node_and_depth_results_convict() {
  let item = fixture.all_of_two_signatures_item_v1()
  let closed = fn(result: Int) {
    rule.bind_exact_item_v1(
      direct_source(accepted_subject(), item),
      item_proof(item),
      scan_hash,
    )
      |> rule.closed_state_v1(result, final_hash)
  }
  and {
    rule.terminal_contradiction_v1(closed(rule.result_node_limit)),
    rule.terminal_contradiction_v1(closed(rule.result_depth_limit)),
    rule.terminal_contradiction_v1(closed(rule.result_malformed)),
    !rule.terminal_contradiction_v1(closed(rule.result_no_fault)),
    rule.mapped_refusal_class_v1(scan_engine.refusal_class_malformed) == rule.result_malformed,
    rule.mapped_refusal_class_v1(scan_engine.refusal_class_node_limit) == rule.result_node_limit,
    rule.mapped_refusal_class_v1(scan_engine.refusal_class_depth_limit) == rule.result_depth_limit,
  }
}

test binds_node_limit_reason_to_its_exact_execution_coordinate() fail {
  rule.bind_execution_v1(
    forced_subject(ExecutionNativeScriptNodeLimit { execution_index: 1 }),
    h32,
    1,
    0,
  ) == rule.bind_execution_v1(
    forced_subject(ExecutionNativeScriptNodeLimit { execution_index: 0 }),
    h32,
    1,
    0,
  )
}

test binds_depth_limit_reason_to_its_exact_execution_coordinate() fail {
  rule.bind_execution_v1(
    forced_subject(ExecutionNativeScriptDepthLimit { execution_index: 1 }),
    h32,
    1,
    0,
  ) == rule.bind_execution_v1(
    forced_subject(ExecutionNativeScriptDepthLimit { execution_index: 0 }),
    h32,
    1,
    0,
  )
}

/// A tampered checkpoint is never a terminal: the datum's own hash must
/// recompute from the carried source, control, and successor.
test refuses_a_substituted_checkpoint() fail {
  let item = fixture.malformed_payload_item_v1()
  let closed = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), item),
    item_proof(item),
    scan_hash,
  )
    |> rule.closed_state_v1(rule.result_malformed, final_hash)
  rule.terminal_contradiction_v1(
    rule.ExecutionSourceScanStateV1 { ..closed, checkpoint_hash: h32 },
  )
}

/// The successor is part of the checkpoint: swapping the next expected
/// script without recomputing the checkpoint leaves an inauthentic state,
/// and recomputing it for a different successor yields a different hash.
test wrong_successor_is_bound_into_the_checkpoint() fail {
  let item = fixture.malformed_payload_item_v1()
  let opened = rule.bind_exact_item_v1(
    direct_source(accepted_subject(), item),
    item_proof(item),
    scan_hash,
  )
  let to_final = rule.closed_state_v1(opened, rule.result_malformed, final_hash)
  let to_scan = rule.closed_state_v1(opened, rule.result_malformed, scan_hash)
  expect to_final.checkpoint_hash != to_scan.checkpoint_hash
  rule.terminal_contradiction_v1(
    rule.ExecutionSourceScanStateV1 {
      ..to_final,
      next_expected_script_hash: scan_hash,
    },
  )
}
