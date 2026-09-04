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
  ExecutionNativeScriptMalformed, RejectionReasonV1,
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
    bound: rule.BoundExecutionV1 {
      subject,
      validation_traces_root: h32,
      validation_trace_count: 1,
      execution_index: 0,
      accused_class: if subject.direction == substrate.direction_wrongful_rejection {
        rule.result_malformed
      } else {
        rule.result_pending
      },
    },
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
