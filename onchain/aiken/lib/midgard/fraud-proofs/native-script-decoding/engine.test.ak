//// Unit selectors for the decoding-fault engine (#635, #633): the budgeted
//// fold over the frozen scan primitives, the machine bind, the authenticated
//// chunk window, and the canonical thread-state encoder.
////
//// The limit classes (1, 2) are driven here with crafted controls: under
//// #640's 16,384-byte item cap a committed descriptor can never reach
//// 16,384 nodes or stack depth, so the validators cannot exhibit them from
//// authentic fixtures — the engine's classification must still be exact.

use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_script_decoding/engine
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/native_script_scan_v1.{
  NativeScriptFrameV1, NativeScriptStructureControlV1,
}

fn bound_control_of(item: ByteArray) -> NativeScriptStructureControlV1 {
  expect engine.MachineBoundV1 { control } = engine.bind_machine_v1(
    item,
    bytearray.length(item),
  )
  control
}

fn full_window(item: ByteArray) -> Option<engine.ScanWindowV1> {
  Some(engine.ScanWindowV1 { bytes: item, start_offset: 0 })
}

/// A signature script binds at its payload and folds to the exact canonical
/// terminal: one node, no frames.
test decoding_engine_scans_a_signature_script_to_the_exact_terminal() {
  let item = tf.signature_script_item_v1()
  let control = bound_control_of(item)
  expect engine.ScanAdvancedV1 { control: terminal } = engine.budgeted_scan_v1(
    control,
    full_window(item),
    [],
    3,
  )
  and {
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    terminal.node_count == 1,
  }
}

/// A container folds through its prover-supplied frame preimages: the frames
/// must hash-chain to the machine's own stack commitments, in consumption
/// order.
test decoding_engine_scans_a_container_through_its_frames() {
  let item = tf.all_of_two_signatures_item_v1()
  let control = bound_control_of(item)
  let first_frame = NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.all_node,
    child_count: 2,
    remaining: 2,
    valid_count: 0,
    required: 0,
  }
  expect engine.ScanAdvancedV1 { control: terminal } = engine.budgeted_scan_v1(
    control,
    full_window(item),
    [first_frame, NativeScriptFrameV1 { ..first_frame, remaining: 1 }],
    6,
  )
  and {
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    terminal.node_count == 3,
  }
}

/// The budget genuinely partitions the fold: stopping mid-scan and resuming
/// from the carried control reaches the same terminal.
test decoding_engine_scan_resumes_from_a_budget_stop() {
  let item = tf.all_of_two_signatures_item_v1()
  let control = bound_control_of(item)
  expect engine.ScanAdvancedV1 { control: mid } = engine.budgeted_scan_v1(
    control,
    full_window(item),
    [],
    2,
  )
  expect mid.stage == native_script_scan_v1.structure_stage_frame
  let first_frame = NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.all_node,
    child_count: 2,
    remaining: 2,
    valid_count: 0,
    required: 0,
  }
  expect engine.ScanAdvancedV1 { control: terminal } = engine.budgeted_scan_v1(
    mid,
    full_window(item),
    [first_frame, NativeScriptFrameV1 { ..first_frame, remaining: 1 }],
    10,
  )
  and {
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    terminal.node_count == 3,
  }
}

/// An undecodable payload token refuses with class 0.
test decoding_engine_scan_refuses_a_malformed_payload() {
  let item = tf.malformed_payload_item_v1()
  let control = bound_control_of(item)
  engine.budgeted_scan_v1(control, full_window(item), [], 1) == engine.ScanRefusedV1 {
    refusal_class: engine.refusal_class_malformed,
  }
}

/// The frozen machine's node ceiling maps to class 1. The control is crafted
/// at the ceiling because no #640-capped item can reach it authentically.
test decoding_engine_scan_classifies_the_node_limit() {
  let node = tf.signature_node_bytes_v1()
  let control = NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(node),
    stack_root: #"",
    stack_depth: 0,
    node_count: native_script_scan_v1.max_native_script_nodes,
  }
  engine.budgeted_scan_v1(
    control,
    Some(engine.ScanWindowV1 { bytes: node, start_offset: 0 }),
    [],
    1,
  ) == engine.ScanRefusedV1 { refusal_class: engine.refusal_class_node_limit }
}

/// The frozen machine's depth ceiling maps to class 2.
test decoding_engine_scan_classifies_the_depth_limit() {
  // The container payload of the all-of-two item: its first token pushes a
  // frame, which at ceiling depth is the refusal.
  let payload = bytearray.drop(tf.all_of_two_signatures_item_v1(), 4)
  let occupied_root = native_script_scan_v1.hash_frame_v1(
    NativeScriptFrameV1 {
      tail: #"",
      kind: native_script_scan_v1.all_node,
      child_count: 2,
      remaining: 2,
      valid_count: 0,
      required: 0,
    },
  )
  let control = NativeScriptStructureControlV1 {
    version: 1,
    stage: native_script_scan_v1.structure_stage_token,
    start_offset: 0,
    cursor: 0,
    end_offset: bytearray.length(payload),
    stack_root: occupied_root,
    stack_depth: native_script_scan_v1.max_native_script_depth,
    node_count: 1,
  }
  engine.budgeted_scan_v1(
    control,
    Some(engine.ScanWindowV1 { bytes: payload, start_offset: 0 }),
    [],
    1,
  ) == engine.ScanRefusedV1 { refusal_class: engine.refusal_class_depth_limit }
}

/// The safe-read margin: a window truncated short of a full token's worst
/// case makes the fold STOP, never refuse — a truncation artifact must not
/// impersonate a class-0 fault.
test decoding_engine_scan_stops_on_a_truncated_window_instead_of_refusing() {
  let item = tf.signature_script_item_v1()
  let control = bound_control_of(item)
  // Ten bytes past the cursor, with more item remaining beyond the window.
  let truncated = Some(
    engine.ScanWindowV1 { bytes: bytearray.take(item, 14), start_offset: 0 },
  )
  engine.budgeted_scan_v1(control, truncated, [], 5) == engine.ScanAdvancedV1 {
    control,
  }
}

/// A windowless fold performs no token step at all.
test decoding_engine_scan_stops_without_a_window() {
  let item = tf.signature_script_item_v1()
  let control = bound_control_of(item)
  engine.budgeted_scan_v1(control, None, [], 5) == engine.ScanAdvancedV1 {
    control,
  }
}

/// The machine bind's four-way dispatch: a native wrapper starts the machine
/// at its payload; an undecodable or empty wrapper is Malformed; a non-tag-0
/// language is NonNative.
test decoding_engine_bind_machine_dispatches_on_the_wrapper() {
  let item = tf.signature_script_item_v1()
  expect engine.MachineBoundV1 { control } = engine.bind_machine_v1(
    item,
    bytearray.length(item),
  )
  and {
    control.start_offset == 4,
    control.cursor == 4,
    control.end_offset == bytearray.length(item),
    engine.bind_machine_v1(
      tf.malformed_wrapper_item_v1(),
      bytearray.length(tf.malformed_wrapper_item_v1()),
    ) == engine.MachineBindMalformedV1,
    engine.bind_machine_v1(
      tf.empty_payload_item_v1(),
      bytearray.length(tf.empty_payload_item_v1()),
    ) == engine.MachineBindMalformedV1,
    engine.bind_machine_v1(
      tf.plutus_script_item_v1(),
      bytearray.length(tf.plutus_script_item_v1()),
    ) == engine.MachineBindNonNativeV1 { language_tag: 3 },
  }
}

/// The chunk window authenticates against the frozen item commitment and
/// carries the cursor's chunk at its absolute offset.
test decoding_engine_authenticates_a_single_chunk_window() {
  let item = tf.signature_script_item_v1()
  let item_commitment = bounded_item_v1.from_bytes(2, 0, item)
  let window = engine.authenticated_scan_window_v1(
    0,
    bytearray.length(item),
    item_commitment,
    4,
    tf.single_chunk_proof_v1(0, item),
    None,
  )
  window == engine.ScanWindowV1 { bytes: item, start_offset: 0 }
}

/// A single-chunk item admits no phantom adjacent chunk.
test decoding_engine_window_rejects_a_phantom_next_chunk() fail {
  let item = tf.signature_script_item_v1()
  let item_commitment = bounded_item_v1.from_bytes(2, 0, item)
  engine.authenticated_scan_window_v1(
    0,
    bytearray.length(item),
    item_commitment,
    4,
    tf.single_chunk_proof_v1(0, item),
    Some(tf.single_chunk_proof_v1(0, item)),
  ) != engine.ScanWindowV1 { bytes: item, start_offset: 0 }
}

/// §4's constant-size claim: the canonical thread-state encoding stays flat
/// and injective across the fields a step transition can change.
test decoding_engine_thread_state_encoding_is_flat_and_injective() {
  let descriptor = tf.scan_descriptor_v1(0, 0, tf.signature_script_item_v1())
  let pre = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_acceptance,
    engine.source_kind_normal,
    tf.h32_c,
    #"",
    engine.class_pending,
    tf.h32_c,
    engine.outpoint_source_spend,
    0,
  )
  let bound = engine.bound_scan_state_v1(pre, descriptor, #"8258205555")
  and {
    bytearray.length(engine.encode_scan_thread_state_v1(bound)) < 300,
    engine.encode_scan_thread_state_v1(pre) != engine.encode_scan_thread_state_v1(
      bound,
    ),
    engine.encode_scan_thread_state_v1(
      engine.ScanThreadStateV1 {
        ..bound,
        direction: engine.direction_wrongful_rejection,
      },
    ) != engine.encode_scan_thread_state_v1(bound),
  }
}
