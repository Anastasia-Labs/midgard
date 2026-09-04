use aiken/primitive/bytearray
use midgard/native_script_payload_vectors_v1 as vectors
use midgard/native_script_scan_v1

const cross_language_script =
  #"830301828200581c444444444444444444444444444444444444444444444444444444448205182a"

const cross_language_terminal_control = #"8801030018281828400003"

fn advanced(
  result: Option<native_script_scan_v1.NativeScriptStructureStepResultV1>,
) -> native_script_scan_v1.NativeScriptStructureControlV1 {
  expect Some(native_script_scan_v1.NativeScriptStructureAdvanced { control }) = result
  control
}

test scans_the_cross_language_tree_iteratively() {
  let initial = native_script_scan_v1.initial_structure_control_v1(
    0,
    bytearray.length(cross_language_script),
  )
  let root_frame = native_script_scan_v1.NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.at_least_node,
    child_count: 2,
    remaining: 2,
    valid_count: 0,
    required: 1,
  }
  let last_frame = native_script_scan_v1.NativeScriptFrameV1 {
    ..root_frame,
    remaining: 1,
  }
  let root = advanced(
    native_script_scan_v1.structure_token_step_v1(
      initial,
      cross_language_script,
      0,
    ),
  )
  let signature = advanced(
    native_script_scan_v1.structure_token_step_v1(
      root,
      cross_language_script,
      root.cursor,
    ),
  )
  let after_signature = advanced(
    native_script_scan_v1.structure_frame_step_v1(signature, root_frame),
  )
  let timelock = advanced(
    native_script_scan_v1.structure_token_step_v1(
      after_signature,
      cross_language_script,
      after_signature.cursor,
    ),
  )
  let after_timelock = advanced(
    native_script_scan_v1.structure_frame_step_v1(timelock, last_frame),
  )
  let terminal = advanced(
    native_script_scan_v1.finalize_structure_v1(after_timelock),
  )
  and {
    terminal.node_count == 3,
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    native_script_scan_v1.encode_structure_control_v1(terminal) == cross_language_terminal_control,
  }
}

test decodes_the_typescript_terminal_control_canonically() {
  let control = native_script_scan_v1.decode_structure_control_v1(
    cross_language_terminal_control,
  )
  and {
    native_script_scan_v1.structure_terminal_is_exact_v1(control),
    native_script_scan_v1.encode_structure_control_v1(control) == cross_language_terminal_control,
  }
}

test one_token_successor_is_bounded() {
  let initial = native_script_scan_v1.initial_structure_control_v1(
    0,
    bytearray.length(cross_language_script),
  )
  let root = advanced(
    native_script_scan_v1.structure_token_step_v1(
      initial,
      cross_language_script,
      0,
    ),
  )
  and {
    root.stage == native_script_scan_v1.structure_stage_token,
    root.cursor == 4,
    root.stack_depth == 1,
    root.node_count == 1,
    bytearray.length(root.stack_root) == 32,
  }
}

test reports_authenticated_malformed_and_trailing_syntax() {
  let malformed = #"82004144"
  let malformed_initial = native_script_scan_v1.initial_structure_control_v1(
    0,
    bytearray.length(malformed),
  )
  let valid_with_trailing = #"8200581c4444444444444444444444444444444444444444444444444444444400"
  let trailing_initial = native_script_scan_v1.initial_structure_control_v1(
    0,
    bytearray.length(valid_with_trailing),
  )
  let trailing_finalize = advanced(
    native_script_scan_v1.structure_token_step_v1(
      trailing_initial,
      valid_with_trailing,
      0,
    ),
  )
  and {
    native_script_scan_v1.structure_token_step_v1(
      malformed_initial,
      malformed,
      0,
    ) == Some(native_script_scan_v1.NativeScriptStructureInvalid),
    trailing_finalize.stage == native_script_scan_v1.structure_stage_finalize,
    native_script_scan_v1.finalize_structure_v1(trailing_finalize) == Some(
      native_script_scan_v1.NativeScriptStructureInvalid,
    ),
  }
}

test fails_closed_for_a_wrong_frame_preimage() {
  let initial = native_script_scan_v1.initial_structure_control_v1(
    0,
    bytearray.length(cross_language_script),
  )
  let root = advanced(
    native_script_scan_v1.structure_token_step_v1(
      initial,
      cross_language_script,
      0,
    ),
  )
  let signature = advanced(
    native_script_scan_v1.structure_token_step_v1(
      root,
      cross_language_script,
      root.cursor,
    ),
  )
  let wrong_frame = native_script_scan_v1.NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.at_least_node,
    child_count: 2,
    remaining: 1,
    valid_count: 0,
    required: 1,
  }
  native_script_scan_v1.structure_frame_step_v1(signature, wrong_frame) == None
}

// --- the one-shot driver's refusals ---
//
// `payload_structure_is_canonical_v1` is dead code, blocked on the #633
// re-ruling by the ExUnits precondition it fails (see its doc comment). Dead
// code that nothing tests is dead code nobody can weaken safely, so its refusals
// are pinned here in the same `fails_closed_*` style as the staged scan's above:
// weaken the predicate and one of these goes red rather than a wave finding it
// three regenerations later.

/// Positive control. Without it every refusal below is satisfiable by a
/// predicate that returns `False` for everything.
test one_shot_admits_the_cross_language_script() {
  native_script_scan_v1.payload_structure_is_canonical_v1(cross_language_script)
}

test one_shot_fails_closed_for_an_empty_payload() {
  !native_script_scan_v1.payload_structure_is_canonical_v1(#"")
}

/// Tag 7 is neither a signature, a container, nor a timelock, so `token_at_v1`
/// makes no token of it.
test one_shot_fails_closed_for_a_malformed_payload() {
  !native_script_scan_v1.payload_structure_is_canonical_v1(
    vectors.malformed_payload,
  )
}

/// The finalize is exact-consumption: a well-formed node followed by a byte
/// nothing consumes is a refusal, not a prefix match.
test one_shot_fails_closed_for_a_trailing_byte() {
  and {
    native_script_scan_v1.payload_structure_is_canonical_v1(
      vectors.timelock_leaf,
    ),
    !native_script_scan_v1.payload_structure_is_canonical_v1(
      vectors.payload_with_trailing_byte(),
    ),
  }
}

/// A children array that declares two children and is followed by one runs the
/// payload out with a frame still open.
test one_shot_fails_closed_for_a_truncated_child_list() {
  and {
    native_script_scan_v1.payload_structure_is_canonical_v1(
      vectors.wide_payload_n(2),
    ),
    !native_script_scan_v1.payload_structure_is_canonical_v1(
      vectors.payload_with_truncated_child_list(),
    ),
  }
}

/// Node-limit refusal at the bound edge. Driven through the bound-parameterised
/// entry point, because the staged bound is 16,384 and a node costs three bytes:
/// a vector that reaches it is 49,152 bytes and roughly 3.2 billion memory units
/// to evaluate, which is neither reachable through the tag-0 arm nor affordable
/// under the session's memory guardrail. The edge is what matters and the edge
/// is bound-relative.
test one_shot_fails_closed_at_the_node_bound_edge() {
  let payload = vectors.wide_payload_n(8)
  and {
    vectors.wide_payload_nodes(8) == 9,
    native_script_scan_v1.payload_structure_is_canonical_at_bounds_v1(
      payload,
      9,
      native_script_scan_v1.max_native_script_depth,
    ),
    !native_script_scan_v1.payload_structure_is_canonical_at_bounds_v1(
      payload,
      8,
      native_script_scan_v1.max_native_script_depth,
    ),
  }
}

/// Depth-limit refusal at the bound edge, driven the same way and for the same
/// reason. The node bound is left at the staged value so that only the depth
/// bound can be what refuses.
test one_shot_fails_closed_at_the_depth_bound_edge() {
  let payload = vectors.deep_payload(27)
  and {
    vectors.deep_payload_stack_depth(27) == 8,
    native_script_scan_v1.payload_structure_is_canonical_at_bounds_v1(
      payload,
      native_script_scan_v1.max_native_script_nodes,
      8,
    ),
    !native_script_scan_v1.payload_structure_is_canonical_at_bounds_v1(
      payload,
      native_script_scan_v1.max_native_script_nodes,
      7,
    ),
  }
}

/// The default entry point is the staged machine's bounds and nothing else,
/// which is the whole claim `payload_structure_is_canonical_v1` makes about its
/// admission set. It also records why neither bound can ever fire through the
/// tag-0 arm: 16,384 nodes at three bytes each is 49,152 bytes, three times the
/// largest payload `decode_canonical_output` admits.
test one_shot_default_bounds_are_the_staged_machine_bounds() {
  let payload = vectors.wide_payload_n(8)
  and {
    native_script_scan_v1.max_native_script_nodes == 16384,
    native_script_scan_v1.max_native_script_depth == 16384,
    native_script_scan_v1.payload_structure_is_canonical_v1(payload) == native_script_scan_v1.payload_structure_is_canonical_at_bounds_v1(
      payload,
      native_script_scan_v1.max_native_script_nodes,
      native_script_scan_v1.max_native_script_depth,
    ),
    native_script_scan_v1.max_native_script_nodes * vectors.min_node_bytes > vectors.max_reachable_payload_bytes,
  }
}
