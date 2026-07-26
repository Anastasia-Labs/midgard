use aiken/primitive/bytearray
use midgard/native_script_scan_v1

const cross_language_script =
  #"830301828200581c444444444444444444444444444444444444444444444444444444448205182a"

const cross_language_terminal_control =
  #"8801030018281828400003"

fn advanced(
  result: Option<native_script_scan_v1.NativeScriptStructureStepResultV1>,
) -> native_script_scan_v1.NativeScriptStructureControlV1 {
  expect
    Some(native_script_scan_v1.NativeScriptStructureAdvanced { control })
    = result
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
    native_script_scan_v1.structure_frame_step_v1(
      signature,
      root_frame,
    ),
  )
  let timelock = advanced(
    native_script_scan_v1.structure_token_step_v1(
      after_signature,
      cross_language_script,
      after_signature.cursor,
    ),
  )
  let after_timelock = advanced(
    native_script_scan_v1.structure_frame_step_v1(
      timelock,
      last_frame,
    ),
  )
  let terminal = advanced(
    native_script_scan_v1.finalize_structure_v1(after_timelock),
  )
  and {
    terminal.node_count == 3,
    native_script_scan_v1.structure_terminal_is_exact_v1(terminal),
    native_script_scan_v1.encode_structure_control_v1(terminal)
      == cross_language_terminal_control,
  }
}

test decodes_the_typescript_terminal_control_canonically() {
  let control = native_script_scan_v1.decode_structure_control_v1(
    cross_language_terminal_control,
  )
  and {
    native_script_scan_v1.structure_terminal_is_exact_v1(control),
    native_script_scan_v1.encode_structure_control_v1(control)
      == cross_language_terminal_control,
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
  let valid_with_trailing =
    #"8200581c4444444444444444444444444444444444444444444444444444444400"
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
    trailing_finalize.stage
      == native_script_scan_v1.structure_stage_finalize,
    native_script_scan_v1.finalize_structure_v1(trailing_finalize)
      == Some(native_script_scan_v1.NativeScriptStructureInvalid),
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
  native_script_scan_v1.structure_frame_step_v1(
    signature,
    wrong_frame,
  ) == None
}
