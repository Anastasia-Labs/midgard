use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes, slice_len,
}

const frame_domain =
  #"4d6964676172644e61746976655363726970745363616e4672616d655631"

pub const signature_node: Int = 0

pub const all_node: Int = 1

pub const any_node: Int = 2

pub const at_least_node: Int = 3

pub const after_node: Int = 4

pub const before_node: Int = 5

pub const max_native_script_nodes: Int = 16384

pub const max_native_script_depth: Int = 16384

pub const version: Int = 1

pub const structure_stage_token: Int = 0

pub const structure_stage_frame: Int = 1

pub const structure_stage_finalize: Int = 2

pub const structure_stage_terminal: Int = 3

pub type VersionedScriptHeaderV1 {
  language_tag: Int,
  payload_offset: Int,
  payload_length: Int,
}

pub type NativeScriptTokenV1 {
  kind: Int,
  next_offset: Int,
  child_count: Int,
  required: Int,
  key_hash: ByteArray,
  slot: Int,
}

pub type NativeScriptTokenHeadV1 {
  outer_length: Int,
  tag: Int,
  next_offset: Int,
}

pub type NativeScriptFrameV1 {
  tail: ByteArray,
  kind: Int,
  child_count: Int,
  remaining: Int,
  valid_count: Int,
  required: Int,
}

pub type NativeScriptStructureControlV1 {
  version: Int,
  stage: Int,
  start_offset: Int,
  cursor: Int,
  end_offset: Int,
  stack_root: ByteArray,
  stack_depth: Int,
  node_count: Int,
}

pub type NativeScriptStructureStepResultV1 {
  NativeScriptStructureAdvanced {
    control: NativeScriptStructureControlV1,
  }
  NativeScriptStructureInvalid
  NativeScriptStructureNodeLimit
  NativeScriptStructureDepthLimit
}

pub type NativeFrameResultV1 {
  NativeFramePending { frame: NativeScriptFrameV1 }
  NativeFrameComplete { tail: ByteArray, valid: Bool }
}

type CborHeadV1 {
  next_offset: Int,
  value: Int,
}

fn byte_at_if_present(bytes: ByteArray, offset: Int) -> Option<Int> {
  if and {
    offset >= 0,
    offset < bytearray.length(bytes),
  } {
    Some(bytearray.at(bytes, offset))
  } else {
    None
  }
}

fn read_big_endian(
  bytes: ByteArray,
  offset: Int,
  remaining: Int,
  value: Int,
) -> Option<CborHeadV1> {
  if remaining == 0 {
    Some(CborHeadV1 { next_offset: offset, value })
  } else {
    when byte_at_if_present(bytes, offset) is {
      None -> None
      Some(next) ->
        read_big_endian(
          bytes,
          offset + 1,
          remaining - 1,
          value * 256 + next,
        )
    }
  }
}

fn canonical_head(
  bytes: ByteArray,
  offset: Int,
  expected_major: Int,
) -> Option<CborHeadV1> {
  when byte_at_if_present(bytes, offset) is {
    None -> None
    Some(first) -> {
      let major = first / 32
      let additional = first % 32
      if major != expected_major {
        None
      } else if additional < 24 {
        Some(CborHeadV1 { next_offset: offset + 1, value: additional })
      } else if additional == 24 {
        when read_big_endian(bytes, offset + 1, 1, 0) is {
          Some(head) ->
            if head.value >= 24 {
              Some(head)
            } else {
              None
            }
          None -> None
        }
      } else if additional == 25 {
        when read_big_endian(bytes, offset + 1, 2, 0) is {
          Some(head) ->
            if head.value >= 256 {
              Some(head)
            } else {
              None
            }
          None -> None
        }
      } else if additional == 26 {
        when read_big_endian(bytes, offset + 1, 4, 0) is {
          Some(head) ->
            if head.value >= 65536 {
              Some(head)
            } else {
              None
            }
          None -> None
        }
      } else if additional == 27 {
        when read_big_endian(bytes, offset + 1, 8, 0) is {
          Some(head) ->
            if head.value >= 4294967296 {
              Some(head)
            } else {
              None
            }
          None -> None
        }
      } else {
        None
      }
    }
  }
}

pub fn versioned_script_header_v1(
  first_chunk: ByteArray,
  item_length: Int,
) -> Option<VersionedScriptHeaderV1> {
  when canonical_head(first_chunk, 0, 4) is {
    Some(outer) ->
      if outer.value != 2 {
        None
      } else {
        when canonical_head(first_chunk, outer.next_offset, 0) is {
          None -> None
          Some(language) ->
            when canonical_head(first_chunk, language.next_offset, 2) is {
              None -> None
              Some(payload) ->
                if and {
                  or {
                    language.value == 0,
                    language.value == 3,
                    language.value == 128,
                  },
                  payload.value >= 0,
                  payload.next_offset + payload.value == item_length,
                } {
                  Some(
                    VersionedScriptHeaderV1 {
                      language_tag: language.value,
                      payload_offset: payload.next_offset,
                      payload_length: payload.value,
                    },
                  )
                } else {
                  None
                }
            }
        }
      }
    None -> None
  }
}

fn empty_token(kind: Int, next_offset: Int) -> NativeScriptTokenV1 {
  NativeScriptTokenV1 {
    kind,
    next_offset,
    child_count: 0,
    required: 0,
    key_hash: #"",
    slot: 0,
  }
}

pub fn token_head_at_v1(
  window: ByteArray,
  window_offset: Int,
) -> Option<NativeScriptTokenHeadV1> {
  when canonical_head(window, window_offset, 4) is {
    None -> None
    Some(outer) ->
      when canonical_head(window, outer.next_offset, 0) is {
        None -> None
        Some(tag) ->
          Some(
            NativeScriptTokenHeadV1 {
              outer_length: outer.value,
              tag: tag.value,
              next_offset: tag.next_offset,
            },
          )
      }
  }
}

pub fn token_head_is_well_formed_v1(head: NativeScriptTokenHeadV1) -> Bool {
  and {
    head.tag >= signature_node,
    head.tag <= before_node,
    if head.tag == at_least_node {
      head.outer_length == 3
    } else {
      head.outer_length == 2
    },
  }
}

pub fn signature_payload_at_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
) -> Option<NativeScriptTokenV1> {
  when canonical_head(window, window_offset, 2) is {
    Some(key_hash) ->
      if and {
        key_hash.value == 28,
        key_hash.next_offset + 28 <= bytearray.length(window),
      } {
        Some(
          NativeScriptTokenV1 {
            ..empty_token(
              signature_node,
              absolute_offset + key_hash.next_offset + 28 - window_offset,
            ),
            key_hash: slice_len(window, key_hash.next_offset, 28),
          },
        )
      } else {
        None
      }
    None -> None
  }
}

pub fn all_or_any_payload_at_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
  kind: Int,
) -> Option<NativeScriptTokenV1> {
  if and {
    kind != all_node,
    kind != any_node,
  } {
    None
  } else {
    when canonical_head(window, window_offset, 4) is {
      Some(children) ->
        if and {
          children.value >= 0,
          children.value <= max_native_script_nodes,
        } {
          Some(
            NativeScriptTokenV1 {
              ..empty_token(
                kind,
                absolute_offset + children.next_offset - window_offset,
              ),
              child_count: children.value,
            },
          )
        } else {
          None
        }
      None -> None
    }
  }
}

pub fn at_least_payload_at_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
) -> Option<NativeScriptTokenV1> {
  when canonical_head(window, window_offset, 0) is {
    None -> None
    Some(required) ->
      when canonical_head(window, required.next_offset, 4) is {
        Some(children) ->
          if and {
            required.value >= 0,
            children.value >= 0,
            children.value <= max_native_script_nodes,
          } {
            Some(
              NativeScriptTokenV1 {
                ..empty_token(
                  at_least_node,
                  absolute_offset + children.next_offset - window_offset,
                ),
                child_count: children.value,
                required: required.value,
              },
            )
          } else {
            None
          }
        None -> None
      }
  }
}

pub fn timelock_payload_at_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
  kind: Int,
) -> Option<NativeScriptTokenV1> {
  if and {
    kind != after_node,
    kind != before_node,
  } {
    None
  } else {
    when canonical_head(window, window_offset, 0) is {
      Some(slot) ->
        Some(
          NativeScriptTokenV1 {
            ..empty_token(
              kind,
              absolute_offset + slot.next_offset - window_offset,
            ),
            slot: slot.value,
          },
        )
      None -> None
    }
  }
}

pub fn signature_token_from_head_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
  head: NativeScriptTokenHeadV1,
) -> Option<NativeScriptTokenV1> {
  if head.tag != signature_node {
    None
  } else {
    when canonical_head(window, head.next_offset, 2) is {
      Some(key_hash) ->
        if and {
          head.outer_length == 2,
          key_hash.value == 28,
          key_hash.next_offset + 28 <= bytearray.length(window),
        } {
          Some(
            NativeScriptTokenV1 {
              ..empty_token(
                signature_node,
                absolute_offset + key_hash.next_offset + 28 - window_offset,
              ),
              key_hash: slice_len(window, key_hash.next_offset, 28),
            },
          )
        } else {
          None
        }
      None -> None
    }
  }
}

pub fn container_token_from_head_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
  head: NativeScriptTokenHeadV1,
) -> Option<NativeScriptTokenV1> {
  if or {
    head.tag == all_node,
    head.tag == any_node,
  } {
    when canonical_head(window, head.next_offset, 4) is {
      Some(children) ->
        if and {
          head.outer_length == 2,
          children.value >= 0,
          children.value <= max_native_script_nodes,
        } {
          Some(
            NativeScriptTokenV1 {
              ..empty_token(
                head.tag,
                absolute_offset + children.next_offset - window_offset,
              ),
              child_count: children.value,
            },
          )
        } else {
          None
        }
      None -> None
    }
  } else if head.tag == at_least_node {
    when canonical_head(window, head.next_offset, 0) is {
      None -> None
      Some(required) ->
        when canonical_head(window, required.next_offset, 4) is {
          Some(children) ->
            if and {
              head.outer_length == 3,
              required.value >= 0,
              children.value >= 0,
              children.value <= max_native_script_nodes,
            } {
              Some(
                NativeScriptTokenV1 {
                  ..empty_token(
                    at_least_node,
                    absolute_offset + children.next_offset - window_offset,
                  ),
                  child_count: children.value,
                  required: required.value,
                },
              )
            } else {
              None
            }
          None -> None
        }
    }
  } else {
    None
  }
}

pub fn timelock_token_from_head_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
  head: NativeScriptTokenHeadV1,
) -> Option<NativeScriptTokenV1> {
  if or {
    head.tag == after_node,
    head.tag == before_node,
  } {
    when canonical_head(window, head.next_offset, 0) is {
      Some(slot) ->
        if head.outer_length == 2 {
          Some(
            NativeScriptTokenV1 {
              ..empty_token(
                head.tag,
                absolute_offset + slot.next_offset - window_offset,
              ),
              slot: slot.value,
            },
          )
        } else {
          None
        }
      None -> None
    }
  } else {
    None
  }
}

pub fn token_at_v1(
  window: ByteArray,
  window_offset: Int,
  absolute_offset: Int,
) -> Option<NativeScriptTokenV1> {
  when token_head_at_v1(window, window_offset) is {
    None -> None
    Some(head) ->
      if head.tag == signature_node {
        signature_token_from_head_v1(
          window,
          window_offset,
          absolute_offset,
          head,
        )
      } else if and {
        head.tag >= all_node,
        head.tag <= at_least_node,
      } {
        container_token_from_head_v1(
          window,
          window_offset,
          absolute_offset,
          head,
        )
      } else {
        timelock_token_from_head_v1(
          window,
          window_offset,
          absolute_offset,
          head,
        )
      }
  }
}

pub fn frame_is_well_formed_v1(frame: NativeScriptFrameV1) -> Bool {
  let processed = frame.child_count - frame.remaining
  and {
    bytearray.length(frame.tail) == 0 || bytearray.length(frame.tail) == 32,
    frame.kind >= all_node,
    frame.kind <= at_least_node,
    frame.child_count > 0,
    frame.child_count <= max_native_script_nodes,
    frame.remaining > 0,
    frame.remaining <= frame.child_count,
    frame.valid_count >= 0,
    frame.valid_count <= processed,
    if frame.kind == at_least_node {
      frame.required >= 0
    } else {
      frame.required == 0
    },
  }
}

pub fn hash_frame_v1(frame: NativeScriptFrameV1) -> ByteArray {
  expect frame_is_well_formed_v1(frame)
  frame_domain
    |> bytearray.concat(encode_definite_array_header(6))
    |> bytearray.concat(encode_definite_bytes(frame.tail))
    |> bytearray.concat(cbor.serialise(frame.kind))
    |> bytearray.concat(cbor.serialise(frame.child_count))
    |> bytearray.concat(cbor.serialise(frame.remaining))
    |> bytearray.concat(cbor.serialise(frame.valid_count))
    |> bytearray.concat(cbor.serialise(frame.required))
    |> blake2b_256
}

pub fn frame_for_token_v1(
  token: NativeScriptTokenV1,
  tail: ByteArray,
) -> Option<NativeScriptFrameV1> {
  if and {
    token.kind >= all_node,
    token.kind <= at_least_node,
    token.child_count > 0,
    token.child_count <= max_native_script_nodes,
  } {
    Some(
      NativeScriptFrameV1 {
        tail,
        kind: token.kind,
        child_count: token.child_count,
        remaining: token.child_count,
        valid_count: 0,
        required: token.required,
      },
    )
  } else {
    None
  }
}

pub fn empty_container_result_v1(
  token: NativeScriptTokenV1,
) -> Option<Bool> {
  if token.child_count != 0 {
    None
  } else if token.kind == all_node {
    Some(True)
  } else if token.kind == any_node {
    Some(False)
  } else if token.kind == at_least_node {
    Some(token.required == 0)
  } else {
    None
  }
}

pub fn apply_child_v1(
  frame: NativeScriptFrameV1,
  child_valid: Bool,
) -> Option<NativeFrameResultV1> {
  if !frame_is_well_formed_v1(frame) {
    None
  } else {
    let valid_count = frame.valid_count + if child_valid {
      1
    } else {
      0
    }
    if frame.remaining == 1 {
      let valid = if frame.kind == all_node {
        valid_count == frame.child_count
      } else if frame.kind == any_node {
        valid_count > 0
      } else {
        valid_count >= frame.required
      }
      Some(NativeFrameComplete { tail: frame.tail, valid })
    } else {
      Some(
        NativeFramePending {
          frame: NativeScriptFrameV1 {
            ..frame,
            remaining: frame.remaining - 1,
            valid_count,
          },
        },
      )
    }
  }
}

fn structure_control_is_well_formed_v1(
  control: NativeScriptStructureControlV1,
) -> Bool {
  and {
    control.version == version,
    control.stage >= structure_stage_token,
    control.stage <= structure_stage_terminal,
    control.start_offset >= 0,
    control.end_offset > control.start_offset,
    control.cursor >= control.start_offset,
    control.cursor <= control.end_offset,
    control.stack_depth >= 0,
    control.stack_depth <= max_native_script_depth,
    control.node_count >= 0,
    control.node_count <= max_native_script_nodes,
    if control.stack_depth == 0 {
      control.stack_root == #""
    } else {
      bytearray.length(control.stack_root) == 32
    },
    if control.stage == structure_stage_token {
      control.cursor < control.end_offset
    } else if control.stage == structure_stage_frame {
      and {
        control.stack_depth > 0,
        bytearray.length(control.stack_root) == 32,
      }
    } else if control.stage == structure_stage_finalize {
      and {
        control.stack_depth == 0,
        control.stack_root == #"",
      }
    } else {
      and {
        control.cursor == control.end_offset,
        control.stack_depth == 0,
        control.stack_root == #"",
        control.node_count > 0,
      }
    },
  }
}

pub fn initial_structure_control_v1(
  start_offset: Int,
  total_length: Int,
) -> NativeScriptStructureControlV1 {
  let control = NativeScriptStructureControlV1 {
    version,
    stage: structure_stage_token,
    start_offset,
    cursor: start_offset,
    end_offset: start_offset + total_length,
    stack_root: #"",
    stack_depth: 0,
    node_count: 0,
  }
  expect structure_control_is_well_formed_v1(control)
  control
}

pub fn encode_structure_control_v1(
  control: NativeScriptStructureControlV1,
) -> ByteArray {
  expect structure_control_is_well_formed_v1(control)
  encode_definite_array_header(8)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.start_offset))
    |> bytearray.concat(cbor.serialise(control.cursor))
    |> bytearray.concat(cbor.serialise(control.end_offset))
    |> bytearray.concat(encode_definite_bytes(control.stack_root))
    |> bytearray.concat(cbor.serialise(control.stack_depth))
    |> bytearray.concat(cbor.serialise(control.node_count))
}

pub fn decode_structure_control_v1(
  control_cbor: ByteArray,
) -> NativeScriptStructureControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
    [
      version_data, stage_data, start_offset_data, cursor_data,
      end_offset_data, stack_root_data, stack_depth_data, node_count_data,
    ] = builtin.un_list_data(data)
  let control = NativeScriptStructureControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    start_offset: builtin.un_i_data(start_offset_data),
    cursor: builtin.un_i_data(cursor_data),
    end_offset: builtin.un_i_data(end_offset_data),
    stack_root: builtin.un_b_data(stack_root_data),
    stack_depth: builtin.un_i_data(stack_depth_data),
    node_count: builtin.un_i_data(node_count_data),
  }
  expect structure_control_is_well_formed_v1(control)
  expect encode_structure_control_v1(control) == control_cbor
  control
}

fn advanced_structure_v1(
  control: NativeScriptStructureControlV1,
) -> NativeScriptStructureStepResultV1 {
  if structure_control_is_well_formed_v1(control) {
    NativeScriptStructureAdvanced { control }
  } else {
    NativeScriptStructureInvalid
  }
}

fn complete_structure_node_v1(
  control: NativeScriptStructureControlV1,
  cursor: Int,
  node_count: Int,
) -> NativeScriptStructureStepResultV1 {
  advanced_structure_v1(
    NativeScriptStructureControlV1 {
      ..control,
      stage: if control.stack_depth > 0 {
        structure_stage_frame
      } else {
        structure_stage_finalize
      },
      cursor,
      node_count,
    },
  )
}

/// Parses one complete canonical native-script node from an authenticated
/// window. A token is smaller than one output chunk, but the caller may
/// provide the adjacent authenticated chunk when it crosses a boundary.
pub fn structure_token_step_v1(
  control: NativeScriptStructureControlV1,
  window: ByteArray,
  window_offset: Int,
) -> Option<NativeScriptStructureStepResultV1> {
  if and {
    structure_control_is_well_formed_v1(control),
    control.stage == structure_stage_token,
    window_offset >= 0,
    window_offset < bytearray.length(window),
  } {
    when token_at_v1(window, window_offset, control.cursor) is {
      None -> Some(NativeScriptStructureInvalid)
      Some(token) ->
        if or {
          token.next_offset <= control.cursor,
          token.next_offset > control.end_offset,
        } {
          Some(NativeScriptStructureInvalid)
        } else {
          let node_count = control.node_count + 1
          if node_count > max_native_script_nodes {
            Some(NativeScriptStructureNodeLimit)
          } else if and {
            token.kind >= all_node,
            token.kind <= at_least_node,
            token.child_count > 0,
          } {
            let stack_depth = control.stack_depth + 1
            if stack_depth > max_native_script_depth {
              Some(NativeScriptStructureDepthLimit)
            } else {
              when frame_for_token_v1(token, control.stack_root) is {
                None -> Some(NativeScriptStructureInvalid)
                Some(frame) ->
                  Some(
                    advanced_structure_v1(
                      NativeScriptStructureControlV1 {
                        ..control,
                        cursor: token.next_offset,
                        stack_root: hash_frame_v1(frame),
                        stack_depth,
                        node_count,
                      },
                    ),
                  )
              }
            }
          } else {
            Some(
              complete_structure_node_v1(
                control,
                token.next_offset,
                node_count,
              ),
            )
          }
        }
    }
  } else {
    None
  }
}

/// Reveals one authenticated stack-frame preimage and records completion of
/// one child. Child validity is deliberately ignored: output admission checks
/// native-script syntax here and evaluates the script only when it is used.
pub fn structure_frame_step_v1(
  control: NativeScriptStructureControlV1,
  frame: NativeScriptFrameV1,
) -> Option<NativeScriptStructureStepResultV1> {
  if and {
    structure_control_is_well_formed_v1(control),
    control.stage == structure_stage_frame,
    frame_is_well_formed_v1(frame),
    hash_frame_v1(frame) == control.stack_root,
  } {
    when apply_child_v1(frame, False) is {
      None -> None
      Some(NativeFramePending { frame: next_frame }) ->
        Some(
          advanced_structure_v1(
            NativeScriptStructureControlV1 {
              ..control,
              stage: structure_stage_token,
              stack_root: hash_frame_v1(next_frame),
            },
          ),
        )
      Some(NativeFrameComplete { tail, .. }) -> {
        let stack_depth = control.stack_depth - 1
        Some(
          advanced_structure_v1(
            NativeScriptStructureControlV1 {
              ..control,
              stage: if stack_depth > 0 {
                structure_stage_frame
              } else {
                structure_stage_finalize
              },
              stack_root: tail,
              stack_depth,
            },
          ),
        )
      }
    }
  } else {
    None
  }
}

pub fn finalize_structure_v1(
  control: NativeScriptStructureControlV1,
) -> Option<NativeScriptStructureStepResultV1> {
  if and {
    structure_control_is_well_formed_v1(control),
    control.stage == structure_stage_finalize,
  } {
    if and {
      control.cursor == control.end_offset,
      control.node_count > 0,
    } {
      Some(
        advanced_structure_v1(
          NativeScriptStructureControlV1 {
            ..control,
            stage: structure_stage_terminal,
          },
        ),
      )
    } else {
      Some(NativeScriptStructureInvalid)
    }
  } else {
    None
  }
}

pub fn structure_terminal_is_exact_v1(
  control: NativeScriptStructureControlV1,
) -> Bool {
  and {
    structure_control_is_well_formed_v1(control),
    control.stage == structure_stage_terminal,
  }
}
