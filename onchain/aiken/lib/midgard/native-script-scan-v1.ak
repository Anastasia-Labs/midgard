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
