use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{slice_len}

pub type CborHeadV1 {
  next_offset: Int,
  value: Int,
}

pub type CborBytesV1 {
  next_offset: Int,
  value: ByteArray,
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
        read_big_endian(bytes, offset + 1, remaining - 1, value * 256 + next)
    }
  }
}

/// Reads one definite, shortest-form CBOR argument for the requested major
/// type. Indefinite forms, reserved additional-info values, and non-minimal
/// integer/length encodings fail closed.
pub fn head_at_v1(
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

pub fn bytes_at_v1(bytes: ByteArray, offset: Int) -> Option<CborBytesV1> {
  when head_at_v1(bytes, offset, 2) is {
    None -> None
    Some(head) ->
      if head.next_offset + head.value <= bytearray.length(bytes) {
        Some(
          CborBytesV1 {
            next_offset: head.next_offset + head.value,
            value: slice_len(bytes, head.next_offset, head.value),
          },
        )
      } else {
        None
      }
  }
}
