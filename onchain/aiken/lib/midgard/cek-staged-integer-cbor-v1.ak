//// The canonical CBOR of large Integers, read and written in bulk.
////
//// `cek_staged_integer_v1` reads and writes an Integer's canonical CBOR one
//// 64-byte segment at a time. These entries accept exactly the same bytes and
//// produce exactly the same bytes, but do the per-segment work in bulk:
////
//// - the reader checks every full segment's head at once with one
////   `andByteString` against a mask, and takes the segment payloads eight at a
////   time, joining them in a balanced tree;
//// - the writer emits the segments eight at a time as pieces, which
////   `cek_staged_blob_v1.flat_root_of_pieces_v1` roots without joining them.
////
//// Small integers (an argument of at most 64 bytes) go through
//// `cek_staged_integer_v1` unchanged.

use aiken/builtin
use midgard/cek_staged_blob_v1.{concat_balanced_v1}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, SignedLimbsV1, encode_canonical_integer_cbor_v1,
  from_limbs_v1, parse_canonical_integer_cbor_v1, sub_limbs_v1,
}

/// The bytes of a full segment: its two-byte head and 64 payload bytes.
const segment_span = 66

/// The payload bytes of a full segment.
const segment_bytes = 64

/// The head of a full segment.
const segment_head = #"5840"

/// One full segment's mask: ones over the head, zeros over the payload.
const unit_mask =
  #"ffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

/// One full segment's head over a zero payload.
const unit_heads =
  #"584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

/// `unit` repeated to exactly `length` bytes (`length` at most 65,536).
fn repeat_to(unit: ByteArray, length: Int) -> ByteArray {
  if builtin.length_of_bytearray(unit) >= length {
    builtin.slice_bytearray(0, length, unit)
  } else {
    repeat_to(builtin.append_bytearray(unit, unit), length)
  }
}

/// True when the `full` segments from offset 2 all carry the full-segment head.
fn full_heads_hold(cbor: ByteArray, full: Int) -> Bool {
  let span = segment_span * full
  builtin.and_bytearray(
    False,
    builtin.slice_bytearray(2, span, cbor),
    repeat_to(unit_mask, span),
  ) == repeat_to(unit_heads, span)
}

/// The payloads of eight full segments whose first payload is at `at`.
fn eight_payloads(cbor: ByteArray, at: Int) -> ByteArray {
  builtin.append_bytearray(
    builtin.append_bytearray(
      builtin.append_bytearray(
        builtin.slice_bytearray(at, segment_bytes, cbor),
        builtin.slice_bytearray(at + 66, segment_bytes, cbor),
      ),
      builtin.append_bytearray(
        builtin.slice_bytearray(at + 132, segment_bytes, cbor),
        builtin.slice_bytearray(at + 198, segment_bytes, cbor),
      ),
    ),
    builtin.append_bytearray(
      builtin.append_bytearray(
        builtin.slice_bytearray(at + 264, segment_bytes, cbor),
        builtin.slice_bytearray(at + 330, segment_bytes, cbor),
      ),
      builtin.append_bytearray(
        builtin.slice_bytearray(at + 396, segment_bytes, cbor),
        builtin.slice_bytearray(at + 462, segment_bytes, cbor),
      ),
    ),
  )
}

/// The payloads of full segments `index` to `full - 1`, in order, then `tail`.
fn payloads(
  cbor: ByteArray,
  index: Int,
  full: Int,
  tail: List<ByteArray>,
) -> List<ByteArray> {
  if full - index >= 8 {
    [
      eight_payloads(cbor, 4 + segment_span * index),
      ..payloads(cbor, index + 8, full, tail)
    ]
  } else if index < full {
    [
      builtin.slice_bytearray(4 + segment_span * index, segment_bytes, cbor),
      ..payloads(cbor, index + 1, full, tail)
    ]
  } else {
    tail
  }
}

/// The argument of an indefinite tag 2 or tag 3 bignum of `length` bytes.
///
/// The segments are `full` full ones and a last one of `size` bytes (1 to 64)
/// whose head is minimal. With the tag, the indefinite head and the break, the
/// length is `3 + 66 * full + last`, where `last`, the last segment with its
/// head, is 2 to 24 or 26 to 66 bytes, so `full` is fixed by the length.
fn indefinite_argument(cbor: ByteArray, length: Int) -> ByteArray {
  expect builtin.index_bytearray(cbor, length - 1) == 0xff
  let between = length - 3
  let full = ( between - 2 ) / segment_span
  expect full >= 1
  let last_at = 2 + segment_span * full
  let last = between - segment_span * full
  let head = builtin.index_bytearray(cbor, last_at)
  let final = if last <= 24 {
    expect head == 0x3f + last
    builtin.slice_bytearray(last_at + 1, last - 1, cbor)
  } else {
    let size = last - 2
    expect head == 0x58
    expect size >= 24
    expect size <= segment_bytes
    expect builtin.index_bytearray(cbor, last_at + 1) == size
    builtin.slice_bytearray(last_at + 2, size, cbor)
  }
  expect full_heads_hold(cbor, full)
  let argument = concat_balanced_v1(payloads(cbor, 0, full, [final]))
  expect builtin.index_bytearray(argument, 0) > 0
  argument
}

/// Reads the canonical CBOR of an Integer, refusing every other spelling.
/// Accepts and returns exactly what
/// `cek_staged_integer_v1.parse_canonical_integer_cbor_v1` does.
pub fn parse_integer_cbor_bulk_v1(cbor: ByteArray) -> CanonicalIntegerV1 {
  let initial = builtin.index_bytearray(cbor, 0)
  if and {
    initial >= 0xc2,
    builtin.index_bytearray(cbor, 1) == 0x5f,
  } {
    expect initial <= 0xc3
    CanonicalIntegerV1 {
      negative: initial == 0xc3,
      argument: indefinite_argument(cbor, builtin.length_of_bytearray(cbor)),
    }
  } else {
    parse_canonical_integer_cbor_v1(cbor)
  }
}

/// The head of a last segment of `size` bytes (1 to 64).
fn last_head(size: Int) -> ByteArray {
  if size < 24 {
    builtin.cons_bytearray(0x40 + size, #"")
  } else {
    builtin.cons_bytearray(0x58, builtin.cons_bytearray(size, #""))
  }
}

fn segment_at(argument: ByteArray, at: Int) -> ByteArray {
  builtin.append_bytearray(
    segment_head,
    builtin.slice_bytearray(at, segment_bytes, argument),
  )
}

/// Eight full segments whose first payload starts at `at` in the argument.
fn eight_segments(argument: ByteArray, at: Int) -> ByteArray {
  builtin.append_bytearray(
    builtin.append_bytearray(
      builtin.append_bytearray(
        segment_at(argument, at),
        segment_at(argument, at + 64),
      ),
      builtin.append_bytearray(
        segment_at(argument, at + 128),
        segment_at(argument, at + 192),
      ),
    ),
    builtin.append_bytearray(
      builtin.append_bytearray(
        segment_at(argument, at + 256),
        segment_at(argument, at + 320),
      ),
      builtin.append_bytearray(
        segment_at(argument, at + 384),
        segment_at(argument, at + 448),
      ),
    ),
  )
}

fn segments(
  argument: ByteArray,
  index: Int,
  full: Int,
  tail: List<ByteArray>,
) -> List<ByteArray> {
  if full - index >= 8 {
    [
      eight_segments(argument, segment_bytes * index),
      ..segments(argument, index + 8, full, tail)
    ]
  } else if index < full {
    [
      segment_at(argument, segment_bytes * index),
      ..segments(argument, index + 1, full, tail)
    ]
  } else {
    tail
  }
}

/// The canonical CBOR of an Integer as pieces whose concatenation is
/// `cek_staged_integer_v1.encode_canonical_integer_cbor_v1(integer)`.
///
/// Precondition, as for that writer: `integer.argument` is minimal, with no
/// leading zero byte, as the readers and `from_limbs_v1` return it. It is not
/// checked here; an argument with a leading zero yields non-canonical CBOR.
pub fn encode_integer_cbor_pieces_v1(
  integer: CanonicalIntegerV1,
) -> List<ByteArray> {
  let CanonicalIntegerV1 { negative, argument } = integer
  let size = builtin.length_of_bytearray(argument)
  if size <= segment_bytes {
    [encode_canonical_integer_cbor_v1(integer)]
  } else {
    let full = ( size - 1 ) / segment_bytes
    let last = size - segment_bytes * full
    let opening = if negative {
      #"c35f"
    } else {
      #"c25f"
    }
    [
      opening,
      ..segments(
        argument,
        0,
        full,
        [
          builtin.append_bytearray(
            last_head(last),
            builtin.append_bytearray(
              builtin.slice_bytearray(segment_bytes * full, last, argument),
              #"ff",
            ),
          ),
        ],
      )
    ]
  }
}

/// The canonical CBOR of an Integer, equal to
/// `cek_staged_integer_v1.encode_canonical_integer_cbor_v1`.
pub fn encode_integer_cbor_bulk_v1(integer: CanonicalIntegerV1) -> ByteArray {
  concat_balanced_v1(encode_integer_cbor_pieces_v1(integer))
}

/// The canonical CBOR, as pieces, of an integer given by sign and magnitude
/// limbs; their concatenation equals
/// `cek_staged_integer_v1.integer_cbor_from_magnitude_v1(integer)`.
pub fn integer_cbor_pieces_from_magnitude_v1(
  integer: SignedLimbsV1,
) -> List<ByteArray> {
  let SignedLimbsV1 { negative, magnitude } = integer
  if negative {
    expect magnitude != []
    encode_integer_cbor_pieces_v1(
      CanonicalIntegerV1 {
        negative: True,
        argument: from_limbs_v1(sub_limbs_v1(magnitude, [1])),
      },
    )
  } else {
    encode_integer_cbor_pieces_v1(
      CanonicalIntegerV1 { negative: False, argument: from_limbs_v1(magnitude) },
    )
  }
}

/// The canonical CBOR of an integer given by sign and magnitude limbs, equal
/// to `cek_staged_integer_v1.integer_cbor_from_magnitude_v1(integer)`. At most
/// 33,795 bytes for an integer in range, so it can be joined and rooted whole,
/// which costs slightly less than rooting the pieces.
pub fn integer_cbor_from_magnitude_bulk_v1(integer: SignedLimbsV1) -> ByteArray {
  concat_balanced_v1(integer_cbor_pieces_from_magnitude_v1(integer))
}

/// Compares two integers read from their canonical CBOR without converting
/// them: by sign, then by the length and bytes of the argument (a longer or
/// greater argument is a larger magnitude, which is smaller when negative).
pub fn compare_canonical_integers_v1(
  left: CanonicalIntegerV1,
  right: CanonicalIntegerV1,
) -> Ordering {
  if left.negative != right.negative {
    if left.negative {
      Less
    } else {
      Greater
    }
  } else {
    let left_size = builtin.length_of_bytearray(left.argument)
    let right_size = builtin.length_of_bytearray(right.argument)
    let arguments = if left_size < right_size {
      Less
    } else if left_size > right_size {
      Greater
    } else if left.argument == right.argument {
      Equal
    } else if builtin.less_than_bytearray(left.argument, right.argument) {
      Less
    } else {
      Greater
    }
    if left.negative {
      when arguments is {
        Less -> Greater
        Greater -> Less
        Equal -> Equal
      }
    } else {
      arguments
    }
  }
}
