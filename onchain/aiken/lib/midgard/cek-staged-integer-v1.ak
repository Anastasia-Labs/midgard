//// Integers read from, and written to, their canonical Plutus Data CBOR, and
//// arithmetic on their magnitudes in fixed-size limbs.
////
//// An Integer leaf commits the flat root of `serialiseData (I n)`. That CBOR is
//// major type 0 (n >= 0) or 1 (n < 0) with the minimal head while the CBOR
//// argument fits 64 bits, and tag 2 (n >= 0) or tag 3 (n < 0) over the
//// argument's minimal big-endian bytes otherwise. The argument is n for
//// n >= 0 and -n - 1 for n < 0. Bytes up to 64 long are one definite string;
//// longer ones are an indefinite string of 64-byte segments, only the last
//// shorter, closed by a break.
////
//// Magnitudes are converted piecewise: 512-byte limbs taken from the least
//// significant end, least significant first, so a whole large integer is never
//// converted at once.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use midgard/cek_data_v1.{
  DataSummaryV1, IntegerData, data_node_memory_v1, hash_data_node_v1,
}
use midgard/cek_staged_blob_v1.{concat_balanced_v1}

/// The bytes in one limb.
pub const limb_bytes = 512

/// 2^(8 * limb_bytes).
const limb_base =
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(1, builtin.replicate_byte(limb_bytes, 0)),
  )

/// The bytes of the longest definite bignum string, and of every segment of an
/// indefinite one but the last.
const segment_bytes = 64

/// An integer as its canonical CBOR spells it: the sign and the minimal
/// big-endian bytes of the CBOR argument (n, or -n - 1 when negative). Zero
/// and -1 have an empty argument.
pub type CanonicalIntegerV1 {
  negative: Bool,
  argument: ByteArray,
}

/// An integer as a sign and its magnitude in limbs, least significant first,
/// with no zero limb at the most significant end. Zero is non-negative with no
/// limbs.
pub type SignedLimbsV1 {
  negative: Bool,
  magnitude: List<Int>,
}

/// The value of a CBOR head's argument when it is the minimal head for that
/// value: `extra` argument bytes follow the initial byte.
fn small_argument(cbor: ByteArray, extra: Int, minimum: Int) -> ByteArray {
  expect builtin.length_of_bytearray(cbor) == 1 + extra
  let value = builtin.bytearray_to_integer(
    True,
    builtin.slice_bytearray(1, extra, cbor),
  )
  expect value >= minimum
  builtin.integer_to_bytearray(True, 0, value)
}

/// Segments of an indefinite byte string from the segment head at `offset`;
/// the break is the byte at `break_at`.
fn indefinite_segments(
  cbor: ByteArray,
  offset: Int,
  break_at: Int,
) -> List<ByteArray> {
  let head = builtin.index_bytearray(cbor, offset)
  if head == 0x58 {
    let size = builtin.index_bytearray(cbor, offset + 1)
    if size == segment_bytes {
      let segment = builtin.slice_bytearray(offset + 2, segment_bytes, cbor)
      let next = offset + 2 + segment_bytes
      if next == break_at {
        [segment]
      } else {
        [segment, ..indefinite_segments(cbor, next, break_at)]
      }
    } else {
      expect size >= 24
      expect size < segment_bytes
      expect offset + 2 + size == break_at
      [builtin.slice_bytearray(offset + 2, size, cbor)]
    }
  } else {
    expect head > 0x40
    expect head < 0x58
    let size = head - 0x40
    expect offset + 1 + size == break_at
    [builtin.slice_bytearray(offset + 1, size, cbor)]
  }
}

/// The argument bytes of a tag 2 or tag 3 bignum, whose byte-string head is at
/// offset 1.
fn bignum_argument(cbor: ByteArray, length: Int) -> ByteArray {
  let head = builtin.index_bytearray(cbor, 1)
  let argument = if head == 0x5f {
    let break_at = length - 1
    expect builtin.index_bytearray(cbor, break_at) == 0xff
    let joined = concat_balanced_v1(indefinite_segments(cbor, 2, break_at))
    expect builtin.length_of_bytearray(joined) > segment_bytes
    joined
  } else if head == 0x58 {
    let size = builtin.index_bytearray(cbor, 2)
    expect size >= 24
    expect size <= segment_bytes
    expect length == 3 + size
    builtin.slice_bytearray(3, size, cbor)
  } else {
    // At least nine bytes: an argument below 2^64 has a major-type head.
    expect head >= 0x49
    expect head < 0x58
    expect length == head - 0x3e
    builtin.slice_bytearray(2, head - 0x40, cbor)
  }
  expect builtin.index_bytearray(argument, 0) > 0
  argument
}

/// Reads the canonical CBOR of an Integer, refusing every other spelling.
pub fn parse_canonical_integer_cbor_v1(cbor: ByteArray) -> CanonicalIntegerV1 {
  let length = builtin.length_of_bytearray(cbor)
  let initial = builtin.index_bytearray(cbor, 0)
  if initial < 0x40 {
    let negative = initial >= 0x20
    let additional = if negative {
      initial - 0x20
    } else {
      initial
    }
    let argument = if additional < 24 {
      expect length == 1
      builtin.integer_to_bytearray(True, 0, additional)
    } else if additional == 24 {
      small_argument(cbor, 1, 24)
    } else if additional == 25 {
      small_argument(cbor, 2, 256)
    } else if additional == 26 {
      small_argument(cbor, 4, 65536)
    } else {
      expect additional == 27
      small_argument(cbor, 8, 4294967296)
    }
    CanonicalIntegerV1 { negative, argument }
  } else if initial == 0xc2 {
    CanonicalIntegerV1 {
      negative: False,
      argument: bignum_argument(cbor, length),
    }
  } else {
    expect initial == 0xc3
    CanonicalIntegerV1 {
      negative: True,
      argument: bignum_argument(cbor, length),
    }
  }
}

fn definite_bytes_head(size: Int) -> ByteArray {
  if size < 24 {
    builtin.cons_bytearray(0x40 + size, #"")
  } else {
    builtin.cons_bytearray(0x58, builtin.cons_bytearray(size, #""))
  }
}

fn encode_segments(
  argument: ByteArray,
  offset: Int,
  size: Int,
) -> List<ByteArray> {
  let remaining = size - offset
  if remaining > segment_bytes {
    [
      builtin.append_bytearray(
        #"5840",
        builtin.slice_bytearray(offset, segment_bytes, argument),
      ),
      ..encode_segments(argument, offset + segment_bytes, size)
    ]
  } else {
    [
      builtin.append_bytearray(
        definite_bytes_head(remaining),
        builtin.slice_bytearray(offset, remaining, argument),
      ),
      #"ff",
    ]
  }
}

/// The canonical CBOR of an Integer, the inverse of
/// `parse_canonical_integer_cbor_v1`.
///
/// Precondition: `integer.argument` is minimal, with no leading zero byte, as
/// `parse_canonical_integer_cbor_v1` and `from_limbs_v1` return it. The writer
/// does not check this; an argument with a leading zero yields bytes that are
/// not the canonical CBOR of any Integer. Never pass it an argument taken from
/// a witness without reading it through one of those first.
pub fn encode_canonical_integer_cbor_v1(
  integer: CanonicalIntegerV1,
) -> ByteArray {
  let CanonicalIntegerV1 { negative, argument } = integer
  let size = builtin.length_of_bytearray(argument)
  if size <= 8 {
    let value = builtin.bytearray_to_integer(True, argument)
    cbor.serialise(
      if negative {
        -value - 1
      } else {
        value
      },
    )
  } else {
    let tag = if negative {
      0xc3
    } else {
      0xc2
    }
    if size <= segment_bytes {
      builtin.cons_bytearray(
        tag,
        builtin.append_bytearray(definite_bytes_head(size), argument),
      )
    } else {
      builtin.cons_bytearray(
        tag,
        builtin.cons_bytearray(
          0x5f,
          concat_balanced_v1(encode_segments(argument, 0, size)),
        ),
      )
    }
  }
}

fn all_ff(bytes: ByteArray, size: Int) -> Bool {
  if size <= 8192 {
    bytes == builtin.replicate_byte(size, 0xff)
  } else {
    and {
      builtin.slice_bytearray(0, 8192, bytes) == builtin.replicate_byte(
        8192,
        0xff,
      ),
      all_ff(builtin.slice_bytearray(8192, size - 8192, bytes), size - 8192),
    }
  }
}

/// Cardano's memory size of the integer: the 64-bit words of its magnitude,
/// and one for zero. A negative integer's magnitude is its argument plus one,
/// which is one byte longer only when the argument is all 0xff.
pub fn magnitude_words_v1(integer: CanonicalIntegerV1) -> Int {
  let size = builtin.length_of_bytearray(integer.argument)
  if and {
    integer.negative,
    size % 8 == 0,
    all_ff(integer.argument, size),
  } {
    size / 8 + 1
  } else if size == 0 {
    1
  } else {
    ( size + 7 ) / 8
  }
}

/// The Data summary of an Integer leaf from its CBOR's flat root and length
/// and its memory words, without building the Integer. Equal to
/// `cek_data_v1.integer_data_summary_v1` for the same integer.
pub fn integer_leaf_from_cbor_v1(
  cbor_root: ByteArray,
  cbor_length: Int,
  words: Int,
) -> DataSummaryV1 {
  let node = IntegerData { cbor_root, cbor_length, memory: 4 + words }
  DataSummaryV1 {
    root: hash_data_node_v1(node),
    cbor_length,
    memory: data_node_memory_v1(node),
  }
}

fn limbs_below(bytes: ByteArray, end: Int) -> List<Int> {
  if end > limb_bytes {
    [
      builtin.bytearray_to_integer(
        True,
        builtin.slice_bytearray(end - limb_bytes, limb_bytes, bytes),
      ),
      ..limbs_below(bytes, end - limb_bytes)
    ]
  } else if end > 0 {
    [builtin.bytearray_to_integer(True, builtin.slice_bytearray(0, end, bytes))]
  } else {
    []
  }
}

/// The limbs of a magnitude given as minimal big-endian bytes.
pub fn to_limbs_v1(magnitude: ByteArray) -> List<Int> {
  limbs_below(magnitude, builtin.length_of_bytearray(magnitude))
}

fn bytes_from_top(limbs: List<Int>) -> ByteArray {
  when limbs is {
    [] -> #""
    [top, ..rest] ->
      if top == 0 {
        bytes_from_top(rest)
      } else {
        concat_balanced_v1(
          [
            builtin.integer_to_bytearray(True, 0, top),
            ..list.map(
              rest,
              fn(limb) { builtin.integer_to_bytearray(True, limb_bytes, limb) },
            )
          ],
        )
      }
  }
}

/// The minimal big-endian bytes of a magnitude given in limbs.
pub fn from_limbs_v1(limbs: List<Int>) -> ByteArray {
  bytes_from_top(list.reverse(limbs))
}

fn add_with_carry(left: List<Int>, right: List<Int>, carry: Int) -> List<Int> {
  when left is {
    [] ->
      when right is {
        [] ->
          if carry == 0 {
            []
          } else {
            [carry]
          }
        _ ->
          if carry == 0 {
            right
          } else {
            add_with_carry(right, [], carry)
          }
      }
    [x, ..xs] ->
      when right is {
        [] ->
          if carry == 0 {
            left
          } else {
            let sum = x + carry
            if sum < limb_base {
              [sum, ..xs]
            } else {
              [sum - limb_base, ..add_with_carry(xs, [], 1)]
            }
          }
        [y, ..ys] -> {
          let sum = x + y + carry
          if sum < limb_base {
            [sum, ..add_with_carry(xs, ys, 0)]
          } else {
            [sum - limb_base, ..add_with_carry(xs, ys, 1)]
          }
        }
      }
  }
}

/// The sum of two magnitudes.
pub fn add_limbs_v1(left: List<Int>, right: List<Int>) -> List<Int> {
  add_with_carry(left, right, 0)
}

/// A limb under `rest`, dropping it when it would be a zero most significant
/// limb.
fn push_limb(limb: Int, rest: List<Int>) -> List<Int> {
  if limb == 0 && rest == [] {
    []
  } else {
    [limb, ..rest]
  }
}

fn subtract_with_borrow(
  left: List<Int>,
  right: List<Int>,
  borrow: Int,
) -> List<Int> {
  when right is {
    [] ->
      if borrow == 0 {
        left
      } else {
        expect [x, ..xs] = left
        if x > 0 {
          push_limb(x - 1, xs)
        } else {
          push_limb(limb_base - 1, subtract_with_borrow(xs, [], 1))
        }
      }
    [y, ..ys] -> {
      expect [x, ..xs] = left
      let difference = x - y - borrow
      if difference >= 0 {
        push_limb(difference, subtract_with_borrow(xs, ys, 0))
      } else {
        push_limb(difference + limb_base, subtract_with_borrow(xs, ys, 1))
      }
    }
  }
}

/// The difference of two magnitudes; `left` must be at least `right`.
pub fn sub_limbs_v1(left: List<Int>, right: List<Int>) -> List<Int> {
  subtract_with_borrow(left, right, 0)
}

/// Compares two magnitudes.
pub fn cmp_magnitude_v1(left: List<Int>, right: List<Int>) -> Ordering {
  when left is {
    [] ->
      if right == [] {
        Equal
      } else {
        Less
      }
    [x, ..xs] ->
      when right is {
        [] -> Greater
        [y, ..ys] ->
          when cmp_magnitude_v1(xs, ys) is {
            Equal ->
              if x < y {
                Less
              } else if x > y {
                Greater
              } else {
                Equal
              }
            higher -> higher
          }
      }
  }
}

/// The sign and magnitude limbs of an integer read from its CBOR.
pub fn signed_limbs_v1(integer: CanonicalIntegerV1) -> SignedLimbsV1 {
  let limbs = to_limbs_v1(integer.argument)
  if integer.negative {
    SignedLimbsV1 { negative: True, magnitude: add_with_carry(limbs, [], 1) }
  } else {
    SignedLimbsV1 { negative: False, magnitude: limbs }
  }
}

/// The sum of two integers.
pub fn add_signed_v1(left: SignedLimbsV1, right: SignedLimbsV1) -> SignedLimbsV1 {
  if left.negative == right.negative {
    SignedLimbsV1 {
      negative: left.negative,
      magnitude: add_limbs_v1(left.magnitude, right.magnitude),
    }
  } else {
    when cmp_magnitude_v1(left.magnitude, right.magnitude) is {
      Greater ->
        SignedLimbsV1 {
          negative: left.negative,
          magnitude: sub_limbs_v1(left.magnitude, right.magnitude),
        }
      Less ->
        SignedLimbsV1 {
          negative: right.negative,
          magnitude: sub_limbs_v1(right.magnitude, left.magnitude),
        }
      Equal -> SignedLimbsV1 { negative: False, magnitude: [] }
    }
  }
}

/// The difference of two integers.
pub fn subtract_signed_v1(
  left: SignedLimbsV1,
  right: SignedLimbsV1,
) -> SignedLimbsV1 {
  when right.magnitude is {
    [] -> left
    _ ->
      add_signed_v1(
        left,
        SignedLimbsV1 { negative: !right.negative, magnitude: right.magnitude },
      )
  }
}

/// The canonical CBOR of an integer given by sign and magnitude limbs. A
/// negative integer's CBOR argument is its magnitude minus one.
pub fn integer_cbor_from_magnitude_v1(integer: SignedLimbsV1) -> ByteArray {
  let SignedLimbsV1 { negative, magnitude } = integer
  if negative {
    expect magnitude != []
    encode_canonical_integer_cbor_v1(
      CanonicalIntegerV1 {
        negative: True,
        argument: from_limbs_v1(sub_limbs_v1(magnitude, [1])),
      },
    )
  } else {
    encode_canonical_integer_cbor_v1(
      CanonicalIntegerV1 { negative: False, argument: from_limbs_v1(magnitude) },
    )
  }
}
