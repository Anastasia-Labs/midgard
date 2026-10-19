use aiken/builtin
use aiken/cbor
use aiken/collection/list
use midgard/cek_staged_blob_v1.{
  concat_balanced_v1, flat_root_of_pieces_v1, flat_root_v1,
}
use midgard/cek_staged_integer_cbor_v1.{
  compare_canonical_integers_v1, encode_integer_cbor_bulk_v1,
  encode_integer_cbor_pieces_v1, integer_cbor_from_magnitude_bulk_v1,
  integer_cbor_pieces_from_magnitude_v1, parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, encode_canonical_integer_cbor_v1,
  integer_cbor_from_magnitude_v1, parse_canonical_integer_cbor_v1,
  signed_limbs_v1,
}

// ## Fixtures

fn ascending(from: Int, acc: ByteArray) -> ByteArray {
  if from < 0 {
    acc
  } else {
    ascending(from - 1, builtin.cons_bytearray(from, acc))
  }
}

fn repeat_to(block: ByteArray, length: Int) -> ByteArray {
  if builtin.length_of_bytearray(block) >= length {
    builtin.slice_bytearray(0, length, block)
  } else {
    repeat_to(builtin.append_bytearray(block, block), length)
  }
}

/// `length` bytes 0, 1, ..., 255, 0, 1, ... (at most 65,536).
fn pattern(length: Int) -> ByteArray {
  repeat_to(ascending(255, #""), length)
}

/// The integer whose big-endian bytes are 1 followed by `pattern(size - 1)`:
/// `size` bytes, no two neighbouring 64-byte segments alike.
fn magnitude(size: Int) -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(1, pattern(size - 1)),
  )
}

/// `size` copies of `byte`, at any size (one replicate is at most 8,192).
fn replicate(size: Int, byte: Int) -> ByteArray {
  if size <= 8192 {
    builtin.replicate_byte(size, byte)
  } else {
    builtin.append_bytearray(
      builtin.replicate_byte(8192, byte),
      replicate(size - 8192, byte),
    )
  }
}

fn repeated(size: Int, byte: Int) -> Int {
  builtin.bytearray_to_integer(True, replicate(size, byte))
}

/// 2^(8 * bytes).
fn byte_power(bytes: Int) -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(1, replicate(bytes, 0)),
  )
}

/// 2^262143 - 1, the largest integer.
fn largest() -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(0x7f, replicate(32767, 0xff)),
  )
}

/// The bulk reader and writers agree byte for byte with the per-segment ones.
fn agrees(n: Int) -> Bool {
  let encoded = cbor.serialise(n)
  let parsed = parse_canonical_integer_cbor_v1(encoded)
  let signed = signed_limbs_v1(parsed)
  and {
    parse_integer_cbor_bulk_v1(encoded) == parsed,
    encode_integer_cbor_bulk_v1(parsed) == encoded,
    concat_balanced_v1(encode_integer_cbor_pieces_v1(parsed)) == encoded,
    encode_canonical_integer_cbor_v1(parsed) == encoded,
    integer_cbor_from_magnitude_bulk_v1(signed) == encoded,
    integer_cbor_from_magnitude_v1(signed) == encoded,
    flat_root_of_pieces_v1(integer_cbor_pieces_from_magnitude_v1(signed)) == flat_root_v1(
      encoded,
    ),
  }
}

fn agrees_both_signs(n: Int) -> Bool {
  and {
    agrees(n),
    agrees(-n),
    agrees(-n - 1),
  }
}

/// True whenever the bulk reader accepts `encoded`, so a refusal row passes
/// only when the reader aborts.
fn accepted(encoded: ByteArray) -> Bool {
  builtin.length_of_bytearray(parse_integer_cbor_bulk_v1(encoded).argument) >= 0
}

fn segment(byte: Int) -> ByteArray {
  builtin.append_bytearray(#"5840", replicate(64, byte))
}

/// `bytes` with the byte at `at` replaced by `value`.
fn with_byte(bytes: ByteArray, at: Int, value: Int) -> ByteArray {
  builtin.append_bytearray(
    builtin.slice_bytearray(0, at, bytes),
    builtin.cons_bytearray(
      value,
      builtin.slice_bytearray(
        at + 1,
        builtin.length_of_bytearray(bytes) - at - 1,
        bytes,
      ),
    ),
  )
}

fn compare_ints(left: Int, right: Int) -> Ordering {
  if left < right {
    Less
  } else if left > right {
    Greater
  } else {
    Equal
  }
}

// ## Agreement: accepted values

test agrees_on_small_integers() {
  list.all(
    [
      0,
      1,
      23,
      24,
      255,
      256,
      65535,
      65536,
      byte_power(4) - 1,
      byte_power(4),
      byte_power(8) - 1,
      byte_power(8),
      byte_power(8) + 1,
    ],
    agrees_both_signs,
  )
}

test agrees_up_to_one_definite_string() {
  list.all(
    [9, 23, 24, 25, 63, 64],
    fn(size) { agrees_both_signs(magnitude(size)) },
  )
}

/// One full segment and every size of last segment, across the one-byte and
/// two-byte last heads.
test agrees_on_every_last_segment_size() {
  list.all(
    [65, 66, 86, 87, 88, 89, 100, 127, 128],
    fn(size) { agrees_both_signs(magnitude(size)) },
  )
}

/// Around a whole block of eight full segments.
test agrees_around_eight_full_segments() {
  list.all(
    [448, 449, 511, 512, 513, 535, 536, 537, 576, 577, 1024, 1025, 1088, 1089],
    fn(size) { agrees_both_signs(magnitude(size)) },
  )
}

test agrees_at_4097_and_8192_bytes() {
  and {
    agrees_both_signs(magnitude(4097)),
    agrees_both_signs(magnitude(8192)),
  }
}

test agrees_at_32768_bytes() {
  and {
    agrees(magnitude(32768)),
    agrees(-magnitude(32768)),
  }
}

test agrees_at_the_range_edges() {
  and {
    agrees(largest()),
    agrees(-largest()),
    agrees(-largest() - 1),
  }
}

/// Negative growth: -2^(8k) has a k-byte argument of 0xff bytes.
test agrees_through_negative_growth() {
  list.all(
    [64, 512, 4096],
    fn(size) {
      and {
        agrees(-byte_power(size)),
        agrees(-byte_power(size) - 1),
        agrees(-byte_power(size) + 1),
        agrees(repeated(size, 0xff)),
      }
    },
  )
}

test reads_the_value_and_sign() {
  let n = magnitude(600)
  let positive = parse_integer_cbor_bulk_v1(cbor.serialise(n))
  let negative = parse_integer_cbor_bulk_v1(cbor.serialise(-n))
  and {
    !positive.negative,
    builtin.bytearray_to_integer(True, positive.argument) == n,
    negative.negative,
    builtin.bytearray_to_integer(True, negative.argument) == n - 1,
  }
}

test reads_a_changed_payload_byte_as_another_value() {
  let honest = cbor.serialise(magnitude(1100))
  let tampered = with_byte(honest, 4 + 66 * 9 + 5, 0xee)
  parse_integer_cbor_bulk_v1(tampered) != parse_integer_cbor_bulk_v1(honest)
}

// ## Refusals: the per-segment reader's

test refuses_a_leading_zero() fail {
  accepted(#"c249000100000000000000")
}

test refuses_a_leading_zero_in_the_first_segment() fail {
  accepted(with_byte(cbor.serialise(magnitude(65)), 4, 0))
}

test refuses_a_leading_zero_in_a_long_bignum() fail {
  accepted(with_byte(cbor.serialise(magnitude(4097)), 4, 0))
}

test refuses_an_eight_byte_bignum() fail {
  accepted(#"c2480100000000000000")
}

test refuses_a_one_byte_bignum() fail {
  accepted(#"c24105")
}

test refuses_a_definite_form_above_64_bytes() fail {
  accepted(builtin.append_bytearray(#"c25841", replicate(65, 1)))
}

/// A 10-byte argument under the two-byte definite head `58 0a`.
test refuses_a_non_minimal_definite_head() fail {
  accepted(builtin.append_bytearray(#"c2580a", replicate(10, 1)))
}

test refuses_a_63_byte_middle_segment() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f583f",
      builtin.append_bytearray(replicate(63, 1), #"42aaaaff"),
    ),
  )
}

test refuses_an_empty_final_segment() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f",
      builtin.append_bytearray(segment(1), #"40ff"),
    ),
  )
}

test refuses_a_missing_break() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f",
      builtin.append_bytearray(segment(1), #"4101"),
    ),
  )
}

test refuses_an_indefinite_form_of_64_bytes() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f",
      builtin.append_bytearray(segment(1), #"ff"),
    ),
  )
}

test refuses_an_indefinite_form_of_one_short_segment() fail {
  accepted(#"c25f4101ff")
}

test refuses_an_oversized_final_segment() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f5841",
      builtin.append_bytearray(replicate(65, 1), #"ff"),
    ),
  )
}

test refuses_an_oversized_final_segment_after_a_full_one() fail {
  accepted(
    builtin.append_bytearray(
      builtin.append_bytearray(#"c25f", segment(1)),
      builtin.append_bytearray(
        #"5841",
        builtin.append_bytearray(replicate(65, 1), #"ff"),
      ),
    ),
  )
}

test refuses_a_short_segment_before_the_last() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f4101",
      builtin.append_bytearray(segment(1), #"ff"),
    ),
  )
}

test refuses_a_non_minimal_segment_head() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f",
      builtin.append_bytearray(segment(1), #"5802aaaaff"),
    ),
  )
}

/// A 23-byte last segment under a two-byte head: 25 bytes with its head.
test refuses_a_non_minimal_23_byte_last_segment() fail {
  accepted(
    builtin.append_bytearray(
      builtin.append_bytearray(#"c25f", segment(1)),
      builtin.append_bytearray(
        #"5817",
        builtin.append_bytearray(replicate(23, 1), #"ff"),
      ),
    ),
  )
}

test refuses_a_long_segment_head() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f590041",
      builtin.append_bytearray(replicate(65, 1), #"ff"),
    ),
  )
}

test refuses_a_non_minimal_one_byte_head() fail {
  accepted(#"1805")
}

/// 23 under the one-byte head: the largest value it may not carry.
test refuses_a_non_minimal_one_byte_head_at_the_boundary() fail {
  accepted(#"1817")
}

test refuses_a_non_minimal_negative_one_byte_head_at_the_boundary() fail {
  accepted(#"3817")
}

test refuses_a_non_minimal_negative_head() fail {
  accepted(#"3805")
}

test refuses_a_reserved_head() fail {
  accepted(#"1c")
}

test refuses_trailing_bytes() fail {
  accepted(#"0100")
}

test refuses_trailing_bytes_after_a_long_bignum() fail {
  accepted(builtin.append_bytearray(cbor.serialise(magnitude(600)), #"00"))
}

test refuses_another_tag() fail {
  accepted(#"c449010000000000000000")
}

test refuses_another_tag_on_an_indefinite_string() fail {
  accepted(with_byte(cbor.serialise(magnitude(600)), 0, 0xc4))
}

test refuses_a_byte_string() fail {
  accepted(#"4101")
}

// ## Refusals: segment heads deep in a long bignum

test refuses_a_wrong_head_in_the_ninth_full_segment() fail {
  accepted(with_byte(cbor.serialise(magnitude(1100)), 2 + 66 * 8, 0x59))
}

test refuses_a_wrong_size_in_the_ninth_full_segment() fail {
  accepted(with_byte(cbor.serialise(magnitude(1100)), 3 + 66 * 8, 0x41))
}

test refuses_a_wrong_size_in_the_last_full_segment() fail {
  accepted(with_byte(cbor.serialise(magnitude(1100)), 3 + 66 * 16, 0x3f))
}

test refuses_a_wrong_head_in_a_32768_byte_bignum() fail {
  accepted(with_byte(cbor.serialise(-magnitude(32768)), 2 + 66 * 300, 0x40))
}

/// 1,100 bytes: 17 full segments and a 12-byte last one, head 0x4c.
test refuses_a_last_one_byte_head_off_by_one() fail {
  accepted(with_byte(cbor.serialise(magnitude(1100)), 2 + 66 * 17, 0x4d))
}

/// 1,130 bytes: 17 full segments and a 42-byte last one, head 0x58 0x2a.
test refuses_a_last_two_byte_head_off_by_one() fail {
  accepted(with_byte(cbor.serialise(magnitude(1130)), 3 + 66 * 17, 0x2b))
}

test refuses_a_dropped_last_payload_byte() fail {
  let honest = cbor.serialise(magnitude(1130))
  let length = builtin.length_of_bytearray(honest)
  accepted(
    builtin.append_bytearray(
      builtin.slice_bytearray(0, length - 2, honest),
      #"ff",
    ),
  )
}

// ## Comparison without conversion

test compares_like_native_integers() {
  let values = [
    0,
    1,
    -1,
    23,
    24,
    -24,
    -25,
    255,
    256,
    -256,
    -257,
    byte_power(8) - 1,
    byte_power(8),
    -byte_power(8),
    -byte_power(8) - 1,
    magnitude(65),
    magnitude(65) + 1,
    -magnitude(65),
    -magnitude(65) - 1,
    magnitude(66),
    -magnitude(66),
    repeated(64, 0xff),
    -repeated(64, 0xff) - 1,
    largest(),
    -largest(),
    -largest() - 1,
  ]
  let parsed = list.map(
    values,
    fn(n) { Pair(n, parse_integer_cbor_bulk_v1(cbor.serialise(n))) },
  )
  list.all(
    parsed,
    fn(left) {
      list.all(
        parsed,
        fn(right) {
          compare_canonical_integers_v1(left.2nd, right.2nd) == compare_ints(
            left.1st,
            right.1st,
          )
        },
      )
    },
  )
}

test compares_equal_length_arguments_by_their_bytes() {
  let a = parse_integer_cbor_bulk_v1(cbor.serialise(magnitude(32768)))
  let b = parse_integer_cbor_bulk_v1(
    cbor.serialise(magnitude(32768) + byte_power(4000)),
  )
  and {
    compare_canonical_integers_v1(a, b) == Less,
    compare_canonical_integers_v1(b, a) == Greater,
    compare_canonical_integers_v1(
      CanonicalIntegerV1 { ..a, negative: True },
      CanonicalIntegerV1 { ..b, negative: True },
    ) == Greater,
    compare_canonical_integers_v1(a, a) == Equal,
  }
}
