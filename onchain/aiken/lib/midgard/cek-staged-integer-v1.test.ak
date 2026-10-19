use aiken/builtin
use aiken/cbor
use aiken/collection/list
use midgard/cek_data_v1.{integer_data_summary_v1, integer_memory_size_v1}
use midgard/cek_staged_blob_v1.{flat_root_v1}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, SignedLimbsV1, add_limbs_v1, add_signed_v1,
  cmp_magnitude_v1, encode_canonical_integer_cbor_v1, from_limbs_v1,
  integer_cbor_from_magnitude_v1, integer_leaf_from_cbor_v1, limb_bytes,
  magnitude_words_v1, parse_canonical_integer_cbor_v1, signed_limbs_v1,
  sub_limbs_v1, subtract_signed_v1, to_limbs_v1,
}

// ## Fixtures

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

/// 2^bits.
fn power(bits: Int) -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(small_power(bits % 8), replicate(bits / 8, 0)),
  )
}

fn small_power(bits: Int) -> Int {
  if bits == 0 {
    1
  } else {
    2 * small_power(bits - 1)
  }
}

/// The integer whose big-endian bytes are `size` copies of `byte`.
fn repeated(size: Int, byte: Int) -> Int {
  builtin.bytearray_to_integer(True, replicate(size, byte))
}

/// 2^262143 - 1, the largest integer.
fn largest() -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(0x7f, replicate(32767, 0xff)),
  )
}

/// -2^262143, the smallest integer.
fn smallest() -> Int {
  -largest() - 1
}

/// The integer whose big-endian bytes are 1, 2, 3, ... (mod 256), `size` long.
fn counting(size: Int) -> Int {
  builtin.bytearray_to_integer(True, counting_bytes(size, #""))
}

fn counting_bytes(size: Int, acc: ByteArray) -> ByteArray {
  if size == 0 {
    acc
  } else {
    counting_bytes(size - 1, builtin.cons_bytearray(size % 255 + 1, acc))
  }
}

/// The parse of `serialiseData (I n)` reads back n, re-encodes to the same
/// bytes, and gives the memory words and Integer leaf of the existing summary.
fn reads(n: Int) -> Bool {
  let encoded = cbor.serialise(n)
  let parsed = parse_canonical_integer_cbor_v1(encoded)
  let argument = builtin.bytearray_to_integer(True, parsed.argument)
  let root = flat_root_v1(encoded)
  let words = magnitude_words_v1(parsed)
  and {
    parsed.negative == ( n < 0 ),
    if n < 0 {
      argument == -n - 1
    } else {
      argument == n
    },
    encode_canonical_integer_cbor_v1(parsed) == encoded,
    words == integer_memory_size_v1(n),
    integer_leaf_from_cbor_v1(root, builtin.length_of_bytearray(encoded), words) == integer_data_summary_v1(
      n,
      root,
    ),
  }
}

fn reads_both_signs(magnitude: Int) -> Bool {
  and {
    reads(magnitude),
    reads(-magnitude),
  }
}

fn limbs_of(n: Int) -> SignedLimbsV1 {
  signed_limbs_v1(parse_canonical_integer_cbor_v1(cbor.serialise(n)))
}

/// Limbwise sum and difference agree with the native result's canonical CBOR.
fn adds_and_subtracts(left: Int, right: Int) -> Bool {
  let a = limbs_of(left)
  let b = limbs_of(right)
  and {
    integer_cbor_from_magnitude_v1(add_signed_v1(a, b)) == cbor.serialise(
      left + right,
    ),
    integer_cbor_from_magnitude_v1(subtract_signed_v1(a, b)) == cbor.serialise(
      left - right,
    ),
    integer_cbor_from_magnitude_v1(subtract_signed_v1(b, a)) == cbor.serialise(
      right - left,
    ),
  }
}

fn absolute(n: Int) -> Int {
  if n < 0 {
    -n
  } else {
    n
  }
}

fn compares(left: Int, right: Int) -> Bool {
  cmp_magnitude_v1(limbs_of(left).magnitude, limbs_of(right).magnitude) == compare_ints(
    absolute(left),
    absolute(right),
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

/// True whenever the reader accepts `encoded`, so a refusal row passes only
/// when the reader aborts.
fn accepted(encoded: ByteArray) -> Bool {
  builtin.length_of_bytearray(parse_canonical_integer_cbor_v1(encoded).argument) >= 0
}

fn segment(byte: Int) -> ByteArray {
  builtin.append_bytearray(#"5840", replicate(64, byte))
}

// ## Canonical reading: accepted

test reads_zero_and_one() {
  and {
    reads(0),
    reads(1),
    reads(-1),
  }
}

test reads_every_head_boundary() {
  list.all(
    [
      23,
      24,
      255,
      256,
      65535,
      65536,
      power(32) - 1,
      power(32),
      power(64) - 1,
      power(64),
    ],
    reads_both_signs,
  )
}

test reads_the_64_bit_edges() {
  and {
    reads(power(64) - 1),
    reads(-power(64)),
    reads(power(64)),
    reads(-power(64) - 1),
  }
}

test reads_the_512_bit_edges() {
  and {
    reads_both_signs(power(512) - 1),
    reads_both_signs(power(512)),
    reads_both_signs(power(512) + 1),
  }
}

test reads_64_65_128_and_129_byte_magnitudes() {
  list.all(
    [64, 65, 128, 129],
    fn(size) {
      and {
        reads_both_signs(counting(size)),
        reads_both_signs(repeated(size, 0xff)),
        reads_both_signs(power(8 * size - 1)),
      }
    },
  )
}

/// The integer range is [-2^262143, 2^262143 - 1]: 32,768-byte magnitudes.
test reads_the_largest_integer() {
  reads(largest())
}

test reads_the_largest_negative_32768_byte_integer() {
  reads(-largest())
}

test reads_a_32768_byte_integer() {
  reads(-repeated(32768, 0x5a))
}

/// -2^262143 has a 32,768-byte magnitude of 4,096 words.
test reads_the_smallest_integer() {
  let encoded = cbor.serialise(smallest())
  let parsed = parse_canonical_integer_cbor_v1(encoded)
  and {
    parsed.negative,
    parsed.argument == builtin.cons_bytearray(0x7f, replicate(32767, 0xff)),
    encode_canonical_integer_cbor_v1(parsed) == encoded,
    magnitude_words_v1(parsed) == 4096,
  }
}

test reads_negative_magnitude_growth() {
  list.all(
    [8, 64, 512, 4096],
    fn(size) {
      and {
        reads(-power(8 * size)),
        reads(-power(8 * size) - 1),
        reads(-power(8 * size) + 1),
      }
    },
  )
}

test negative_growth_adds_a_memory_word() {
  let parsed = parse_canonical_integer_cbor_v1(cbor.serialise(-power(4096)))
  and {
    builtin.length_of_bytearray(parsed.argument) == 512,
    magnitude_words_v1(parsed) == 65,
  }
}

// ## Canonical reading: refused

test refuses_a_leading_zero() fail {
  accepted(#"c249000100000000000000")
}

test refuses_a_leading_zero_in_the_first_segment() fail {
  let honest = cbor.serialise(counting(65))
  let tampered = builtin.append_bytearray(
    builtin.slice_bytearray(0, 4, honest),
    builtin.cons_bytearray(
      0,
      builtin.slice_bytearray(
        5,
        builtin.length_of_bytearray(honest) - 5,
        honest,
      ),
    ),
  )
  accepted(tampered)
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

test refuses_an_oversized_final_segment() fail {
  accepted(
    builtin.append_bytearray(
      #"c25f5841",
      builtin.append_bytearray(replicate(65, 1), #"ff"),
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

test refuses_a_non_minimal_two_byte_head() fail {
  accepted(#"1900ff")
}

test refuses_a_non_minimal_four_byte_head() fail {
  accepted(#"1a0000ffff")
}

test refuses_a_non_minimal_eight_byte_head() fail {
  accepted(#"1b00000000ffffffff")
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

test refuses_trailing_bytes_after_a_bignum() fail {
  accepted(builtin.append_bytearray(cbor.serialise(power(64)), #"00"))
}

test refuses_another_tag() fail {
  accepted(#"c449010000000000000000")
}

test refuses_a_byte_string() fail {
  accepted(#"4101")
}

// ## Limbs

test limbs_round_trip() {
  list.all(
    [0, 1, 255, 511, 512, 513, 1023, 1024, 1025, 8192],
    fn(size) {
      let bytes = if size == 0 {
        #""
      } else {
        builtin.integer_to_bytearray(True, 0, counting(size))
      }
      let limbs = to_limbs_v1(bytes)
      and {
        list.length(limbs) == ( size + limb_bytes - 1 ) / limb_bytes,
        from_limbs_v1(limbs) == bytes,
      }
    },
  )
}

test limbs_drop_zero_top_limbs() {
  and {
    from_limbs_v1([5, 0, 0]) == #"05",
    from_limbs_v1([0, 0]) == #"",
    from_limbs_v1([]) == #"",
    add_limbs_v1([], []) == [],
    sub_limbs_v1([7], [7]) == [],
  }
}

test limbs_refuse_a_larger_subtrahend() fail {
  list.length(sub_limbs_v1([1], [2])) >= 0
}

test adds_small_and_mixed_signs() {
  list.all(
    [
      Pair(0, 0),
      Pair(0, 5),
      Pair(5, 0),
      Pair(-5, 5),
      Pair(5, -5),
      Pair(-1, -1),
      Pair(1, -2),
      Pair(-1, 2),
      Pair(power(64), -1),
      Pair(-power(64), 1),
    ],
    fn(pair) { adds_and_subtracts(pair.1st, pair.2nd) },
  )
}

test adds_at_limb_boundaries() {
  list.all(
    [511, 512, 513, 1023, 1024, 1025],
    fn(size) {
      and {
        adds_and_subtracts(counting(size), counting(size)),
        adds_and_subtracts(counting(size), -counting(size - 1)),
        adds_and_subtracts(power(8 * size), -1),
        adds_and_subtracts(-power(8 * size), 1),
        adds_and_subtracts(power(8 * size) - 1, 1),
      }
    },
  )
}

test adds_through_carry_chains() {
  and {
    adds_and_subtracts(repeated(1536, 0xff), 1),
    adds_and_subtracts(repeated(1536, 0xff), repeated(1536, 0xff)),
    adds_and_subtracts(power(8 * 1536), -1),
    adds_and_subtracts(-repeated(1536, 0xff), -1),
    adds_and_subtracts(-repeated(1024, 0xff), repeated(1536, 0xff)),
  }
}

test adds_through_negative_growth() {
  and {
    adds_and_subtracts(-(power(4096) - 1), -1),
    adds_and_subtracts(-power(4096), 1),
    adds_and_subtracts(-power(4096), -power(4096)),
  }
}

test adds_to_zero_and_with_zero() {
  and {
    adds_and_subtracts(counting(700), -counting(700)),
    adds_and_subtracts(0, counting(700)),
    adds_and_subtracts(-counting(700), 0),
  }
}

test adds_at_8192_bytes() {
  adds_and_subtracts(counting(8192), -repeated(8191, 0x5a))
}

test adds_at_32768_bytes() {
  and {
    adds_and_subtracts(largest() - 1, 1),
    adds_and_subtracts(smallest() + 1, -1),
    adds_and_subtracts(repeated(32767, 0xff), 1),
    adds_and_subtracts(-repeated(32767, 0xff), -1),
    adds_and_subtracts(counting(32768), counting(32768)),
    adds_and_subtracts(counting(32768), -repeated(32768, 0x7f)),
  }
}

test compares_magnitudes() {
  list.all(
    [
      Pair(0, 0),
      Pair(0, 1),
      Pair(1, 0),
      Pair(-3, 2),
      Pair(power(4096), power(4096) - 1),
      Pair(power(4096) - 1, power(4096)),
      Pair(counting(600), counting(600)),
      Pair(counting(600), counting(600) + power(8)),
    ],
    fn(pair) { compares(pair.1st, pair.2nd) },
  )
}

test writer_refuses_a_negative_zero() fail {
  builtin.length_of_bytearray(
    integer_cbor_from_magnitude_v1(
      SignedLimbsV1 { negative: True, magnitude: [] },
    ),
  ) > 0
}

test writer_matches_the_reader_on_extracted_arguments() {
  let encoded = cbor.serialise(-repeated(129, 0xff))
  encode_canonical_integer_cbor_v1(
    CanonicalIntegerV1 {
      negative: True,
      argument: parse_canonical_integer_cbor_v1(encoded).argument,
    },
  ) == encoded
}
