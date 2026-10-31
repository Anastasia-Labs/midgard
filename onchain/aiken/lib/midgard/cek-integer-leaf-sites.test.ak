//// Every site that creates an I leaf creates it over the integer's canonical
//// CBOR, at any size: the script decoder (Flat integer constants and Data
//// integers inside Flat constants), the ledger Data stream, the staged
//// integer reader and writer, and direct constants. The Data scalar step
//// (`cek_core_semantic_data_scalar`) relabels a leaf as an integer and back
//// without reading its CBOR, so these rows are what keep the leaf it reads
//// canonical. (The iData argument's leaf is bound to the pre-state by the
//// roots hop; those rows are in
//// `validators/fraud-proofs/validation-trace/cek-core-semantic-data-scalar.test.ak`.)

use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_builtin_v1.{SemanticConstantValue}
use midgard/cek_data_integer_v1
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_staged_integer_cbor_v1.{
  encode_integer_cbor_bulk_v1, parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{CanonicalIntegerV1}
use midgard/fraud_proofs/cek_staged_fixture_v1.{pattern, semantic_integer_cbor}
use midgard/fraud_proofs/native_tx/codec.{slice_len}
use midgard/script_decode_bits_v1.{
  integer_summary, mag_finish, mag_new, mag_push,
}

// ## Integers

/// 12,000 magnitude bytes with a nonzero first byte.
fn long_magnitude() -> ByteArray {
  pattern(12000, 3)
}

fn integer_of(negative: Bool, magnitude: ByteArray) -> Int {
  let m = builtin.bytearray_to_integer(True, magnitude)
  if negative {
    -1 - m
  } else {
    m
  }
}

/// The leaf the staged steps commit for `value`, and its words.
fn canonical_leaf(value: Int) -> (DataSummaryV1, Int) {
  expect SemanticConstantValue { payload, memory, .. } = semantic_integer_cbor(
    cbor.serialise(value),
  )
  (payload, memory)
}

/// CBOR of tag 2 or 3 over `argument` split into 64-byte segments, as the
/// canonical writer lays it out, without checking the argument is minimal.
fn segmented(negative: Bool, argument: ByteArray) -> ByteArray {
  encode_integer_cbor_bulk_v1(CanonicalIntegerV1 { negative, argument })
}

/// CBOR of tag 2 over `argument` as one definite byte string of 256 to
/// 65,535 bytes.
fn definite(argument: ByteArray) -> ByteArray {
  let length = bytearray.length(argument)
  bytearray.concat(
    builtin.cons_bytearray(
      0xc2,
      builtin.cons_bytearray(
        0x59,
        builtin.cons_bytearray(
          length / 256,
          builtin.cons_bytearray(length % 256, #""),
        ),
      ),
    ),
    argument,
  )
}

// ## The script decoder

fn decoded_in_steps(
  mag: script_decode_bits_v1.MagSink,
  bytes: ByteArray,
  step: Int,
) -> script_decode_bits_v1.MagSink {
  if bytearray.length(bytes) <= step {
    mag_push(mag, bytes)
  } else {
    decoded_in_steps(
      mag_push(mag, bytearray.take(bytes, step)),
      bytearray.drop(bytes, step),
      step,
    )
  }
}

/// The leaf the decoder commits for a magnitude read `step` bytes at a time.
fn decoded(
  negative: Bool,
  magnitude: ByteArray,
  step: Int,
) -> (DataSummaryV1, Int) {
  mag_finish(decoded_in_steps(mag_new(negative), magnitude, step))
}

test the_decoder_commits_a_long_integer_as_its_canonical_leaf() {
  and {
    decoded(False, long_magnitude(), 1000) == canonical_leaf(
      integer_of(False, long_magnitude()),
    ),
    decoded(True, long_magnitude(), 4095) == canonical_leaf(
      integer_of(True, long_magnitude()),
    ),
  }
}

test the_decoder_drops_leading_zeros_of_a_long_magnitude() {
  let spelled = bytearray.concat(#"0000000000", long_magnitude())
  and {
    decoded(False, spelled, 3) == canonical_leaf(
      integer_of(False, long_magnitude()),
    ),
    decoded(True, spelled, 1000) == canonical_leaf(
      integer_of(True, long_magnitude()),
    ),
  }
}

test the_decoder_reads_a_padded_small_magnitude_as_its_canonical_leaf() {
  and {
    decoded(False, #"0000000000000001", 3) == (integer_summary(1), 1),
    integer_summary(1) == cek_data_v1.semantic_data_summary_v1(
      builtin.i_data(1),
    ),
    decoded(True, #"00000000000000000000", 4) == (integer_summary(-1), 1),
    integer_summary(-1) == cek_data_v1.semantic_data_summary_v1(
      builtin.i_data(-1),
    ),
    decoded(False, bytearray.concat(#"00", pattern(64, 1)), 1) == (
      integer_summary(integer_of(False, pattern(64, 1))),
      8,
    ),
  }
}

// ## The ledger Data stream

fn streamed_from(
  control: cek_data_integer_v1.CekDataIntegerControlV1,
  source: ByteArray,
  source_start: Int,
) -> Option<cek_data_integer_v1.CekDataIntegerControlV1> {
  if control.stage == cek_data_integer_v1.stage_terminal {
    Some(control)
  } else {
    let source_end = source_start + bytearray.length(source)
    let next = when
      cek_data_integer_v1.next_source_span_v1(control, source_end)
    is {
      None -> cek_data_integer_v1.step_v1(control, source_end, None)
      Some(span) ->
        if span.absolute_start - source_start + span.length > bytearray.length(
          source,
        ) {
          None
        } else {
          cek_data_integer_v1.step_v1(
            control,
            source_end,
            Some(
              slice_len(source, span.absolute_start - source_start, span.length),
            ),
          )
        }
    }
    when next is {
      None -> None
      Some(control) -> streamed_from(control, source, source_start)
    }
  }
}

/// The leaf the ledger Data stream commits for an integer's CBOR, or None
/// when it refuses the spelling.
fn streamed(source: ByteArray) -> Option<DataSummaryV1> {
  when
    streamed_from(
      cek_data_integer_v1.initial_control_v1(17, bytearray.length(source)),
      source,
      17,
    )
  is {
    None -> None
    Some(terminal) -> cek_data_integer_v1.finalize_v1(terminal)
  }
}

test the_stream_commits_a_long_integer_as_its_canonical_leaf() {
  let positive = integer_of(False, long_magnitude())
  let negative = integer_of(True, long_magnitude())
  let (positive_leaf, _) = canonical_leaf(positive)
  let (negative_leaf, _) = canonical_leaf(negative)
  and {
    streamed(cbor.serialise(positive)) == Some(positive_leaf),
    streamed(cbor.serialise(negative)) == Some(negative_leaf),
  }
}

test the_stream_refuses_a_long_magnitude_with_a_leading_zero() {
  and {
    streamed(segmented(False, bytearray.concat(#"00", long_magnitude()))) == None,
    streamed(segmented(True, bytearray.concat(#"00", long_magnitude()))) == None,
  }
}

test the_stream_refuses_a_long_magnitude_as_one_definite_string() {
  streamed(definite(long_magnitude())) == None
}

test the_stream_refuses_an_empty_segment() {
  let first = pattern(64, 1)
  streamed(bytearray.concat(bytearray.concat(#"c25f5840", first), #"40ff")) == None
}

test the_stream_refuses_non_canonical_small_spellings() {
  and {
    // 23 behind a one-byte head.
    streamed(#"1817") == None,
    // 2^64 - 1 as an eight-byte bignum.
    streamed(#"c248ffffffffffffffff") == None,
    // 2^64 with a leading zero byte.
    streamed(#"c24a00010000000000000000") == None,
    // 2^64 as an indefinite string of 9 bytes.
    streamed(#"c25f49010000000000000000ff") == None,
    // 2^64 and -2^64 - 1 behind a long tag 2 or tag 3 head.
    streamed(#"d80249010000000000000000") == None,
    streamed(#"d80349010000000000000000") == None,
  }
}

// ## The staged integer reader and writer

test the_staged_writer_is_canonical_for_long_integers() {
  let positive = integer_of(False, long_magnitude())
  let negative = integer_of(True, long_magnitude())
  and {
    encode_integer_cbor_bulk_v1(
      parse_integer_cbor_bulk_v1(cbor.serialise(positive)),
    ) == cbor.serialise(positive),
    encode_integer_cbor_bulk_v1(
      parse_integer_cbor_bulk_v1(cbor.serialise(negative)),
    ) == cbor.serialise(negative),
  }
}

test the_staged_reader_refuses_a_long_magnitude_with_a_leading_zero() fail {
  let integer = parse_integer_cbor_bulk_v1(
    segmented(False, bytearray.concat(#"00", long_magnitude())),
  )
  integer.negative == False
}

test the_staged_reader_refuses_a_negative_long_magnitude_with_a_leading_zero() fail {
  let integer = parse_integer_cbor_bulk_v1(
    segmented(True, bytearray.concat(#"00", long_magnitude())),
  )
  integer.negative
}

test the_staged_reader_refuses_a_long_magnitude_as_one_definite_string() fail {
  let integer = parse_integer_cbor_bulk_v1(definite(long_magnitude()))
  integer.negative == False
}

test the_staged_reader_refuses_a_bignum_behind_a_long_tag_head() fail {
  let integer = parse_integer_cbor_bulk_v1(#"d80249010000000000000000")
  integer.negative == False
}

test the_staged_reader_refuses_a_small_integer_behind_a_long_head() fail {
  let integer = parse_integer_cbor_bulk_v1(#"1817")
  integer.negative == False
}

// ## Direct constants

test a_direct_integer_constant_commits_its_canonical_leaf() {
  let check = fn(value: Int) {
    let encoded = cbor.serialise(value)
    cek_data_v1.semantic_data_summary_v1(builtin.i_data(value)) == cek_data_v1.integer_data_summary_v1(
      value,
      cek_data_v1.bounded_blob_root_v1(encoded),
    )
  }
  and {
    check(0),
    check(-1),
    check(18446744073709551616),
    check(-18446744073709551617),
    check(integer_of(False, pattern(4000, 1))),
  }
}
