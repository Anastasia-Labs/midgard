//// The Plutus Data CBOR decoder against the Cardano ledger's rules, through
//// both of its entries.
////
//// The Data entry (`deserialise`, `expect_deserialise`) decodes exactly what
//// the ledger's `decodeData` accepts, to the same value, and refuses the rest.
//// The envelope entry (`deserialise_envelope`, `expect_deserialise_envelope`)
//// does the same, except that it also accepts byte strings and chunks longer
//// than 64 bytes. The vectors whose outcomes come from the ledger's decoder
//// itself, through the TypeScript reader pinned against it, are in the
//// generated `cbor-deserialise-v1-golden.test.ak`.

use aiken/builtin
use aiken/collection/list
use aiken/fuzz
use aiken/math
use aiken/primitive/bytearray
use midgard/cbor_deserialise
use midgard/fraud_proofs/native_tx/codec

// ## Helpers

fn i(n: Int) -> Data {
  builtin.i_data(n)
}

fn b(bytes: ByteArray) -> Data {
  builtin.b_data(bytes)
}

fn l(items: List<Data>) -> Data {
  builtin.list_data(items)
}

fn m(entries: Pairs<Data, Data>) -> Data {
  builtin.map_data(entries)
}

fn c(index: Int, fields: List<Data>) -> Data {
  builtin.constr_data(index, fields)
}

fn ab(count: Int) -> ByteArray {
  builtin.replicate_byte(count, 0xab)
}

fn data_entry_decodes_to(bytes: ByteArray, expected: Data) -> Bool {
  and {
    cbor_deserialise.deserialise(bytes) == Some(expected),
    {
      let data <- cbor_deserialise.expect_deserialise(bytes)
      data == expected
    },
  }
}

fn envelope_entry_decodes_to(bytes: ByteArray, expected: Data) -> Bool {
  and {
    cbor_deserialise.deserialise_envelope(bytes) == Some(expected),
    {
      let data <- cbor_deserialise.expect_deserialise_envelope(bytes)
      data == expected
    },
  }
}

fn decodes_to(bytes: ByteArray, expected: Data) -> Bool {
  and {
    data_entry_decodes_to(bytes, expected),
    envelope_entry_decodes_to(bytes, expected),
  }
}

fn round_trips(data: Data) -> Bool {
  decodes_to(builtin.serialise_data(data), data)
}

// True whenever the failing variant returns, so a `fail` test over it fails
// only when the script fails.
fn expect_returns(bytes: ByteArray) -> Bool {
  let data <- cbor_deserialise.expect_deserialise(bytes)
  builtin.serialise_data(data) != #""
}

fn expect_envelope_returns(bytes: ByteArray) -> Bool {
  let data <- cbor_deserialise.expect_deserialise_envelope(bytes)
  builtin.serialise_data(data) != #""
}

// Both entries give the same value wherever the Data entry accepts, and each
// failing variant returns the value its `Option` variant returns.
fn entries_agree(bytes: ByteArray) -> Bool {
  let from_data = cbor_deserialise.deserialise(bytes)
  let from_envelope = cbor_deserialise.deserialise_envelope(bytes)
  and {
    when from_data is {
      Some(value) -> and {
          from_envelope == from_data,
          {
            let decoded <- cbor_deserialise.expect_deserialise(bytes)
            decoded == value
          },
        }
      None -> True
    },
    when from_envelope is {
      Some(value) -> {
        let decoded <- cbor_deserialise.expect_deserialise_envelope(bytes)
        decoded == value
      }
      None -> True
    },
  }
}

// ## Round trips through `serialise_data`

test prop_round_trips_serialised_data(data via fuzz.data()) {
  round_trips(data)
}

test round_trips_every_constructor_index_width() {
  list.all(
    [0, 6, 7, 127, 128, 1000, 4294967296, 18446744073709551615],
    fn(index) { round_trips(c(index, [i(index)])) },
  )
}

test round_trips_integers_beyond_64_bits() {
  list.all(
    [
      18446744073709551615,
      18446744073709551616,
      -18446744073709551616,
      -18446744073709551617,
      math.pow(2, 512) - 1,
      math.pow(2, 600),
      -math.pow(2, 600),
    ],
    fn(n) { round_trips(i(n)) },
  )
}

// A magnitude over 64 bytes is written as an indefinite string of chunks.
test round_trips_a_bignum_written_in_several_chunks() {
  let bytes = builtin.serialise_data(i(math.pow(2, 600)))
  and {
    bytearray.take(bytes, 4) == #"c25f5840",
    data_entry_decodes_to(bytes, i(math.pow(2, 600))),
  }
}

test round_trips_large_constructors_nested_in_lists_and_maps() {
  round_trips(
    l(
      [
        c(128, []),
        m(
          [
            Pair(c(1000, [i(math.pow(2, 70))]), c(200, [])),
            Pair(i(0), l([c(18446744073709551615, [b(ab(65))])])),
          ],
        ),
      ],
    ),
  )
}

test round_trips_byte_strings_of_every_chunk_count() {
  list.all(
    [0, 1, 23, 24, 63, 64, 65, 128, 129, 1000],
    fn(count) { round_trips(b(ab(count))) },
  )
}

// ## Forms the ledger accepts but does not write

test decodes_non_minimal_heads() {
  and {
    decodes_to(#"1800", i(0)),
    decodes_to(#"190000", i(0)),
    decodes_to(#"1b0000000000000001", i(1)),
    decodes_to(#"1bffffffffffffffff", i(18446744073709551615)),
    decodes_to(#"3bffffffffffffffff", i(-18446744073709551616)),
    decodes_to(#"5800", b(#"")),
    decodes_to(#"5b0000000000000000", b(#"")),
    decodes_to(#"98020001", l([i(0), i(1)])),
    decodes_to(#"b900010203", m([Pair(i(2), i(3))])),
    decodes_to(#"d9007980", c(0, [])),
    decodes_to(#"db000000000000007980", c(0, [])),
  }
}

test decodes_indefinite_lists_maps_and_byte_strings() {
  and {
    decodes_to(#"9fff", l([])),
    decodes_to(#"9f00ff", l([i(0)])),
    decodes_to(#"bfff", m([])),
    decodes_to(#"bf0000ff", m([Pair(i(0), i(0))])),
    decodes_to(#"5fff", b(#"")),
    decodes_to(#"5f40ff", b(#"")),
    decodes_to(#"5f41aaff", b(#"aa")),
    decodes_to(#"5f41aa42bbccff", b(#"aabbcc")),
    decodes_to(#"5f5b0000000000000000ff", b(#"")),
    decodes_to(#"d8799fff", c(0, [])),
    decodes_to(#"d87a9f00ff", c(1, [i(0)])),
  }
}

test decodes_map_entries_in_order_with_duplicate_keys() {
  and {
    decodes_to(#"a200000001", m([Pair(i(0), i(0)), Pair(i(0), i(1))])),
    decodes_to(#"a201000000", m([Pair(i(1), i(0)), Pair(i(0), i(0))])),
    decodes_to(#"bf8000a000ff", m([Pair(l([]), i(0)), Pair(m([]), i(0))])),
  }
}

test decodes_bignums() {
  and {
    decodes_to(#"c249010000000000000000", i(18446744073709551616)),
    decodes_to(#"c349010000000000000000", i(-18446744073709551617)),
    decodes_to(#"c349ffffffffffffffffff", i(-4722366482869645213696)),
    decodes_to(#"c3484000000000000000", i(-4611686018427387905)),
    decodes_to(#"c24105", i(5)),
    decodes_to(#"c240", i(0)),
    decodes_to(#"c340", i(-1)),
    decodes_to(#"c25fff", i(0)),
    decodes_to(#"c25f40ff", i(0)),
    decodes_to(#"c25f4101ff", i(1)),
    decodes_to(#"c25f41014102ff", i(258)),
  }
}

test decodes_every_constructor_tag_form() {
  and {
    decodes_to(#"d87980", c(0, [])),
    decodes_to(#"d87f80", c(6, [])),
    decodes_to(#"d9050080", c(7, [])),
    decodes_to(#"d9055f80", c(102, [])),
    decodes_to(#"d9057880", c(127, [])),
    decodes_to(#"d866820080", c(0, [])),
    decodes_to(#"d866821a0000000180", c(1, [])),
    decodes_to(#"d86698020080", c(0, [])),
    decodes_to(#"d8669f0080ff", c(0, [])),
    decodes_to(#"d8669f187f80ff", c(127, [])),
    decodes_to(#"d8669f18809f00ffff", c(128, [i(0)])),
    decodes_to(#"d86682188080", c(128, [])),
    decodes_to(#"d866821903e880", c(1000, [])),
    decodes_to(#"d866821bffffffffffffffff80", c(18446744073709551615, [])),
    decodes_to(#"d87981d866821b000000010000000080", c(0, [c(4294967296, [])])),
  }
}

test decodes_an_empty_byte_string_as_the_last_byte() {
  and {
    decodes_to(#"40", b(#"")),
    decodes_to(#"8140", l([b(#"")])),
    decodes_to(#"a10040", m([Pair(i(0), b(#""))])),
    decodes_to(#"d87a8140", c(1, [b(#"")])),
    decodes_to(#"82405f40ff", l([b(#""), b(#"")])),
  }
}

// An empty byte string that ends the input, against the Cardano ledger
// (plutus-core 1.71, protocol version 11, reading each payload as a Data
// constant): it accepts each payload below to the value given, and refuses a
// byte string head that claims one byte more than the input holds.
test decodes_an_empty_byte_string_ending_the_input_as_the_ledger_does() {
  and {
    decodes_to(#"824040", l([b(#""), b(#"")])),
    decodes_to(#"818140", l([l([b(#"")])])),
    decodes_to(#"d86682018140", c(1, [b(#"")])),
    decodes_to(#"a14040", m([Pair(b(#""), b(#""))])),
    decodes_to(#"a146c84797cfa72140", m([Pair(b(#"c84797cfa721"), b(#""))])),
    decodes_to(
      #"a2448d3ed5d702448d3ed5d740",
      m([Pair(b(#"8d3ed5d7"), i(2)), Pair(b(#"8d3ed5d7"), b(#""))]),
    ),
  }
}

test decodes_the_item_after_an_empty_byte_string() {
  and {
    decodes_to(#"824000", l([b(#""), i(0)])),
    decodes_to(#"82405fff", l([b(#""), b(#"")])),
    decodes_to(#"9f40ff", l([b(#"")])),
    decodes_to(#"d87a9f40ff", c(1, [b(#"")])),
    decodes_to(#"d8799f40ff", c(0, [b(#"")])),
  }
}

test decodes_other_empty_items_ending_the_input_as_the_ledger_does() {
  and {
    decodes_to(#"5fff", b(#"")),
    decodes_to(#"815fff", l([b(#"")])),
    decodes_to(#"c240", i(0)),
    decodes_to(#"c340", i(-1)),
    decodes_to(#"81c240", l([i(0)])),
    decodes_to(#"80", l([])),
    decodes_to(#"8180", l([l([])])),
    decodes_to(#"819fff", l([l([])])),
    decodes_to(#"a0", m([])),
    decodes_to(#"81a0", l([m([])])),
  }
}

test both_entries_refuse_a_byte_string_claiming_one_byte_past_the_end() {
  list.all(
    [#"41", #"8141", #"a10041", #"d87a8141", #"60", #"8160"],
    fn(bytes) {
      and {
        cbor_deserialise.deserialise(bytes) == None,
        cbor_deserialise.deserialise_envelope(bytes) == None,
      }
    },
  )
}

test decodes_byte_strings_of_64_bytes_and_longer_ones_in_chunks() {
  and {
    decodes_to(bytearray.concat(#"5840", ab(64)), b(ab(64))),
    decodes_to(
      bytearray.concat(#"5f5840", bytearray.concat(ab(64), #"41abff")),
      b(ab(65)),
    ),
    decodes_to(
      bytearray.concat(
        #"5f5840",
        bytearray.concat(ab(64), bytearray.concat(#"5840", ab(64)))
          |> bytearray.concat(#"ff"),
      ),
      b(ab(128)),
    ),
    decodes_to(
      bytearray.concat(#"c25840", ab(64)),
      i(builtin.bytearray_to_integer(True, ab(64))),
    ),
  }
}

// ## Refused by both entries

const refused_by_both_entries: List<ByteArray> =
  [
    // Truncated, trailing bytes, misplaced breaks.
    #"", #"18", #"19ff", #"1a0001", #"41", #"5801", #"5902ab", #"5b00000000",
    #"81", #"8119", #"9f", #"9f00", #"a1", #"a100", #"bf00ff", #"bf000000ff",
    #"d879", #"81d9", #"5f", #"5f41aa", #"0000", #"00ff", #"80ff", #"d87980ff00",
    #"ff", #"8100ff", #"81ff", #"1f00", #"8200",
    // Reserved and indefinite heads.
    #"1c", #"1d", #"1e", #"1f", #"3c", #"3f", #"5c", #"5d", #"5e", #"9c", #"bc",
    #"dc", #"dd", #"de", #"df00", #"fc",
    // Text strings, simple values and floats.
    #"60", #"6161", #"780161", #"7f61ff", #"8160", #"a16000", #"f4", #"f5",
    #"f6", #"f7", #"f800", #"f93c00", #"fa3f800000", #"fb3ff0000000000000",
    #"81f5",
    // Chunks that are not definite byte strings.
    #"5f5f41aaffff", #"5f5fff41aaff", #"5f00ff", #"5f3fff", #"5f0001", #"5f80ff",
    #"5f60ff", #"5f6101ff",
    // Bignums.
    #"c200", #"c280", #"c29f", #"c26101", #"c2c249010000000000000000",
    #"c25f5f40ffff", #"c25f4101410000ff", #"d8024101", #"d8034101",
    #"d9000241ff",
    // Unknown tags, and constructor tags over anything but a list.
    #"c000", #"c100", #"c400", #"d81800", #"d87880", #"d904ff80", #"d9057980",
    #"d95f80", #"d9ffff00", #"da0001000000", #"dbffffffffffffffff00", #"c0c000",
    #"d87900", #"d87940", #"d879a0", #"d879d87980", #"d8798000", #"d87981f6",
    // Tag 102 that is not a two-element array of a word and a list.
    #"d8664101", #"d8668201", #"d86683000080", #"d866820180ff",
    #"d8669f008000ff", #"d8669f0080", #"d866820083", #"d866820040",
    #"d8668200bf", #"d866829f00ff80", #"d86682c2410180",
    #"d86682c24901000000000000000080", #"d866823880", #"d866990002",
    #"d8668218668080",
    // Long declared lengths.
    #"990fff", #"b9ffff", #"9affffffff", #"9bffffffffffffffff",
  ]

test both_entries_refuse_what_the_ledger_refuses() {
  list.all(
    refused_by_both_entries,
    fn(bytes) {
      and {
        cbor_deserialise.deserialise(bytes) == None,
        cbor_deserialise.deserialise_envelope(bytes) == None,
      }
    },
  )
}

// ## Longer than 64 bytes: refused by the Data entry, accepted by the
// envelope entry

fn long_vectors() -> Pairs<ByteArray, Data> {
  [
    Pair(bytearray.concat(#"5841", ab(65)), b(ab(65))),
    Pair(bytearray.concat(#"59012c", ab(300)), b(ab(300))),
    Pair(bytearray.concat(#"5a0000012c", ab(300)), b(ab(300))),
    Pair(bytearray.concat(#"5b0000000000000041", ab(65)), b(ab(65))),
    Pair(
      bytearray.concat(#"5f5841", bytearray.concat(ab(65), #"ff")),
      b(ab(65)),
    ),
    Pair(
      bytearray.concat(
        #"5f5841",
        bytearray.concat(ab(65), bytearray.concat(#"5840", ab(64)))
          |> bytearray.concat(#"ff"),
      ),
      b(ab(129)),
    ),
    Pair(
      bytearray.concat(#"c25841", ab(65)),
      i(builtin.bytearray_to_integer(True, ab(65))),
    ),
    Pair(
      bytearray.concat(#"c35841", ab(65)),
      i(-1 - builtin.bytearray_to_integer(True, ab(65))),
    ),
    Pair(bytearray.concat(#"d8798159012c", ab(300)), c(0, [b(ab(300))])),
    Pair(
      bytearray.concat(#"a15841", bytearray.concat(ab(65), #"00")),
      m([Pair(b(ab(65)), i(0))]),
    ),
  ]
}

test the_data_entry_refuses_a_byte_string_or_chunk_over_64_bytes() {
  list.all(
    long_vectors(),
    fn(Pair(bytes, _)) { cbor_deserialise.deserialise(bytes) == None },
  )
}

test the_envelope_entry_accepts_a_byte_string_or_chunk_over_64_bytes() {
  list.all(
    long_vectors(),
    fn(Pair(bytes, expected)) { envelope_entry_decodes_to(bytes, expected) },
  )
}

// `count` bytes 0x5a, built in pieces of at most 8192 bytes.
fn filler(count: Int) -> ByteArray {
  if count <= 8192 {
    builtin.replicate_byte(count, 0x5a)
  } else {
    builtin.append_bytearray(
      builtin.replicate_byte(8192, 0x5a),
      filler(count - 8192),
    )
  }
}

// Every length the on-chain envelope writer emits reads back, up to the
// longest a script can read. From protocol version 11 the Plutus evaluator
// refuses a byte string argument over 65536 bytes, so 65533 bytes, under a
// three-byte head, is the longest payload whose encoding can be read.
test the_envelope_entry_reads_back_encode_definite_bytes() {
  list.all(
    [0, 1, 23, 24, 64, 65, 255, 256, 4095, 65533],
    fn(count) {
      let payload = filler(count)
      envelope_entry_decodes_to(
        codec.encode_definite_bytes(payload),
        b(payload),
      )
    },
  )
}

// One payload byte more makes the encoding 65537 bytes, too long for the
// evaluator to read; Cardano's refuses it the same way.
test the_envelope_entry_cannot_read_an_encoding_over_65536_bytes() fail {
  let payload = filler(65534)
  cbor_deserialise.deserialise_envelope(codec.encode_definite_bytes(payload)) == Some(
    b(payload),
  )
}

test expect_envelope_cannot_read_an_encoding_over_65536_bytes() fail {
  let payload = filler(65534)
  let data <- cbor_deserialise.expect_deserialise_envelope(
    codec.encode_definite_bytes(payload),
  )
  data == b(payload)
}

// ## Agreement between the entries and variants on arbitrary bytes

fn header_bytes() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.list_between(
      fuzz.one_of(
        [
          0x0, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x20, 0x38, 0x3b, 0x40, 0x41,
          0x58, 0x5b, 0x5f, 0x60, 0x80, 0x81, 0x82, 0x83, 0x98, 0x99, 0x9f, 0xa0,
          0xa1, 0xb8, 0xbf, 0xc2, 0xc3, 0xd8, 0xd9, 0xdb, 0x79, 0x7a, 0x66, 0x5,
          0xe0, 0xff,
        ],
      ),
      1,
      12,
    ),
    list_to_bytes,
  )
}

fn list_to_bytes(xs: List<Int>) -> ByteArray {
  when xs is {
    [] -> #""
    [x, ..rest] -> bytearray.push(list_to_bytes(rest), x)
  }
}

fn truncations() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple(fuzz.data(), fuzz.int_between(0, 4096)),
    fn((data, cut)) {
      let bytes = builtin.serialise_data(data)
      bytearray.take(bytes, cut % bytearray.length(bytes))
    },
  )
}

fn with_trailing_bytes() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple(fuzz.data(), fuzz.bytearray_between(1, 4)),
    fn((data, tail)) { bytearray.concat(builtin.serialise_data(data), tail) },
  )
}

fn with_one_replaced_byte() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple3(fuzz.data(), fuzz.int_between(0, 4096), fuzz.byte()),
    fn((data, at, byte)) {
      let bytes = builtin.serialise_data(data)
      let at = at % bytearray.length(bytes)
      bytearray.concat(
        bytearray.take(bytes, at),
        bytearray.push(bytearray.drop(bytes, at + 1), byte),
      )
    },
  )
}

test prop_entries_agree_on_random_bytes(bytes via fuzz.bytearray_between(0, 16)) {
  entries_agree(bytes)
}

test prop_entries_agree_on_header_bytes(bytes via header_bytes()) {
  entries_agree(bytes)
}

test prop_entries_agree_on_truncations(bytes via truncations()) {
  entries_agree(bytes)
}

test prop_entries_agree_on_trailing_bytes(bytes via with_trailing_bytes()) {
  entries_agree(bytes)
}

test prop_entries_agree_on_one_replaced_byte(bytes via with_one_replaced_byte()) {
  entries_agree(bytes)
}

// A `fail` property over these passes when, for every input, the failing
// variant fails the script or the input is outside the tested domain
// (accepted).
fn expect_fails_where_refused(bytes: ByteArray) -> Bool {
  if cbor_deserialise.deserialise(bytes) == None {
    expect_returns(bytes)
  } else {
    fail
  }
}

fn expect_envelope_fails_where_refused(bytes: ByteArray) -> Bool {
  if cbor_deserialise.deserialise_envelope(bytes) == None {
    expect_envelope_returns(bytes)
  } else {
    fail
  }
}

test prop_expect_fails_where_refused_on_header_bytes(
  bytes via header_bytes(),
) fail {
  expect_fails_where_refused(bytes)
}

test prop_expect_fails_where_refused_on_one_replaced_byte(
  bytes via with_one_replaced_byte(),
) fail {
  expect_fails_where_refused(bytes)
}

test prop_expect_fails_where_refused_on_truncations(
  bytes via truncations(),
) fail {
  expect_fails_where_refused(bytes)
}

test prop_expect_envelope_fails_where_refused_on_header_bytes(
  bytes via header_bytes(),
) fail {
  expect_envelope_fails_where_refused(bytes)
}

test prop_expect_envelope_fails_where_refused_on_trailing_bytes(
  bytes via with_trailing_bytes(),
) fail {
  expect_envelope_fails_where_refused(bytes)
}

// ## The failing variants fail the script on refused inputs

test expect_fails_on_the_empty_input() fail {
  expect_returns(#"")
}

test expect_fails_on_a_trailing_byte() fail {
  expect_returns(#"0000")
}

test expect_fails_on_a_byte_string_running_past_the_end() fail {
  expect_returns(#"5801")
}

test expect_fails_on_a_byte_string_claiming_one_byte_at_the_end() fail {
  expect_returns(#"41")
}

test expect_fails_on_a_list_ending_in_a_byte_string_claiming_one_byte() fail {
  expect_returns(#"8141")
}

test expect_fails_on_a_map_ending_in_a_byte_string_claiming_one_byte() fail {
  expect_returns(#"a10041")
}

test expect_fails_on_a_short_wide_argument() fail {
  expect_returns(#"1a0001")
}

test expect_fails_on_a_reserved_head() fail {
  expect_returns(#"1c")
}

test expect_fails_on_an_indefinite_integer() fail {
  expect_returns(#"1f00")
}

test expect_fails_on_a_text_string() fail {
  expect_returns(#"6161")
}

test expect_fails_on_a_simple_value() fail {
  expect_returns(#"f6")
}

test expect_fails_on_a_float() fail {
  expect_returns(#"fb3ff0000000000000")
}

test expect_fails_on_a_break_as_an_item() fail {
  expect_returns(#"ff")
}

test expect_fails_on_a_break_in_a_definite_list() fail {
  expect_returns(#"8100ff")
}

test expect_fails_on_an_odd_indefinite_map() fail {
  expect_returns(#"bf00ff")
}

test expect_fails_on_a_nested_indefinite_chunk() fail {
  expect_returns(#"5f5f41aaffff")
}

test expect_fails_on_a_chunk_that_is_not_a_byte_string() fail {
  expect_returns(#"5f00ff")
}

test expect_fails_on_a_bignum_over_a_list() fail {
  expect_returns(#"c280")
}

test expect_fails_on_a_bignum_tag_with_a_long_head() fail {
  expect_returns(#"d8024101")
}

test expect_fails_on_an_unknown_tag() fail {
  expect_returns(#"d87880")
}

test expect_fails_on_a_tag_above_the_constructor_range() fail {
  expect_returns(#"d9057980")
}

test expect_fails_on_a_constructor_tag_over_an_integer() fail {
  expect_returns(#"d87900")
}

test expect_fails_on_a_constructor_tag_over_a_map() fail {
  expect_returns(#"d879a0")
}

test expect_fails_on_tag_102_over_three_elements() fail {
  expect_returns(#"d86683000080")
}

test expect_fails_on_tag_102_with_a_bignum_index() fail {
  expect_returns(#"d86682c2410180")
}

test expect_fails_on_tag_102_with_a_negative_index() fail {
  expect_returns(#"d866823880")
}

test expect_fails_on_tag_102_over_fields_that_are_not_a_list() fail {
  expect_returns(#"d866820040")
}

test expect_fails_on_an_indefinite_tag_102_without_its_break() fail {
  expect_returns(#"d8669f008000ff")
}

test expect_fails_on_a_65_byte_string() fail {
  expect_returns(bytearray.concat(#"5841", ab(65)))
}

test expect_fails_on_a_65_byte_chunk() fail {
  expect_returns(bytearray.concat(#"5f5841", bytearray.concat(ab(65), #"ff")))
}

test expect_fails_on_a_65_byte_bignum_magnitude() fail {
  expect_returns(bytearray.concat(#"c25841", ab(65)))
}

test expect_envelope_fails_on_a_trailing_byte() fail {
  expect_envelope_returns(
    bytearray.concat(#"5841", bytearray.concat(ab(65), #"00")),
  )
}

test expect_envelope_fails_on_a_byte_string_running_past_the_end() fail {
  expect_envelope_returns(bytearray.concat(#"5842", ab(65)))
}

test expect_envelope_fails_on_a_byte_string_claiming_one_byte_at_the_end() fail {
  expect_envelope_returns(#"41")
}

test expect_envelope_fails_on_a_list_ending_in_a_byte_string_claiming_one_byte() fail {
  expect_envelope_returns(#"8141")
}

test expect_envelope_fails_on_a_nested_indefinite_chunk() fail {
  expect_envelope_returns(#"5f5f41aaffff")
}

test expect_envelope_fails_on_tag_102_over_three_elements() fail {
  expect_envelope_returns(#"d86683000080")
}

test expect_envelope_fails_on_a_bignum_tag_with_a_long_head() fail {
  expect_envelope_returns(#"d8024101")
}

test expect_envelope_fails_on_a_text_string() fail {
  expect_envelope_returns(#"6161")
}

// ## A refused input that declares a long list or map
//
// Each variant stops at the first refused read, so the cost of such an input
// is bounded by the bytes it holds, not by the length it declares.

test deserialise_refuses_a_long_declared_list() {
  cbor_deserialise.deserialise(#"990fff") == None
}

test deserialise_refuses_a_long_declared_map() {
  cbor_deserialise.deserialise(#"b9ffff") == None
}

test expect_fails_on_a_long_declared_list() fail {
  expect_returns(#"990fff")
}

test expect_fails_on_a_long_declared_map() fail {
  expect_returns(#"b9ffff")
}

test expect_envelope_fails_on_a_long_declared_list() fail {
  expect_envelope_returns(#"9bffffffffffffffff")
}

// ## The failing variants decode where they are bound
//
// A refused input fails the script even on a path that never uses the decoded
// value.

test expect_fails_on_a_path_that_does_not_use_the_value(
  n via fuzz.int_between(0, 0),
) fail {
  let data <- cbor_deserialise.expect_deserialise(#"ff")
  n == 0 || builtin.serialise_data(data) == #""
}

test expect_fails_when_the_value_is_unused() fail {
  let _data <- cbor_deserialise.expect_deserialise(#"8119")
  True
}

test expect_envelope_fails_when_the_value_is_unused() fail {
  let _data <- cbor_deserialise.expect_deserialise_envelope(#"8119")
  True
}
