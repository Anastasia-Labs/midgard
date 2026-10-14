//// Differential tests against the standard library's `aiken/cbor.deserialise`:
//// `cbor_deserialise.deserialise` must return exactly what it returns, and
//// `cbor_deserialise.expect_deserialise` must return the same `data` where it
//// returns `Some(data)` and fail the script where it returns `None`.

use aiken/builtin
use aiken/cbor
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/cbor_deserialise

fn agrees(bytes: ByteArray) -> Bool {
  let expected = cbor.deserialise(bytes)
  and {
    cbor_deserialise.deserialise(bytes) == expected,
    when expected is {
      Some(data) -> {
        let decoded <- cbor_deserialise.expect_deserialise(bytes)
        decoded == data
      }
      None -> True
    },
  }
}

// True whenever `expect_deserialise` returns, so a `fail` test over it fails
// only when the script fails.
fn expect_returns(bytes: ByteArray) -> Bool {
  let data <- cbor_deserialise.expect_deserialise(bytes)
  builtin.serialise_data(data) != #""
}

// A `fail` test over this passes when, for every input, the failing variant
// fails the script or the input is outside the tested domain: unfuzzable, or
// accepted by the library.
fn expect_fails_where_refused(bytes: ByteArray) -> Bool {
  if is_fuzzable(bytes) && cbor.deserialise(bytes) == None {
    expect_returns(bytes)
  } else {
    fail
  }
}

// Random inputs are tested unless one of the decoders could reach a path the
// test runner cannot execute. `scan_item` follows the input the way both
// decoders read it, up to the first refusal, after which neither reads
// another byte, and reports a hazard on:
// - a chunk header below 0x40 inside an indefinite byte string: it moves the
//   position back, and both decoders then fail the script on a read before
//   the start of the input, or re-read the same header forever;
// - a tag below 121 other than the refused 102, or any 8-byte tag: the
//   constructor index is then negative or too wide for the test runner's
//   evaluator;
// - a byte-string or chunk head with an 8-byte width: it overflows the test
//   runner's slice;
// - a list, map or field-count head with a 4- or 8-byte length: after a
//   failed read the library still steps through every remaining declared
//   element, which can take billions of steps.
// The same bytes inside a byte string's payload, or after the first refusal,
// are not read as structure and do not exclude an input. Fixed cases below
// cover each hazard domain instead.
fn is_fuzzable(bytes: ByteArray) -> Bool {
  scan_item(bytes, 0, bytearray.length(bytes)) != hazard
}

// `scan_*` return the offset after what they read, `refused` where both
// decoders refuse the input, or `hazard`.
const hazard = -1

const refused = -2

fn scan_item(bytes: ByteArray, at: Int, length: Int) -> Int {
  if at >= length {
    refused
  } else {
    let byte = builtin.index_bytearray(bytes, at)
    let at = at + 1
    if byte < 0x40 {
      scan_argument(bytes, at, length, byte % 0x20, fn(_, at) { at })
    } else if byte == 0x5f {
      scan_chunks(bytes, at, length)
    } else if byte < 0x60 {
      scan_byte_string(bytes, at, length, byte - 0x40)
    } else if byte < 0x80 {
      refused
    } else if byte < 0xa0 {
      scan_container(bytes, at, length, byte - 0x80, 1)
    } else if byte < 0xc0 {
      scan_container(bytes, at, length, byte - 0xa0, 2)
    } else if byte == 0xdb {
      hazard
    } else if byte < 0xe0 {
      scan_argument(
        bytes,
        at,
        length,
        byte - 0xc0,
        fn(tag, at) {
          if at >= length || tag == 102 {
            refused
          } else if tag < 121 {
            hazard
          } else {
            let fields = builtin.index_bytearray(bytes, at)
            scan_container(bytes, at + 1, length, fields - 0x80, 1)
          }
        },
      )
    } else {
      refused
    }
  }
}

fn scan_argument(
  bytes: ByteArray,
  at: Int,
  length: Int,
  header: Int,
  return: fn(Int, Int) -> Int,
) -> Int {
  if header < 24 {
    return(header, at)
  } else if at >= length {
    refused
  } else if header == 24 {
    return(builtin.index_bytearray(bytes, at), at + 1)
  } else {
    let width = if header == 25 {
      2
    } else if header == 26 {
      4
    } else if header == 27 {
      8
    } else {
      0
    }
    if width == 0 || at + width > length {
      refused
    } else {
      return(
        builtin.bytearray_to_integer(
          True,
          builtin.slice_bytearray(at, width, bytes),
        ),
        at + width,
      )
    }
  }
}

fn scan_byte_string(bytes: ByteArray, at: Int, length: Int, header: Int) -> Int {
  if header == 27 {
    hazard
  } else {
    scan_argument(
      bytes,
      at,
      length,
      header,
      fn(width, at) {
        if at >= length || at + width > length {
          refused
        } else {
          at + width
        }
      },
    )
  }
}

fn scan_chunks(bytes: ByteArray, at: Int, length: Int) -> Int {
  if at >= length {
    refused
  } else {
    let byte = builtin.index_bytearray(bytes, at)
    if byte == 0xff {
      at + 1
    } else if byte < 0x40 {
      hazard
    } else {
      let next = scan_byte_string(bytes, at + 1, length, byte - 0x40)
      if next < 0 {
        next
      } else {
        scan_chunks(bytes, next, length)
      }
    }
  }
}

// `per` items make one element: 1 for a list or a constructor's fields, 2 for
// a map.
fn scan_container(
  bytes: ByteArray,
  at: Int,
  length: Int,
  header: Int,
  per: Int,
) -> Int {
  if header == 31 {
    scan_indefinite(bytes, at, length, per)
  } else if header == 26 || header == 27 {
    hazard
  } else {
    scan_argument(
      bytes,
      at,
      length,
      header,
      fn(size, at) { scan_items(bytes, at, length, size * per) },
    )
  }
}

fn scan_items(bytes: ByteArray, at: Int, length: Int, count: Int) -> Int {
  if count <= 0 {
    at
  } else {
    let next = scan_item(bytes, at, length)
    if next < 0 {
      next
    } else {
      scan_items(bytes, next, length, count - 1)
    }
  }
}

fn scan_indefinite(bytes: ByteArray, at: Int, length: Int, per: Int) -> Int {
  if at >= length {
    refused
  } else if builtin.index_bytearray(bytes, at) == 0xff {
    at + 1
  } else {
    let next = scan_items(bytes, at, length, per)
    if next < 0 {
      next
    } else {
      scan_indefinite(bytes, next, length, per)
    }
  }
}

// The library refuses tag 102, which encodes constructor indexes from 128 up,
// so only values free of those round-trip.
test prop_agrees_on_serialised_data(data via fuzz.data()) {
  let bytes = cbor.serialise(data)
  and {
    agrees(bytes),
    when cbor.deserialise(bytes) is {
      Some(decoded) -> decoded == data
      None -> True
    },
  }
}

test prop_agrees_on_every_truncation(
  (data, cut) via fuzz.tuple(fuzz.data(), fuzz.int_between(0, 4096)),
) {
  let bytes = cbor.serialise(data)
  let prefix = bytearray.take(bytes, cut % bytearray.length(bytes))
  if is_fuzzable(prefix) {
    agrees(prefix)
  } else {
    True
  }
}

test prop_agrees_on_trailing_bytes(
  (data, tail) via fuzz.tuple(fuzz.data(), fuzz.bytearray_between(1, 4)),
) {
  let bytes = bytearray.concat(cbor.serialise(data), tail)
  if is_fuzzable(bytes) {
    agrees(bytes)
  } else {
    True
  }
}

test prop_agrees_on_one_replaced_byte(
  (data, at, byte) via fuzz.tuple3(
    fuzz.data(),
    fuzz.int_between(0, 4096),
    fuzz.byte(),
  ),
) {
  let bytes = cbor.serialise(data)
  let at = at % bytearray.length(bytes)
  let mutated = bytearray.concat(
    bytearray.take(bytes, at),
    bytearray.push(bytearray.drop(bytes, at + 1), byte),
  )
  if is_fuzzable(mutated) {
    agrees(mutated)
  } else {
    True
  }
}

test prop_agrees_on_random_bytes(bytes via fuzz.bytearray()) {
  if is_fuzzable(bytes) {
    agrees(bytes)
  } else {
    True
  }
}

fn header_bytes() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.list_between(
      fuzz.one_of(
        [
          0x0, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x20, 0x38, 0x40, 0x41, 0x58,
          0x5b, 0x60, 0x80, 0x81, 0x83, 0x98, 0x9f, 0xa0, 0xa1, 0xb8, 0xbf, 0xc2,
          0xd8, 0xd9, 0x79, 0x7a, 0x66, 0x5, 0xe0, 0xff,
        ],
      ),
      1,
      12,
    ),
    list_to_bytes,
  )
}

test prop_agrees_on_random_header_bytes(bytes via header_bytes()) {
  if is_fuzzable(bytes) {
    agrees(bytes)
  } else {
    True
  }
}

fn list_to_bytes(xs: List<Int>) -> ByteArray {
  when xs is {
    [] -> #""
    [x, ..rest] -> bytearray.push(list_to_bytes(rest), x)
  }
}

// ## Fixed cases, including the library's quirks.

test agrees_on_the_empty_input() {
  and {
    agrees(#""),
    cbor_deserialise.deserialise(#"") == None,
  }
}

test agrees_on_an_empty_byte_string_at_the_end() {
  and {
    agrees(#"40"),
    cbor_deserialise.deserialise(#"40") == None,
    agrees(#"8140"),
    agrees(#"824000"),
  }
}

test agrees_on_wide_integer_arguments() {
  and {
    agrees(#"1903e8"),
    agrees(#"1a000f4240"),
    agrees(#"1b000000e8d4a51000"),
    agrees(#"3903e7"),
    agrees(#"1903"),
    agrees(#"19"),
    agrees(#"1c"),
    agrees(#"1f"),
  }
}

test agrees_on_tags() {
  and {
    agrees(#"d87980"),
    agrees(#"d8799f01ff"),
    agrees(#"d87a8101"),
    agrees(#"d9050080"),
    agrees(#"d9057f80"),
    agrees(#"d86680"),
    agrees(#"d866"),
    agrees(#"d87901"),
    agrees(#"d879a0"),
    agrees(#"d87998"),
    agrees(cbor.serialise(builtin.constr_data(128, []))),
    cbor_deserialise.deserialise(cbor.serialise(builtin.constr_data(128, []))) == None,
  }
}

test agrees_on_lists_and_maps() {
  and {
    agrees(#"80"),
    agrees(#"9fff"),
    agrees(#"9f"),
    agrees(#"830102"),
    agrees(#"a0"),
    agrees(#"a10102"),
    agrees(#"bf0102ff"),
    agrees(#"bf01ff"),
    agrees(#"a101"),
  }
}

test agrees_on_indefinite_byte_strings() {
  and {
    agrees(#"5f4101ff"),
    agrees(#"5f41014102ff"),
    agrees(#"5fff"),
    agrees(#"5f"),
    agrees(#"5f41"),
    agrees(#"5fe0"),
  }
}

test agrees_on_rejected_major_types() {
  and {
    agrees(#"60"),
    agrees(#"780161"),
    agrees(#"f6"),
    agrees(#"e0"),
    agrees(#"fb3ff0000000000000"),
  }
}

test agrees_on_empty_byte_strings_before_the_end() {
  agrees(#"82405f40ff")
}

test agrees_on_a_large_serialised_value() {
  let data: Data = builtin.list_data(
    [
      builtin.b_data(
        #"00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff0011",
      ),
      builtin.i_data(18446744073709551615),
      builtin.i_data(-18446744073709551616),
      builtin.constr_data(7, []),
      builtin.constr_data(127, [builtin.i_data(1)]),
      builtin.map_data([Pair(builtin.i_data(1), builtin.b_data(#""))]),
    ],
  )
  let bytes = cbor.serialise(data)
  and {
    agrees(bytes),
    cbor_deserialise.deserialise(bytes) == Some(data),
  }
}

// A chunk header below 0x40 moves the position back before the start of the
// input; the library then fails the script on its next read, and so does this
// module.
test library_fails_the_script_on_a_backward_chunk() fail {
  cbor.deserialise(#"5f0001") == None
}

test deserialise_fails_the_script_on_a_backward_chunk() fail {
  cbor_deserialise.deserialise(#"5f0001") == None
}

// A refused input that declares a long list or map: the library steps through
// every declared element after its first failed read, so on chain it can run
// out of budget; `deserialise` returns `None` at the first failed read, and
// `expect_deserialise` fails the script there.
test deserialise_refuses_a_long_declared_list() {
  cbor_deserialise.deserialise(#"990fff") == None
}

test deserialise_refuses_a_long_declared_map() {
  cbor_deserialise.deserialise(#"b9ffff") == None
}

// ## The failing variant fails where the library returns `None`.

fn truncations() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple(fuzz.data(), fuzz.int_between(0, 4096)),
    fn((data, cut)) {
      let bytes = cbor.serialise(data)
      bytearray.take(bytes, cut % bytearray.length(bytes))
    },
  )
}

fn with_trailing_bytes() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple(fuzz.data(), fuzz.bytearray_between(1, 4)),
    fn((data, tail)) { bytearray.concat(cbor.serialise(data), tail) },
  )
}

fn with_one_replaced_byte() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple3(fuzz.data(), fuzz.int_between(0, 4096), fuzz.byte()),
    fn((data, at, byte)) {
      let bytes = cbor.serialise(data)
      let at = at % bytearray.length(bytes)
      bytearray.concat(
        bytearray.take(bytes, at),
        bytearray.push(bytearray.drop(bytes, at + 1), byte),
      )
    },
  )
}

test prop_expect_fails_on_refused_truncations(bytes via truncations()) fail {
  expect_fails_where_refused(bytes)
}

test prop_expect_fails_on_refused_trailing_bytes(
  bytes via with_trailing_bytes(),
) fail {
  expect_fails_where_refused(bytes)
}

test prop_expect_fails_on_refused_replaced_bytes(
  bytes via with_one_replaced_byte(),
) fail {
  expect_fails_where_refused(bytes)
}

test prop_expect_fails_on_refused_random_bytes(
  bytes via fuzz.bytearray_between(1, 8),
) fail {
  expect_fails_where_refused(bytes)
}

test prop_expect_fails_on_refused_header_bytes(bytes via header_bytes()) fail {
  expect_fails_where_refused(bytes)
}

test expect_fails_on_the_empty_input() fail {
  expect_returns(#"")
}

test expect_fails_on_an_empty_byte_string_at_the_end() fail {
  expect_returns(#"8140")
}

test expect_fails_on_a_wide_empty_byte_string_at_the_end() fail {
  expect_returns(#"815800")
}

test expect_fails_on_a_backward_chunk_at_the_end() fail {
  expect_returns(#"5f3f")
}

test expect_fails_on_a_wide_argument_at_the_end() fail {
  expect_returns(#"8119")
}

test expect_fails_on_a_short_wide_argument() fail {
  expect_returns(#"1a0001")
}

test expect_fails_on_tag_102() fail {
  expect_returns(#"d8668200")
}

test expect_fails_on_an_unread_trailing_byte() fail {
  expect_returns(#"0000")
}

test expect_fails_on_a_text_string() fail {
  expect_returns(#"6161")
}

test expect_fails_on_a_long_declared_list() fail {
  expect_returns(#"990fff")
}

test expect_fails_on_a_long_declared_map() fail {
  expect_returns(#"b9ffff")
}

test expect_fails_on_a_wide_text_string() fail {
  expect_returns(#"780161")
}

test expect_fails_on_a_simple_value() fail {
  expect_returns(#"f6")
}

test expect_fails_on_a_float() fail {
  expect_returns(#"fb3ff0000000000000")
}

test expect_decodes_empty_byte_strings_before_the_end() {
  let data <- cbor_deserialise.expect_deserialise(#"82405f40ff")
  data == builtin.list_data([builtin.b_data(#""), builtin.b_data(#"")])
}

// The decoding happens where it is bound, so a refused input fails the script
// even on a path that never uses the decoded value.
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
