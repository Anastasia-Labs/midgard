//// Differential tests: `cbor_deserialise.deserialise` must return exactly what
//// the standard library's `aiken/cbor.deserialise` returns.

use aiken/builtin
use aiken/cbor
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/cbor_deserialise

fn agrees(bytes: ByteArray) -> Bool {
  cbor_deserialise.deserialise(bytes) == cbor.deserialise(bytes)
}

// Random inputs avoid three byte patterns, each covered by fixed cases below
// instead:
// - 0x5f: a chunk header inside an indefinite byte string is read as
//   `byte - 64`, so a byte below 0x40 there moves the position back, and both
//   decoders then fail the script on a read before the start of the input.
// - a tag below 121 or with an 8-byte argument: the constructor index is then
//   negative or too wide for the test runner's evaluator.
// - a byte-string head with an 8-byte width: it overflows the test runner's
//   slice.
// Both decoders pass the same values to the same builtins, so none of the
// three can tell them apart.
fn is_fuzzable(bytes: ByteArray) -> Bool {
  is_fuzzable_from(bytes, 0, bytearray.length(bytes))
}

fn is_fuzzable_from(bytes: ByteArray, at: Int, length: Int) -> Bool {
  if at >= length {
    True
  } else {
    let byte = builtin.index_bytearray(bytes, at)
    let small_tag = if byte == 0xd8 {
      at + 1 < length && builtin.index_bytearray(bytes, at + 1) < 121
    } else if byte == 0xd9 {
      at + 2 < length && builtin.bytearray_to_integer(
        True,
        builtin.slice_bytearray(at + 1, 2, bytes),
      ) < 121
    } else {
      byte >= 0xc0 && byte < 0xd8 || byte == 0xda || byte == 0xdb
    }
    and {
      byte != 0x5f,
      !small_tag,
      byte != 0x5b,
      byte != 0x7b,
      is_fuzzable_from(bytes, at + 1, length),
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
    agrees(#"f6"),
    agrees(#"e0"),
  }
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
