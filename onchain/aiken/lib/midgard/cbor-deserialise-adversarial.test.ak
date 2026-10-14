//// Adversarial differential cases for `midgard/cbor_deserialise` against the
//// standard library's `aiken/cbor.deserialise`: exhaustive one-, two- and
//// three-byte inputs, fixed vectors at every header boundary, and the failing
//// variant on refused inputs it reaches by a different path.
////
//// Domains the Aiken test runner cannot evaluate, and so are argued by code
//// reading rather than tested here:
//// - a constructor index below 0 (tags 0..120): the runner panics in
////   `constr_data` (TryFromBigIntError), where the Haskell evaluator does not;
//// - a read before the start of the input inside an aggregated loop: the
////   library fails the script, which would abort the whole loop test.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/fuzz
use midgard/cbor_deserialise

// ## Shared helpers

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

fn expect_returns(bytes: ByteArray) -> Bool {
  let data <- cbor_deserialise.expect_deserialise(bytes)
  builtin.serialise_data(data) != #""
}

fn b1(a: Int) -> ByteArray {
  builtin.cons_bytearray(a, #"")
}

fn b2(a: Int, b: Int) -> ByteArray {
  builtin.cons_bytearray(a, builtin.cons_bytearray(b, #""))
}

fn b3(a: Int, b: Int, c: Int) -> ByteArray {
  builtin.cons_bytearray(
    a,
    builtin.cons_bytearray(b, builtin.cons_bytearray(c, #"")),
  )
}

fn all_between(from: Int, to: Int, predicate: fn(Int) -> Bool) -> Bool {
  if from > to {
    True
  } else if predicate(from) {
    all_between(from + 1, to, predicate)
  } else {
    trace @"disagreement at": from
    False
  }
}

// ## Exhaustive: every 1-byte input

test exhaustive_every_one_byte_input() {
  all_between(0, 255, fn(a) { agrees(b1(a)) })
}

// ## Exhaustive: every 2-byte input
//
// Only a first byte in 0xc0..0xd7 is skipped: it is a tag 0..23, whose
// constructor index is negative, and the runner panics building it. Neither
// decoder fails the script on any other 2-byte input (a backward chunk cannot
// move before the start within two bytes).

fn two_byte_block(hi_from: Int, hi_to: Int) -> Bool {
  all_between(
    hi_from,
    hi_to,
    fn(a) {
      if a >= 0xc0 && a <= 0xd7 {
        True
      } else {
        all_between(
          0,
          255,
          fn(b) {
            let ok = agrees(b2(a, b))
            if ok {
              True
            } else {
              trace @"two-byte first byte": a
              False
            }
          },
        )
      }
    },
  )
}

test exhaustive_two_bytes_00_1f() {
  two_byte_block(0x0, 0x1f)
}

test exhaustive_two_bytes_20_3f() {
  two_byte_block(0x20, 0x3f)
}

test exhaustive_two_bytes_40_5f() {
  two_byte_block(0x40, 0x5f)
}

test exhaustive_two_bytes_60_7f() {
  two_byte_block(0x60, 0x7f)
}

test exhaustive_two_bytes_80_9f() {
  two_byte_block(0x80, 0x9f)
}

test exhaustive_two_bytes_a0_bf() {
  two_byte_block(0xa0, 0xbf)
}

test exhaustive_two_bytes_c0_df() {
  two_byte_block(0xc0, 0xdf)
}

test exhaustive_two_bytes_e0_ff() {
  two_byte_block(0xe0, 0xff)
}

// ## Exhaustive: every 3-byte input over a structural alphabet
//
// The alphabet holds every header boundary: small and wide unsigned and
// negative ints and the illegal 28..31 arguments, byte strings of width 0, 1,
// 2 and 1/2/8-byte widths, the illegal byte-string argument, the
// indefinite-bytes, -list and -map heads, text, tags 121, 122, 127 and 102
// (as data bytes), 1/2/8-byte tag heads, major type 7 and break, and chunk
// headers 0x3e/0x3f that move the position back.
//
// Skipped, each for a stated reason:
// - `5f` followed by a byte below 0x40: the chunk moves the position back and
//   the library either fails the script (before the start) or loops forever
//   (`5f 3f`), which a loop test cannot contain;
// - `d8 b` with b < 121 and b != 102: a negative constructor index panics the
//   runner;
// - a first byte 0x99 or 0x9b: a declared length up to 65,535 that the
//   library walks after its failure, too slow for an exhaustive loop.
fn alphabet() -> List<Int> {
  [
    0x0, 0x1, 0x17, 0x18, 0x19, 0x1b, 0x1c, 0x20, 0x38, 0x3b, 0x3e, 0x3f, 0x40,
    0x41, 0x42, 0x58, 0x59, 0x5b, 0x5c, 0x5f, 0x60, 0x66, 0x79, 0x7a, 0x7f, 0x80,
    0x81, 0x82, 0x98, 0x99, 0x9b, 0x9f, 0xa0, 0xa1, 0xb8, 0xbf, 0xd8, 0xd9, 0xdb,
    0xe0, 0xff,
  ]
}

fn three_byte_skipped(a: Int, b: Int) -> Bool {
  or {
    a == 0x5f && b < 0x40,
    a == 0xd8 && b < 121 && b != 102,
    a == 0x99,
    a == 0x9b,
  }
}

fn three_byte_block(firsts: List<Int>) -> Bool {
  list.all(
    firsts,
    fn(a) {
      list.all(
        alphabet(),
        fn(b) {
          if three_byte_skipped(a, b) {
            True
          } else {
            list.all(
              alphabet(),
              fn(c) {
                let ok = agrees(b3(a, b, c))
                if ok {
                  True
                } else {
                  trace @"three-byte": [a, b, c]
                  False
                }
              },
            )
          }
        },
      )
    },
  )
}

test exhaustive_three_bytes_part_1() {
  three_byte_block([0x0, 0x1, 0x17, 0x18, 0x19, 0x1b, 0x1c])
}

test exhaustive_three_bytes_part_2() {
  three_byte_block([0x20, 0x38, 0x3b, 0x3e, 0x3f, 0x40, 0x41])
}

test exhaustive_three_bytes_part_3() {
  three_byte_block([0x42, 0x58, 0x59, 0x5b, 0x5c, 0x5f, 0x60])
}

test exhaustive_three_bytes_part_4() {
  three_byte_block([0x66, 0x79, 0x7a, 0x7f, 0x80, 0x81, 0x82])
}

test exhaustive_three_bytes_part_5() {
  three_byte_block([0x98, 0x9f, 0xa0, 0xa1, 0xb8, 0xbf])
}

test exhaustive_three_bytes_part_6() {
  three_byte_block([0xd8, 0xd9, 0xdb, 0xe0, 0xff])
}

// ## Fixed vectors beyond three bytes

test agrees_on_wide_ints_and_negatives() {
  and {
    agrees(#"1bffffffffffffffff"),
    agrees(#"3bffffffffffffffff"),
    agrees(#"3affffffff"),
    agrees(#"39ffff"),
    agrees(#"3818"),
    agrees(#"37"),
    agrees(#"1800"),
    agrees(#"190000"),
    agrees(#"1a00000000"),
    agrees(#"1b0000000000000000"),
    agrees(#"1b00000000000000"),
    agrees(#"3c"),
    agrees(#"3d00"),
    agrees(#"3f"),
    agrees(#"1d00"),
    agrees(#"1e0000"),
    cbor_deserialise.deserialise(#"3bffffffffffffffff") == Some(
      builtin.i_data(-18446744073709551616),
    ),
  }
}

test agrees_on_wide_byte_strings() {
  and {
    agrees(#"5801ab"),
    agrees(#"590001ab"),
    agrees(#"5a00000001ab"),
    agrees(#"5b0000000000000001ab"),
    agrees(#"5b0000000000000000"),
    agrees(#"5b000000000000000000"),
    agrees(#"5a0000000000"),
    agrees(#"5800"),
    agrees(#"580000"),
    agrees(#"5801"),
    agrees(#"5902ab"),
    agrees(#"5c00"),
    agrees(#"5d"),
    agrees(#"5e00"),
    agrees(#"9f5800ff"),
    agrees(#"9f590000ff"),
    agrees(#"825800405800"),
  }
}

test agrees_on_indefinite_byte_string_chunks() {
  and {
    agrees(#"5f5801abff"),
    agrees(#"5f590001abff"),
    agrees(#"5f5a00000001abff"),
    agrees(#"5f5b0000000000000001abff"),
    agrees(#"5f5fffff"),
    agrees(#"5f60ff"),
    agrees(#"5f7fff"),
    agrees(#"5f80ff"),
    agrees(#"5fe0ff"),
    agrees(#"5f5cff"),
    agrees(#"5f40ff"),
    agrees(#"5f4040ff"),
    agrees(#"5f41ab40ff"),
    agrees(#"5fff00"),
    agrees(#"5f41ab"),
    agrees(#"5f41abff00"),
    agrees(#"5f5800ff"),
    agrees(#"5f5801"),
    agrees(#"5f58"),
    agrees(#"5f59"),
    agrees(#"5f5900"),
    agrees(#"9f5f41abffff"),
    agrees(#"825f41abff5fff"),
    agrees(#"bf5f41abff5fffff"),
  }
}

// A negative chunk width moves the position back by up to 64 bytes. Landing
// on a byte that was first consumed as chunk data re-reads it as structure.
// Here the jump lands on an `ff` inside the first chunk's data, which ends the
// byte string, and the remaining chunk data and the chunk header itself are
// then read as list items: [h'ff00000000000000', 0, 0, 0, 0, 0, 0, 0, -24].
test agrees_on_a_backward_chunk_that_rereads_data_as_structure() {
  let bytes = #"9f5f48ff000000000000003737ff"
  let fixed = #"9f5f48ff0000000000000037ff"
  and {
    agrees(bytes),
    agrees(fixed),
    cbor_deserialise.deserialise(fixed) == Some(
      builtin.list_data(
        [
          builtin.b_data(#"ff00000000000000"),
          builtin.i_data(0),
          builtin.i_data(0),
          builtin.i_data(0),
          builtin.i_data(0),
          builtin.i_data(0),
          builtin.i_data(0),
          builtin.i_data(0),
          builtin.i_data(-24),
        ],
      ),
    ),
    agrees(#"825f48ff00000000000000370000"),
    agrees(#"815f48ff0000000000000037"),
    agrees(#"5f48ff0000000000000037"),
  }
}

test expect_decodes_a_backward_chunk_that_rereads_data_as_structure() {
  let data <- cbor_deserialise.expect_deserialise(#"9f5f48ff0000000000000037ff")
  data == builtin.list_data(
    [
      builtin.b_data(#"ff00000000000000"),
      builtin.i_data(0),
      builtin.i_data(0),
      builtin.i_data(0),
      builtin.i_data(0),
      builtin.i_data(0),
      builtin.i_data(0),
      builtin.i_data(0),
      builtin.i_data(-24),
    ],
  )
}

// A backward jump onto the `5f` head itself re-reads it as a chunk header
// (31: refused), and a jump onto the chunk header itself re-reads it forever.
test agrees_on_a_backward_chunk_onto_the_head() {
  and {
    agrees(#"5f3e00"),
    agrees(#"815f3e00"),
    agrees(#"5f403d00"),
  }
}

test agrees_on_tags() {
  and {
    agrees(#"d87f80"),
    agrees(#"d9050080"),
    agrees(#"d9057880"),
    agrees(#"d9057980"),
    agrees(#"d904ff80"),
    agrees(#"d9007980"),
    agrees(#"da0000007980"),
    agrees(#"db000000000000007980"),
    agrees(#"d8798080"),
    agrees(#"d8809f01ff"),
    agrees(#"d8799fff"),
    agrees(#"d8799f01"),
    agrees(#"d8799f"),
    agrees(#"d879980100"),
    agrees(#"d87999000100"),
    agrees(#"d8799a0000000100"),
    agrees(#"d8799b000000000000000100"),
    agrees(#"d87900"),
    agrees(#"d8797f"),
    agrees(#"d87940"),
    agrees(#"d87960"),
    agrees(#"d879a0"),
    agrees(#"d8799c"),
    agrees(#"d879ff"),
    agrees(#"d879e0"),
    agrees(#"d87981d87a80"),
    agrees(#"d8668200"),
    agrees(#"d866"),
    agrees(#"d9006680"),
    agrees(#"da0000006680"),
    agrees(#"d86680"),
    agrees(#"dc00"),
    agrees(#"dd"),
    agrees(#"df80"),
    agrees(#"d879"),
    agrees(#"d9"),
    agrees(#"d905"),
    cbor_deserialise.deserialise(#"d87900") == Some(builtin.constr_data(0, [])),
    cbor_deserialise.deserialise(#"d9057980") == Some(
      builtin.constr_data(128, []),
    ),
  }
}

test agrees_on_lists_and_maps() {
  and {
    agrees(#"9f9fffff"),
    agrees(#"9fffff"),
    agrees(#"81ff"),
    agrees(#"8201"),
    agrees(#"980100"),
    agrees(#"99000100"),
    agrees(#"9a0000000100"),
    agrees(#"9b000000000000000100"),
    agrees(#"9a00000000"),
    agrees(#"9b0000000000000000"),
    agrees(#"bf0102ff"),
    agrees(#"bf01ff"),
    agrees(#"bfff"),
    agrees(#"bfffff"),
    agrees(#"b8010102"),
    agrees(#"a201020103"),
    agrees(#"a2010201"),
    agrees(#"9f40ff"),
    agrees(#"8140"),
    agrees(#"8240"),
    agrees(#"81f6"),
    agrees(#"9ff6ff"),
    agrees(#"ff"),
    agrees(#"9f"),
    agrees(#"9f01"),
    agrees(#"bf01"),
    agrees(#"bf0102"),
    agrees(#"9c"),
    agrees(#"bc"),
    agrees(#"9d00"),
  }
}

// ## The failing variant on refused inputs whose refusal it reaches by a
// different path than the library (no end-of-input check before the read).

test expect_fails_on_a_wide_byte_string_running_past_the_end() fail {
  expect_returns(#"5801")
}

test expect_fails_on_a_wide_byte_string_head_at_the_end() fail {
  expect_returns(#"58")
}

test expect_fails_on_a_two_byte_width_running_past_the_end() fail {
  expect_returns(#"5902ab")
}

test expect_fails_on_an_eight_byte_width_short() fail {
  expect_returns(#"5b00000000")
}

test expect_fails_on_an_eight_byte_zero_width_at_the_end() fail {
  expect_returns(#"5b0000000000000000")
}

test expect_fails_on_an_empty_list_with_a_short_wide_length() fail {
  expect_returns(#"9a0000")
}

test expect_fails_on_an_empty_list_with_a_wide_length_at_the_end() fail {
  expect_returns(#"8199")
}

test expect_fails_on_a_tag_at_the_end() fail {
  expect_returns(#"d879")
}

test expect_fails_on_a_wide_tag_at_the_end() fail {
  expect_returns(#"81d9")
}

test expect_fails_on_a_two_byte_tag_short() fail {
  expect_returns(#"d90580")
}

test expect_fails_on_a_tag_102_empty_constr() fail {
  expect_returns(#"d86680")
}

test expect_fails_on_a_wide_tag_102() fail {
  expect_returns(#"d9006680")
}

test expect_fails_on_a_chunk_running_past_the_end() fail {
  expect_returns(#"5f41")
}

test expect_fails_on_a_wide_chunk_at_the_end() fail {
  expect_returns(#"5f5801")
}

test expect_fails_on_an_empty_chunk_at_the_end() fail {
  expect_returns(#"5f40")
}

test expect_fails_on_a_backward_chunk_onto_the_head() fail {
  expect_returns(#"5f3e00")
}

test expect_fails_on_a_backward_chunk_before_the_start() fail {
  expect_returns(#"5f3b00")
}

test expect_fails_on_an_unterminated_indefinite_map() fail {
  expect_returns(#"bf0102")
}

test expect_fails_on_an_odd_indefinite_map() fail {
  expect_returns(#"bf01ff")
}

test expect_fails_on_an_illegal_constr_field_count() fail {
  expect_returns(#"d8799c")
}

test expect_fails_on_a_break_as_an_item() fail {
  expect_returns(#"81ff")
}

test expect_fails_on_a_backward_chunk_with_trailing_bytes() fail {
  expect_returns(#"9f5f48ff0000000000000037ff00")
}

// ## The failing variant over refused 1..3-byte structural inputs.
//
// A `fail` property passes only when every sample fails. Inputs outside the
// domain, or accepted by the library, fail deliberately.

fn structural_bytes() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple4(
      fuzz.int_between(1, 3),
      fuzz.one_of(alphabet()),
      fuzz.one_of(alphabet()),
      fuzz.one_of(alphabet()),
    ),
    fn((n, a, b, c)) {
      if n == 1 {
        b1(a)
      } else if n == 2 {
        b2(a, b)
      } else {
        b3(a, b, c)
      }
    },
  )
}

fn in_strict_domain(bytes: ByteArray) -> Bool {
  let n = builtin.length_of_bytearray(bytes)
  if n < 2 {
    True
  } else {
    let a = builtin.index_bytearray(bytes, 0)
    let b = builtin.index_bytearray(bytes, 1)
    if n == 2 {
      True
    } else {
      !three_byte_skipped(a, b)
    }
  }
}

test prop_expect_fails_on_refused_structural_bytes(
  bytes via structural_bytes(),
) fail {
  if in_strict_domain(bytes) && cbor.deserialise(bytes) == None {
    expect_returns(bytes)
  } else {
    fail
  }
}

fn any_two_bytes() -> Fuzzer<ByteArray> {
  fuzz.map(
    fuzz.tuple(fuzz.int_between(0, 255), fuzz.int_between(0, 255)),
    fn((a, b)) { b2(a, b) },
  )
}

test prop_expect_fails_on_refused_two_byte_inputs(
  bytes via any_two_bytes(),
) fail {
  let a = builtin.index_bytearray(bytes, 0)
  if a >= 0xc0 && a <= 0xd7 {
    fail
  } else if cbor.deserialise(bytes) == None {
    expect_returns(bytes)
  } else {
    fail
  }
}

// ## Cost of a refused input with a large declared length.
//
// After its first failed read the library still steps through every
// remaining element of a definite list or map, so its cost grows with the
// declared length, not with the input. `deserialise` returns `None` at the
// first failed read. Compare the two pairs of readings: the library's grows
// about 16x from 4,095 to 65,535 declared items; the module's does not move.
test cost_library_refuses_declared_4095() {
  cbor.deserialise(#"990fff") == None
}

test cost_library_refuses_declared_65535() {
  cbor.deserialise(#"99ffff") == None
}

test cost_module_refuses_declared_4095() {
  cbor_deserialise.deserialise(#"990fff") == None
}

test cost_module_refuses_declared_65535() {
  cbor_deserialise.deserialise(#"99ffff") == None
}

test cost_library_refuses_declared_65535_map() {
  cbor.deserialise(#"b9ffff") == None
}

test cost_module_refuses_declared_4294967295() {
  cbor_deserialise.deserialise(#"9affffffff") == None
}
