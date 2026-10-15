use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/math
use aiken/primitive/bytearray
use midgard/cbor_deserialise
use midgard/cek_constant_v1

test canonical_integer_constant_matches_typescript() {
  cek_constant_v1.verify_constant_witness_v1(
    #"35930512fad9db8f38585195d5363af8826bb2b002028fe85af5874a85ab305c",
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f00ff",
      payload_cbor: #"1829",
    },
  )
}

test canonical_boolean_constant_matches_typescript() {
  cek_constant_v1.verify_constant_witness_v1(
    #"b9d299a51cb8e6181262d67d07662451930d812d4d79348a86d64361f93a889d",
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f04ff",
      payload_cbor: #"d87a80",
    },
  )
}

test canonical_list_constant_matches_typescript() {
  cek_constant_v1.verify_constant_witness_v1(
    #"455063152b571b24bc7ba2e98d14120484eea1fe9a4b7210b639fb413f27f0ba",
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f0500ff",
      payload_cbor: #"9f0102ff",
    },
  )
}

test canonical_direct_4095_byte_constant_matches_typescript() {
  let bytes = bytearray.from_int_big_endian(0, 4095)
  cek_constant_v1.verify_constant_witness_v1(
    #"f78cf598a2aed2565c1a92207133ec48937408d549b22f702e2803c1017d683e",
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f01ff",
      payload_cbor: cbor.serialise(builtin.b_data(bytes)),
    },
  )
}

test canonical_direct_8800_byte_constant_matches_streamed_root() {
  let bytes = bytearray.from_int_big_endian(0, 4400)
    |> bytearray.concat(bytearray.from_int_big_endian(0, 4400))
  cek_constant_v1.verify_constant_witness_v1(
    #"3e10cc18dcf520d7b952abdd0cd502bf1c7513c34c4fb810c700ea016194fb78",
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f01ff",
      payload_cbor: cbor.serialise(builtin.b_data(bytes)),
    },
  )
}

test direct_payload_above_9215_bytes_fails_closed() fail {
  let bytes = bytearray.from_int_big_endian(0, 4608)
    |> bytearray.concat(bytearray.from_int_big_endian(0, 4608))
  let payload_cbor = cbor.serialise(builtin.b_data(bytes))
  expect bytearray.length(payload_cbor) > 9215
  cek_constant_v1.verify_constant_witness_v1(
    #"3e10cc18dcf520d7b952abdd0cd502bf1c7513c34c4fb810c700ea016194fb78",
    cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f01ff", payload_cbor },
  )
}

test noncanonical_payload_fails_closed() fail {
  cek_constant_v1.verify_constant_witness_v1(
    #"35930512fad9db8f38585195d5363af8826bb2b002028fe85af5874a85ab305c",
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f00ff",
      payload_cbor: #"190017",
    },
  )
}

// The canonical encoding of the empty byte string is its one-byte head.
test empty_byte_string_payload_decodes() {
  and {
    cek_constant_v1.decode_constant_payload_v1(#"40") == builtin.b_data(#""),
    cek_constant_v1.constant_payload_v1(
      cek_constant_v1.ConstantWitnessV1 {
        type_cbor: #"9f01ff",
        payload_cbor: #"40",
      },
    ) == builtin.b_data(#""),
  }
}

// `5800` decodes to the same empty byte string, so only the re-encode check
// refuses it as a payload.
test noncanonical_empty_byte_string_payload_fails_closed() fail {
  expect cbor_deserialise.deserialise(#"5800") == Some(builtin.b_data(#""))
  cek_constant_v1.decode_constant_payload_v1(#"5800") == builtin.b_data(#"")
}

test signed_integer_memory_size_matches_reference_cek() {
  [
    cek_constant_v1.integer_memory_size_v1(-18446744073709551616),
    cek_constant_v1.integer_memory_size_v1(-18446744073709551615),
    cek_constant_v1.integer_memory_size_v1(-1),
    cek_constant_v1.integer_memory_size_v1(0),
    cek_constant_v1.integer_memory_size_v1(18446744073709551615),
    cek_constant_v1.integer_memory_size_v1(18446744073709551616),
  ] == [2, 1, 1, 1, 1, 2]
}

test semantic_list_memory_sums_elements() {
  cek_constant_v1.constant_memory_size_v1(
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f0500ff",
      payload_cbor: #"9f01190100ff",
    },
  ) == 2
}

test data_memory_adds_four_words_per_node() {
  cek_constant_v1.data_memory_size_v1(
    builtin.map_data(
      [Pair(builtin.i_data(1), builtin.list_data([builtin.b_data(#"0000")]))],
    ),
  ) == 18
}

const pv11_value_payload =
  #"a240a1401a000f4240581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa241010542010222"

test canonical_value_constant_matches_typescript() {
  let witness = cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f0dff",
    payload_cbor: pv11_value_payload,
  }
  and {
    cek_constant_v1.verify_constant_witness_v1(
      #"927aa3834dec4fd50eeec6ae8276d3496c323129be9af04ba291809b0631d8bd",
      witness,
    ),
    cek_constant_v1.constant_memory_size_v1(witness) == 3,
    cek_constant_v1.constant_type_v1(witness) == cek_constant_v1.ValueConstant,
    cek_constant_v1.verify_constant_witness_v1(
      #"4a0ee6f6379d9f5a8aa40f228cc79a5594238830dedfb6bfea54973ed52c1eb8",
      cek_constant_v1.ConstantWitnessV1 {
        type_cbor: #"9f0dff",
        payload_cbor: #"a0",
      },
    ),
  }
}

test canonical_array_constant_matches_typescript() {
  let witness = cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f0c00ff",
    payload_cbor: #"9f011903e824ff",
  }
  and {
    cek_constant_v1.verify_constant_witness_v1(
      #"b806d66b21c3df91b6272fd7b367f8d4c07612f6fe73fa08187557b2fbeddcbb",
      witness,
    ),
    cek_constant_v1.constant_memory_size_v1(witness) == 3,
    cek_constant_v1.constant_type_v1(witness) == cek_constant_v1.ArrayConstant {
      element: cek_constant_v1.IntegerConstant,
    },
  }
}

test array_constant_refuses_a_mistyped_element() fail {
  cek_constant_v1.constant_type_v1(
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f0c00ff",
      payload_cbor: #"9f4101ff",
    },
  ) == cek_constant_v1.ArrayConstant {
    element: cek_constant_v1.ByteStringConstant,
  }
}

fn token_map(entries: List<Pair<ByteArray, Int>>) -> Data {
  builtin.map_data(
    list.map(
      entries,
      fn(Pair(name, amount)) {
        Pair(builtin.b_data(name), builtin.i_data(amount))
      },
    ),
  )
}

fn value_map(entries: List<Pair<ByteArray, Data>>) -> Data {
  builtin.map_data(
    list.map(
      entries,
      fn(Pair(policy, tokens)) { Pair(builtin.b_data(policy), tokens) },
    ),
  )
}

const max_quantity = 170141183460469231731687303715884105727

const min_quantity = -170141183460469231731687303715884105728

const long_key =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

test value_data_admits_the_bounds_cardano_admits() {
  and {
    cek_constant_v1.value_data_is_canonical_v1(value_map([])),
    cek_constant_v1.value_data_is_canonical_v1(
      value_map([Pair(long_key, token_map([Pair(long_key, 1)]))]),
    ),
    cek_constant_v1.value_data_is_canonical_v1(
      value_map(
        [
          Pair(
            #"01",
            token_map([Pair(#"", max_quantity), Pair(#"00", min_quantity)]),
          ),
        ],
      ),
    ),
    // A proper prefix sorts first, as in Haskell's ByteString order.
    cek_constant_v1.value_data_is_canonical_v1(
      value_map(
        [
          Pair(#"01", token_map([Pair(#"", 1)])),
          Pair(#"0100", token_map([Pair(#"ff", 1), Pair(#"ff00", 1)])),
        ],
      ),
    ),
  }
}

test value_data_refuses_every_noncanonical_form() {
  let one = token_map([Pair(#"", 1)])
  let refused = [
    value_map([Pair(#"aa" |> bytearray.concat(long_key), one)]),
    value_map(
      [Pair(#"01", token_map([Pair(#"aa" |> bytearray.concat(long_key), 1)]))],
    ),
    value_map([Pair(#"01", token_map([Pair(#"", max_quantity + 1)]))]),
    value_map([Pair(#"01", token_map([Pair(#"", min_quantity - 1)]))]),
    value_map([Pair(#"01", token_map([Pair(#"", 0)]))]),
    value_map([Pair(#"01", token_map([]))]),
    value_map([Pair(#"02", one), Pair(#"01", one)]),
    value_map([Pair(#"01", one), Pair(#"01", token_map([Pair(#"01", 1)]))]),
    value_map([Pair(#"0100", one), Pair(#"01", one)]),
    value_map([Pair(#"01", token_map([Pair(#"02", 1), Pair(#"01", 1)]))]),
    value_map([Pair(#"01", token_map([Pair(#"01", 1), Pair(#"01", 2)]))]),
    builtin.list_data([]),
    builtin.map_data([Pair(builtin.i_data(1), one)]),
    value_map([Pair(#"01", builtin.list_data([]))]),
    value_map(
      [
        Pair(
          #"01",
          builtin.map_data([Pair(builtin.b_data(#""), builtin.b_data(#"01"))]),
        ),
      ],
    ),
  ]
  list.all(
    refused,
    fn(payload) { !cek_constant_v1.value_data_is_canonical_v1(payload) },
  )
}

test value_constant_refuses_a_noncanonical_payload() fail {
  cek_constant_v1.constant_type_v1(
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f0dff",
      payload_cbor: cbor.serialise(
        value_map([Pair(#"01", token_map([Pair(#"", 0)]))]),
      ),
    },
  ) == cek_constant_v1.ValueConstant
}

test constant_type_flat_size_counts_type_application_tags() {
  and {
    cek_constant_v1.constant_type_flat_size_v1(cek_constant_v1.IntegerConstant) == 1,
    cek_constant_v1.constant_type_flat_size_v1(cek_constant_v1.ValueConstant) == 1,
    cek_constant_v1.constant_type_flat_size_v1(
      cek_constant_v1.ListConstant { element: cek_constant_v1.IntegerConstant },
    ) == 3,
    cek_constant_v1.constant_type_flat_size_v1(
      cek_constant_v1.ArrayConstant { element: cek_constant_v1.IntegerConstant },
    ) == 3,
    cek_constant_v1.constant_type_flat_size_v1(
      cek_constant_v1.PairConstant {
        first: cek_constant_v1.IntegerConstant,
        second: cek_constant_v1.DataConstant,
      },
    ) == 5,
  }
}

test value_and_array_type_tags_decode_and_tag_14_does_not() fail {
  cek_constant_v1.constant_type_is_known_v1(#"9f0eff")
}

const g1_point =
  builtin.b_data(
    #"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
  )

fn pair_of(first: Data, second: Data) -> Data {
  builtin.constr_data(0, [first, second])
}

test constant_type_flat_size_bound_is_32() {
  let list_of = fn(element) { cek_constant_v1.ListConstant { element } }
  let deep = list.foldl(
    list.range(1, 15),
    cek_constant_v1.IntegerConstant,
    fn(_, element) { list_of(element) },
  )
  and {
    cek_constant_v1.max_constant_type_flat_size == 32,
    cek_constant_v1.constant_type_flat_size_v1(deep) == 31,
    cek_constant_v1.constant_type_flat_size_v1(list_of(deep)) == 33,
  }
}

test bls_values_are_found_at_any_nesting() {
  let g1 = cek_constant_v1.BlsG1Constant
  let int = cek_constant_v1.IntegerConstant
  let one = builtin.i_data(1)
  let empty = builtin.list_data([])
  and {
    cek_constant_v1.constant_holds_bls_value_v1(g1, g1_point),
    cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.PairConstant { first: int, second: g1 },
      pair_of(one, g1_point),
    ),
    cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ListConstant { element: g1 },
      builtin.list_data([g1_point]),
    ),
    cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ArrayConstant { element: g1 },
      builtin.list_data([g1_point]),
    ),
    cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ListConstant {
        element: cek_constant_v1.ListConstant { element: g1 },
      },
      builtin.list_data([empty, builtin.list_data([g1_point])]),
    ),
    cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ListConstant {
        element: cek_constant_v1.PairConstant { first: g1, second: int },
      },
      builtin.list_data([pair_of(g1_point, one)]),
    ),
  }
}

test empty_bls_lists_hold_no_bls_value() {
  let g1 = cek_constant_v1.BlsG1Constant
  let int = cek_constant_v1.IntegerConstant
  let empty = builtin.list_data([])
  and {
    !cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ListConstant { element: g1 },
      empty,
    ),
    !cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ArrayConstant { element: cek_constant_v1.BlsG2Constant },
      empty,
    ),
    !cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ListConstant {
        element: cek_constant_v1.ListConstant { element: g1 },
      },
      builtin.list_data([empty, empty]),
    ),
    !cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.PairConstant {
        first: cek_constant_v1.ListConstant { element: g1 },
        second: int,
      },
      pair_of(empty, builtin.i_data(1)),
    ),
    !cek_constant_v1.constant_holds_bls_value_v1(
      cek_constant_v1.ListConstant { element: int },
      builtin.list_data([builtin.i_data(1)]),
    ),
  }
}

// Integer constants on both sides of the 64-bit head boundary. The payload is
// the ledger's own serialisation and decodes to the same integer. Values are
// compared as integers: the pinned evaluator's Data equality does not separate
// -2^64 from -2^64 - 1.
fn integer_payload_decodes(value: Int, payload_cbor: ByteArray) -> Bool {
  expect cbor.serialise(builtin.i_data(value)) == payload_cbor
  and {
    builtin.un_i_data(cek_constant_v1.decode_constant_payload_v1(payload_cbor)) == value,
    builtin.un_i_data(
      cek_constant_v1.constant_payload_v1(
        cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f00ff", payload_cbor },
      ),
    ) == value,
  }
}

test integer_constant_with_largest_unsigned_head_decodes() {
  integer_payload_decodes(18446744073709551615, #"1bffffffffffffffff")
}

test integer_constant_with_smallest_positive_bignum_decodes() {
  integer_payload_decodes(18446744073709551616, #"c249010000000000000000")
}

test integer_constant_with_largest_negative_head_decodes() {
  integer_payload_decodes(-18446744073709551616, #"3bffffffffffffffff")
}

test integer_constant_with_smallest_negative_bignum_decodes() {
  integer_payload_decodes(-18446744073709551617, #"c349010000000000000000")
}

// Constructor 127 is the last compact tag (1400); 128 uses tag 102.
fn data_payload_decodes(data: Data, payload_cbor: ByteArray) -> Bool {
  expect cbor.serialise(data) == payload_cbor
  and {
    cek_constant_v1.decode_constant_payload_v1(payload_cbor) == data,
    cek_constant_v1.constant_payload_v1(
      cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f08ff", payload_cbor },
    ) == data,
  }
}

test constructor_127_constant_payload_decodes() {
  and {
    data_payload_decodes(builtin.constr_data(127, []), #"d9057880"),
    data_payload_decodes(
      builtin.constr_data(127, [builtin.i_data(1)]),
      #"d905789f01ff",
    ),
  }
}

test constructor_128_constant_payload_decodes() {
  and {
    data_payload_decodes(builtin.constr_data(128, []), #"d86682188080"),
    data_payload_decodes(
      builtin.constr_data(128, [builtin.i_data(1)]),
      #"d8668218809f01ff",
    ),
  }
}

// Byte strings behind a two-byte tag 2/3 head, and tag 102 with a negative or
// bignum index, are not Plutus Data.
test long_tag_heads_and_signed_constructor_indexes_do_not_decode() {
  and {
    cbor_deserialise.deserialise(#"d80249010000000000000000") == None,
    cbor_deserialise.deserialise(#"d80349010000000000000000") == None,
    cbor_deserialise.deserialise(#"d866822080") == None,
    cbor_deserialise.deserialise(#"d86682c2410180") == None,
  }
}

test long_tag_2_head_payload_fails_closed() fail {
  cek_constant_v1.decode_constant_payload_v1(#"d80249010000000000000000") == builtin.i_data(
    18446744073709551616,
  )
}

test long_tag_3_head_payload_fails_closed() fail {
  cek_constant_v1.decode_constant_payload_v1(#"d80349010000000000000000") == builtin.i_data(
    -18446744073709551617,
  )
}

test constructor_102_with_negative_index_payload_fails_closed() fail {
  cek_constant_v1.decode_constant_payload_v1(#"d866822080") == builtin.constr_data(
    0,
    [],
  )
}

test constructor_102_with_bignum_index_payload_fails_closed() fail {
  cek_constant_v1.decode_constant_payload_v1(#"d86682c2410180") == builtin.constr_data(
    1,
    [],
  )
}

// The direct payload bound (9,215 bytes) admits integers of up to 8,932
// magnitude bytes: a tag 2 or 3 head over an indefinite byte string of 139
// full 64-byte chunks and one 36-byte chunk.
test largest_integers_within_the_direct_payload_bound_decode() {
  let largest = math.pow2(71456) - 1
  let positive = cbor.serialise(builtin.i_data(largest))
  let negative = cbor.serialise(builtin.i_data(-largest - 1))
  and {
    bytearray.length(positive) == 9215,
    bytearray.length(negative) == 9215,
    cek_constant_v1.decode_constant_payload_v1(positive) == builtin.i_data(
      largest,
    ),
    cek_constant_v1.decode_constant_payload_v1(negative) == builtin.i_data(
      -largest - 1,
    ),
  }
}

// One more magnitude byte is still an integer the decoder reads; its payload
// is over the bound, which refuses it before decoding.
test integers_beyond_the_direct_payload_bound_still_decode() {
  let beyond = math.pow2(71456)
  let positive = cbor.serialise(builtin.i_data(beyond))
  let negative = cbor.serialise(builtin.i_data(-beyond - 1))
  and {
    bytearray.length(positive) == 9216,
    bytearray.length(negative) == 9216,
    cbor_deserialise.deserialise(positive) == Some(builtin.i_data(beyond)),
    cbor_deserialise.deserialise(negative) == Some(builtin.i_data(-beyond - 1)),
  }
}

test integer_beyond_the_direct_payload_bound_fails_closed() fail {
  let beyond = math.pow2(71456)
  cek_constant_v1.decode_constant_payload_v1(
    cbor.serialise(builtin.i_data(beyond)),
  ) == builtin.i_data(beyond)
}

// A script's Flat may spell a Data constant in any form the ledger's Data
// decoder accepts; the off-chain builder commits the value through its
// canonical payload. The roots below are the TypeScript builder's roots for
// these Data constants (cek-program.data-constant-spelling.test.ts).
const data_one_root =
  #"e057a14e2548433aa820c2162b36ac98168db657696f4a94fc20b450cc533a1b"

const data_minus_one_root =
  #"0c4d51014e8455817589ef43a949c3502f7acc38c36d8b3c6cdbee77d380ef9e"

const data_constructor_0_root =
  #"2fdea9c423cf4307f98d4a2c6079d1313105e5f7697b297e811d14345cdea3bb"

const data_constructor_7_root =
  #"ca0f5832b69d147c495db22b97da258a94e69268bea11bebb21b068e5e224472"

const data_two_pow_64_root =
  #"f3f49964ea5ee398bd5f7fec6f8b88285310fd4701a2b84593a90ac9824ba3c3"

fn data_witness(payload_cbor: ByteArray) -> cek_constant_v1.ConstantWitnessV1 {
  cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f08ff", payload_cbor }
}

test canonical_data_constant_witnesses_match_typescript() {
  and {
    cek_constant_v1.verify_constant_witness_v1(
      data_one_root,
      data_witness(#"01"),
    ),
    cek_constant_v1.verify_constant_witness_v1(
      data_minus_one_root,
      data_witness(#"20"),
    ),
    cek_constant_v1.verify_constant_witness_v1(
      data_constructor_0_root,
      data_witness(#"d87980"),
    ),
    cek_constant_v1.verify_constant_witness_v1(
      data_constructor_7_root,
      data_witness(#"d9050080"),
    ),
    cek_constant_v1.verify_constant_witness_v1(
      data_two_pow_64_root,
      data_witness(#"c249010000000000000000"),
    ),
  }
}

// Each spelling decodes to the committed value but is not its serialisation,
// so a witness carrying it is refused only by the canonical re-encode check.
fn spelling_of(value: Data, canonical: ByteArray, spelling: ByteArray) -> Bool {
  and {
    cbor_deserialise.deserialise(spelling) == Some(value),
    cbor.serialise(value) == canonical,
    spelling != canonical,
  }
}

test noncanonical_data_spellings_decode_to_the_committed_values() {
  and {
    spelling_of(builtin.i_data(1), #"01", #"c24101"),
    spelling_of(builtin.i_data(1), #"01", #"1801"),
    spelling_of(builtin.i_data(-1), #"20", #"c34100"),
    spelling_of(builtin.constr_data(0, []), #"d87980", #"d866820080"),
    spelling_of(builtin.constr_data(7, []), #"d9050080", #"d866820780"),
    spelling_of(
      builtin.i_data(18446744073709551616),
      #"c249010000000000000000",
      #"c24a00010000000000000000",
    ),
  }
}

test data_witness_with_c2_bignum_spelling_fails_closed() fail {
  cek_constant_v1.verify_constant_witness_v1(
    data_one_root,
    data_witness(#"c24101"),
  )
}

test data_witness_with_nonminimal_head_spelling_fails_closed() fail {
  cek_constant_v1.verify_constant_witness_v1(
    data_one_root,
    data_witness(#"1801"),
  )
}

test data_witness_with_c3_bignum_spelling_fails_closed() fail {
  cek_constant_v1.verify_constant_witness_v1(
    data_minus_one_root,
    data_witness(#"c34100"),
  )
}

test data_witness_with_tag_102_constructor_0_spelling_fails_closed() fail {
  cek_constant_v1.verify_constant_witness_v1(
    data_constructor_0_root,
    data_witness(#"d866820080"),
  )
}

test data_witness_with_tag_102_constructor_7_spelling_fails_closed() fail {
  cek_constant_v1.verify_constant_witness_v1(
    data_constructor_7_root,
    data_witness(#"d866820780"),
  )
}

test data_witness_with_padded_bignum_spelling_fails_closed() fail {
  cek_constant_v1.verify_constant_witness_v1(
    data_two_pow_64_root,
    data_witness(#"c24a00010000000000000000"),
  )
}
