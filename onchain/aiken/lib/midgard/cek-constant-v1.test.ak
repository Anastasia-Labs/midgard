use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
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
