use aiken/builtin
use aiken/cbor
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
    #"39286d9f63929ce56cdd18c4b40524a0c1ecf7909825a3ed3b60f3afcd299d3a",
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
    #"83ee73cbaad1ceaad96adde815cb0cd758faa8846e9cf06ab966889df1750a87",
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
    #"83ee73cbaad1ceaad96adde815cb0cd758faa8846e9cf06ab966889df1750a87",
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
    cek_constant_v1.integer_memory_size_v1(-129),
    cek_constant_v1.integer_memory_size_v1(-128),
    cek_constant_v1.integer_memory_size_v1(-1),
    cek_constant_v1.integer_memory_size_v1(0),
    cek_constant_v1.integer_memory_size_v1(127),
    cek_constant_v1.integer_memory_size_v1(128),
  ] == [2, 1, 1, 1, 1, 2]
}

test semantic_list_memory_sums_elements() {
  cek_constant_v1.constant_memory_size_v1(
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f0500ff",
      payload_cbor: #"9f01190100ff",
    },
  ) == 3
}

test data_memory_adds_four_words_per_node() {
  cek_constant_v1.data_memory_size_v1(
    builtin.map_data(
      [Pair(builtin.i_data(1), builtin.list_data([builtin.b_data(#"0000")]))],
    ),
  ) == 19
}
