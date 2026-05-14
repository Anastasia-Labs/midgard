use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray

pub const native_tx_version: Int = 1

pub type NativeTxBodyCompact {
  spend_inputs_hash: ByteArray,
  reference_inputs_hash: ByteArray,
  outputs_hash: ByteArray,
  fee: Int,
  validity_interval_start: Int,
  validity_interval_end: Int,
  required_observers_hash: ByteArray,
  required_signers_hash: ByteArray,
  mint_hash: ByteArray,
  script_integrity_hash: ByteArray,
  auxiliary_data_hash: ByteArray,
  network_id: Int,
}

pub type NativeTxCompact {
  body: NativeTxBodyCompact,
  witness_set_hash: ByteArray,
  validity_code: Int,
}

pub type NativeTxCompactView {
  tx_id: ByteArray,
  spend_inputs_hash: ByteArray,
  compact_cbor: ByteArray,
}

fn expect_h32(bytes: ByteArray) -> ByteArray {
  expect bytearray.length(bytes) == 32
  bytes
}

fn expect_non_negative(value: Int) -> Int {
  expect value >= 0
  value
}

fn expect_validity_code(value: Int) -> Int {
  expect _non_negative = expect_non_negative(value)
  expect value <= 5
  value
}

fn expect_network_id(value: Int) -> Int {
  expect _non_negative = expect_non_negative(value)
  expect or {
    value == 0,
    value == 1,
    value == 255,
  }
  value
}

fn encode_definite_array_header(length: Int) -> ByteArray {
  expect length >= 0
  if length <= 23 {
    bytearray.push(#"", 128 + length)
  } else if length <= 255 {
    bytearray.concat(#"98", bytearray.from_int_big_endian(length, 1))
  } else if length <= 65535 {
    bytearray.concat(#"99", bytearray.from_int_big_endian(length, 2))
  } else {
    bytearray.concat(#"9a", bytearray.from_int_big_endian(length, 4))
  }
}

pub fn encode_native_byte_list(items: List<ByteArray>) -> ByteArray {
  bytearray.concat(
    encode_definite_array_header(list.length(items)),
    list.foldr(
      items,
      #"",
      fn(item, encoded_tail) {
        bytearray.concat(cbor.serialise(item), encoded_tail)
      },
    ),
  )
}

pub fn encode_native_tx_body_compact(body: NativeTxBodyCompact) -> ByteArray {
  let NativeTxBodyCompact {
    spend_inputs_hash,
    reference_inputs_hash,
    outputs_hash,
    fee,
    validity_interval_start,
    validity_interval_end,
    required_observers_hash,
    required_signers_hash,
    mint_hash,
    script_integrity_hash,
    auxiliary_data_hash,
    network_id,
  } = body

  expect _spend_inputs_hash = expect_h32(spend_inputs_hash)
  expect _reference_inputs_hash = expect_h32(reference_inputs_hash)
  expect _outputs_hash = expect_h32(outputs_hash)
  expect _fee = expect_non_negative(fee)
  expect _required_observers_hash = expect_h32(required_observers_hash)
  expect _required_signers_hash = expect_h32(required_signers_hash)
  expect _mint_hash = expect_h32(mint_hash)
  expect _script_integrity_hash = expect_h32(script_integrity_hash)
  expect _auxiliary_data_hash = expect_h32(auxiliary_data_hash)
  expect _network_id = expect_network_id(network_id)

  #"8c"
    |> bytearray.concat(cbor.serialise(spend_inputs_hash))
    |> bytearray.concat(cbor.serialise(reference_inputs_hash))
    |> bytearray.concat(cbor.serialise(outputs_hash))
    |> bytearray.concat(cbor.serialise(fee))
    |> bytearray.concat(cbor.serialise(validity_interval_start))
    |> bytearray.concat(cbor.serialise(validity_interval_end))
    |> bytearray.concat(cbor.serialise(required_observers_hash))
    |> bytearray.concat(cbor.serialise(required_signers_hash))
    |> bytearray.concat(cbor.serialise(mint_hash))
    |> bytearray.concat(cbor.serialise(script_integrity_hash))
    |> bytearray.concat(cbor.serialise(auxiliary_data_hash))
    |> bytearray.concat(cbor.serialise(network_id))
}

pub fn encode_native_tx_compact(tx: NativeTxCompact) -> ByteArray {
  let NativeTxCompact { body, witness_set_hash, validity_code } = tx

  expect _witness_set_hash = expect_h32(witness_set_hash)
  expect _validity_code = expect_validity_code(validity_code)

  #"84"
    |> bytearray.concat(cbor.serialise(native_tx_version))
    |> bytearray.concat(encode_native_tx_body_compact(body))
    |> bytearray.concat(cbor.serialise(witness_set_hash))
    |> bytearray.concat(cbor.serialise(validity_code))
}

pub fn verify_native_tx_compact(
  native_tx_id: ByteArray,
  native_tx: NativeTxCompact,
  native_tx_compact_cbor: ByteArray,
) -> NativeTxCompactView {
  expect encode_native_tx_compact(native_tx) == native_tx_compact_cbor

  let NativeTxCompact { body, .. } = native_tx
  let NativeTxBodyCompact { spend_inputs_hash, .. } = body
  let computed_tx_id = encode_native_tx_body_compact(body) |> blake2b_256
  expect computed_tx_id == native_tx_id

  NativeTxCompactView {
    tx_id: native_tx_id,
    spend_inputs_hash,
    compact_cbor: native_tx_compact_cbor,
  }
}

pub fn verify_spend_inputs_preimage(
  spend_input_cbors: List<ByteArray>,
  expected_spend_inputs_hash: ByteArray,
) -> Bool {
  blake2b_256(encode_native_byte_list(spend_input_cbors)) == expected_spend_inputs_hash
}

pub fn spend_input_cbor_at(
  spend_input_cbors: List<ByteArray>,
  input_index: Int,
) -> ByteArray {
  spend_input_cbors |> list.expect_at(input_index)
}

test encode_native_tx_compact_matches_node_cbor_shape() {
  let body = NativeTxBodyCompact {
    spend_inputs_hash: #"0101010101010101010101010101010101010101010101010101010101010101",
    reference_inputs_hash: #"0202020202020202020202020202020202020202020202020202020202020202",
    outputs_hash: #"0303030303030303030303030303030303030303030303030303030303030303",
    fee: 0,
    validity_interval_start: -1,
    validity_interval_end: -1,
    required_observers_hash: #"0404040404040404040404040404040404040404040404040404040404040404",
    required_signers_hash: #"0505050505050505050505050505050505050505050505050505050505050505",
    mint_hash: #"0606060606060606060606060606060606060606060606060606060606060606",
    script_integrity_hash: #"0707070707070707070707070707070707070707070707070707070707070707",
    auxiliary_data_hash: #"0808080808080808080808080808080808080808080808080808080808080808",
    network_id: 0,
  }
  let body_cbor =
    #"8c582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200303030303030303030303030303030303030303030303030303030303030303002020582004040404040404040404040404040404040404040404040404040404040404045820050505050505050505050505050505050505050505050505050505050505050558200606060606060606060606060606060606060606060606060606060606060606582007070707070707070707070707070707070707070707070707070707070707075820080808080808080808080808080808080808080808080808080808080808080800"
  let compact = NativeTxCompact {
    body,
    witness_set_hash: #"0909090909090909090909090909090909090909090909090909090909090909",
    validity_code: 0,
  }
  let compact_cbor =
    #"8401"
      |> bytearray.concat(body_cbor)
      |> bytearray.concat(#"5820090909090909090909090909090909090909090909090909090909090909090900")
  let view =
    verify_native_tx_compact(blake2b_256(body_cbor), compact, compact_cbor)
  and {
    encode_native_tx_body_compact(body) == body_cbor,
    encode_native_tx_compact(compact) == compact_cbor,
    view.tx_id == blake2b_256(body_cbor),
    view.spend_inputs_hash == #"0101010101010101010101010101010101010101010101010101010101010101",
    view.compact_cbor == compact_cbor,
  }
}

test encode_native_byte_list_matches_node_preimage_shape() {
  let input_0 = #"1111"
  let input_1 = #"222222"
  let spend_input_cbors = [input_0, input_1]
  let preimage_cbor =
    #"8242"
      |> bytearray.concat(input_0)
      |> bytearray.concat(#"43")
      |> bytearray.concat(input_1)

  and {
    encode_native_byte_list(spend_input_cbors) == preimage_cbor,
    verify_spend_inputs_preimage(
      spend_input_cbors,
      blake2b_256(preimage_cbor),
    ),
    spend_input_cbor_at(spend_input_cbors, 1) == input_1,
  }
}
