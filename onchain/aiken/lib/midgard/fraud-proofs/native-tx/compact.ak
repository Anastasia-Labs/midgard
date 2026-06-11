use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, decode_canonical_uint_at, encode_definite_bytes,
  expect_byte, expect_h32, expect_network_id, expect_non_negative,
  expect_validity_code, slice_len,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact, native_tx_version,
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
    |> bytearray.concat(encode_definite_bytes(spend_inputs_hash))
    |> bytearray.concat(encode_definite_bytes(reference_inputs_hash))
    |> bytearray.concat(encode_definite_bytes(outputs_hash))
    |> bytearray.concat(cbor.serialise(fee))
    |> bytearray.concat(cbor.serialise(validity_interval_start))
    |> bytearray.concat(cbor.serialise(validity_interval_end))
    |> bytearray.concat(encode_definite_bytes(required_observers_hash))
    |> bytearray.concat(encode_definite_bytes(required_signers_hash))
    |> bytearray.concat(encode_definite_bytes(mint_hash))
    |> bytearray.concat(encode_definite_bytes(script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(auxiliary_data_hash))
    |> bytearray.concat(cbor.serialise(network_id))
}

pub fn encode_native_tx_witness_set_compact(
  witness_set: NativeTxWitnessSetCompact,
) -> ByteArray {
  let NativeTxWitnessSetCompact {
    addr_tx_wits_hash,
    script_tx_wits_hash,
    redeemer_tx_wits_hash,
  } = witness_set

  expect _addr_tx_wits_hash = expect_h32(addr_tx_wits_hash)
  expect _script_tx_wits_hash = expect_h32(script_tx_wits_hash)
  expect _redeemer_tx_wits_hash = expect_h32(redeemer_tx_wits_hash)

  #"83"
    |> bytearray.concat(encode_definite_bytes(addr_tx_wits_hash))
    |> bytearray.concat(encode_definite_bytes(script_tx_wits_hash))
    |> bytearray.concat(encode_definite_bytes(redeemer_tx_wits_hash))
}

pub fn encode_native_tx_compact(tx: NativeTxCompact) -> ByteArray {
  let NativeTxCompact { body, witness_set_hash, validity_code } = tx

  expect _witness_set_hash = expect_h32(witness_set_hash)
  expect _validity_code = expect_validity_code(validity_code)

  #"84"
    |> bytearray.concat(cbor.serialise(native_tx_version))
    |> bytearray.concat(encode_native_tx_body_compact(body))
    |> bytearray.concat(encode_definite_bytes(witness_set_hash))
    |> bytearray.concat(cbor.serialise(validity_code))
}

fn decode_hash32_at(bytes: ByteArray, offset: Int) -> Pair<Int, ByteArray> {
  let offset = expect_byte(bytes, offset, 88)
  let offset = expect_byte(bytes, offset, 32)
  Pair(offset + 32, slice_len(bytes, offset, 32))
}

fn decode_native_tx_body_compact_cbor_at(
  compact_cbor: ByteArray,
  offset: Int,
) -> Pair<Int, NativeTxBodyCompact> {
  let offset = expect_byte(compact_cbor, offset, 140)
  let Pair(offset, spend_inputs_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, reference_inputs_hash) =
    decode_hash32_at(compact_cbor, offset)
  let Pair(offset, outputs_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, fee) = decode_canonical_uint_at(compact_cbor, offset)
  let Pair(offset, validity_interval_start) =
    decode_canonical_int_at(compact_cbor, offset)
  let Pair(offset, validity_interval_end) =
    decode_canonical_int_at(compact_cbor, offset)
  let Pair(offset, required_observers_hash) =
    decode_hash32_at(compact_cbor, offset)
  let Pair(offset, required_signers_hash) =
    decode_hash32_at(compact_cbor, offset)
  let Pair(offset, mint_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, script_integrity_hash) =
    decode_hash32_at(compact_cbor, offset)
  let Pair(offset, auxiliary_data_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, network_id) = decode_canonical_uint_at(compact_cbor, offset)

  Pair(
    offset,
    NativeTxBodyCompact {
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
      network_id: expect_network_id(network_id),
    },
  )
}

pub fn decode_native_tx_compact(compact_cbor: ByteArray) -> NativeTxCompact {
  let offset = expect_byte(compact_cbor, 0, 132)
  let offset = expect_byte(compact_cbor, offset, native_tx_version)
  let Pair(offset, body) =
    decode_native_tx_body_compact_cbor_at(compact_cbor, offset)
  let Pair(offset, witness_set_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, validity_code) =
    decode_canonical_uint_at(compact_cbor, offset)
  expect offset == bytearray.length(compact_cbor)

  NativeTxCompact {
    body,
    witness_set_hash,
    validity_code: expect_validity_code(validity_code),
  }
}

pub fn verify_native_tx_compact(
  native_tx_id: ByteArray,
  native_tx: NativeTxCompact,
  native_tx_compact_cbor: ByteArray,
) -> VerifiedMidgardNativeTxCompact {
  let NativeTxCompact { body, witness_set_hash, validity_code } = native_tx
  let midgard_tx_body_cbor = encode_native_tx_body_compact(body)

  let computed_tx_id = midgard_tx_body_cbor |> blake2b_256
  expect computed_tx_id == native_tx_id
  expect _witness_set_hash = expect_h32(witness_set_hash)
  expect _validity_code = expect_validity_code(validity_code)

  let midgard_tx_compact_cbor =
    #"84"
      |> bytearray.concat(cbor.serialise(native_tx_version))
      |> bytearray.concat(midgard_tx_body_cbor)
      |> bytearray.concat(encode_definite_bytes(witness_set_hash))
      |> bytearray.concat(cbor.serialise(validity_code))

  expect midgard_tx_compact_cbor == native_tx_compact_cbor

  VerifiedMidgardNativeTxCompact {
    tx_id: native_tx_id,
    version: native_tx_version,
    tx_compact: native_tx,
  }
}

pub fn verify_native_tx_compact_cbor(
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
) -> VerifiedMidgardNativeTxCompact {
  let offset = expect_byte(native_tx_compact_cbor, 0, 132)
  let offset = expect_byte(native_tx_compact_cbor, offset, native_tx_version)
  let body_start = offset
  let Pair(offset, body) =
    decode_native_tx_body_compact_cbor_at(native_tx_compact_cbor, offset)
  let body_cbor =
    slice_len(native_tx_compact_cbor, body_start, offset - body_start)
  expect blake2b_256(body_cbor) == native_tx_id

  let Pair(offset, witness_set_hash) =
    decode_hash32_at(native_tx_compact_cbor, offset)
  let Pair(offset, validity_code) =
    decode_canonical_uint_at(native_tx_compact_cbor, offset)
  expect offset == bytearray.length(native_tx_compact_cbor)

  VerifiedMidgardNativeTxCompact {
    tx_id: native_tx_id,
    version: native_tx_version,
    tx_compact: NativeTxCompact {
      body,
      witness_set_hash,
      validity_code: expect_validity_code(validity_code),
    },
  }
}
