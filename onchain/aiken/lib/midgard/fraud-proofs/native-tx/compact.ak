use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_bytes, expect_deserialise, expect_h32, expect_network_id,
  expect_non_negative, expect_validity_code, validity_from_code,
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

fn decode_native_tx_body_compact_data(data: Data) -> NativeTxBodyCompact {
  expect [
    spend_inputs_hash_data, reference_inputs_hash_data, outputs_hash_data,
    fee_data, validity_interval_start_data, validity_interval_end_data,
    required_observers_hash_data, required_signers_hash_data, mint_hash_data,
    script_integrity_hash_data, auxiliary_data_hash_data, network_id_data,
  ] = builtin.un_list_data(data)

  NativeTxBodyCompact {
    spend_inputs_hash: builtin.un_b_data(spend_inputs_hash_data),
    reference_inputs_hash: builtin.un_b_data(reference_inputs_hash_data),
    outputs_hash: builtin.un_b_data(outputs_hash_data),
    fee: builtin.un_i_data(fee_data),
    validity_interval_start: builtin.un_i_data(validity_interval_start_data),
    validity_interval_end: builtin.un_i_data(validity_interval_end_data),
    required_observers_hash: builtin.un_b_data(required_observers_hash_data),
    required_signers_hash: builtin.un_b_data(required_signers_hash_data),
    mint_hash: builtin.un_b_data(mint_hash_data),
    script_integrity_hash: builtin.un_b_data(script_integrity_hash_data),
    auxiliary_data_hash: builtin.un_b_data(auxiliary_data_hash_data),
    network_id: builtin.un_i_data(network_id_data),
  }
}

fn decode_native_tx_compact_data(data: Data) -> Pair<Int, NativeTxCompact> {
  expect [version_data,
    body_data,
    witness_set_hash_data, validity_code_data] = builtin.un_list_data(data)
  let version = builtin.un_i_data(version_data)
  expect version == native_tx_version

  let validity_code = builtin.un_i_data(validity_code_data)
  Pair(
    version,
    NativeTxCompact {
      body: decode_native_tx_body_compact_data(body_data),
      witness_set_hash: builtin.un_b_data(witness_set_hash_data),
      validity_code,
    },
  )
}

pub fn decode_native_tx_compact(compact_cbor: ByteArray) -> NativeTxCompact {
  let Pair(_, compact) =
    compact_cbor |> expect_deserialise |> decode_native_tx_compact_data
  compact
}

pub fn verify_native_tx_compact(
  native_tx_id: ByteArray,
  native_tx: NativeTxCompact,
  native_tx_compact_cbor: ByteArray,
) -> VerifiedMidgardNativeTxCompact {
  expect encode_native_tx_compact(native_tx) == native_tx_compact_cbor

  let NativeTxCompact { body, .. } = native_tx
  let computed_tx_id = encode_native_tx_body_compact(body) |> blake2b_256
  expect computed_tx_id == native_tx_id

  VerifiedMidgardNativeTxCompact {
    tx_id: native_tx_id,
    version: native_tx_version,
    body,
    witness_set_hash: native_tx.witness_set_hash,
    validity: native_tx.validity_code |> validity_from_code,
    validity_code: native_tx.validity_code,
    compact_cbor: native_tx_compact_cbor,
  }
}
