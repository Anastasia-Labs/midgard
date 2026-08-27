use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, decode_canonical_uint_at, encode_definite_bytes,
  expect_byte, expect_h32, expect_network_id, expect_non_negative,
  expect_validity_code, slice_len,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxWitnessSetCompact, VerifiedMidgardNativeTxCompact,
  native_tx_version_v1,
}

const native_tx_body_v1_domain: ByteArray =
  #"4d6964676172644e61746976655478426f64795631"

const native_tx_full_v1_domain: ByteArray =
  #"4d6964676172644e6174697665547846756c6c5631"

const native_tx_proof_source_v1_domain: ByteArray =
  #"4d6964676172644e6174697665547850726f6f66536f757263655631"

fn expect_supported_native_tx_version(version: Int) -> Int {
  expect version == native_tx_version_v1
  version
}

pub fn native_tx_id_for_version(version: Int, body_cbor: ByteArray) -> ByteArray {
  let version = expect_supported_native_tx_version(version)
  native_tx_body_v1_domain
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(body_cbor)
    |> blake2b_256
}

/// Hashes exact canonical native-V1 transaction bytes, including every witness
/// preimage. Callers must first decode the bytes with the exact-V1 decoder;
/// this helper only defines the domain-separated consensus commitment.
pub fn native_tx_full_hash_v1(native_tx_cbor: ByteArray) -> ByteArray {
  native_tx_full_v1_domain
    |> bytearray.concat(cbor.serialise(native_tx_version_v1))
    |> bytearray.concat(native_tx_cbor)
    |> blake2b_256
}

pub fn encode_native_tx_body_compact(body: NativeTxBodyCompact) -> ByteArray {
  let
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
        network_id,
      }
    = body

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
  let
      NativeTxWitnessSetCompact {
        addr_tx_wits_hash,
        script_tx_wits_hash,
        redeemer_tx_wits_hash,
      }
    = witness_set

  expect _addr_tx_wits_hash = expect_h32(addr_tx_wits_hash)
  expect _script_tx_wits_hash = expect_h32(script_tx_wits_hash)
  expect _redeemer_tx_wits_hash = expect_h32(redeemer_tx_wits_hash)

  #"83"
    |> bytearray.concat(encode_definite_bytes(addr_tx_wits_hash))
    |> bytearray.concat(encode_definite_bytes(script_tx_wits_hash))
    |> bytearray.concat(encode_definite_bytes(redeemer_tx_wits_hash))
}

pub fn decode_native_tx_witness_set_compact(
  witness_set_cbor: ByteArray,
) -> NativeTxWitnessSetCompact {
  let offset = expect_byte(witness_set_cbor, 0, 131)
  let Pair(offset, addr_tx_wits_hash) = decode_hash32_at(
    witness_set_cbor,
    offset,
  )
  let Pair(offset, script_tx_wits_hash) = decode_hash32_at(
    witness_set_cbor,
    offset,
  )
  let Pair(offset, redeemer_tx_wits_hash) = decode_hash32_at(
    witness_set_cbor,
    offset,
  )
  expect offset == bytearray.length(witness_set_cbor)
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash,
    script_tx_wits_hash,
    redeemer_tx_wits_hash,
  }
}

pub fn encode_native_tx_proof_source_v1(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
) -> ByteArray {
  #"83"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
}

pub fn native_tx_proof_commitment_v1(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
) -> ByteArray {
  native_tx_proof_source_v1_domain
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(
        encode_native_tx_proof_source_v1(
          compact_cbor,
          witness_set_compact_cbor,
          field_preimage_lengths_cbor,
        ),
      )
    |> blake2b_256
}

pub fn encode_native_tx_field_preimage_lengths_v1(
  lengths: NativeTxFieldPreimageLengthsV1,
) -> ByteArray {
  let
      NativeTxFieldPreimageLengthsV1 {
        spend_inputs,
        reference_inputs,
        outputs,
        required_observers,
        required_signers,
        mint,
        address_witnesses,
        script_witnesses,
        redeemers,
      }
    = lengths
  expect spend_inputs >= 0
  expect reference_inputs >= 0
  expect outputs >= 0
  expect required_observers >= 0
  expect required_signers >= 0
  expect mint >= 0
  expect address_witnesses >= 0
  expect script_witnesses >= 0
  expect redeemers >= 0
  #"89"
    |> bytearray.concat(cbor.serialise(spend_inputs))
    |> bytearray.concat(cbor.serialise(reference_inputs))
    |> bytearray.concat(cbor.serialise(outputs))
    |> bytearray.concat(cbor.serialise(required_observers))
    |> bytearray.concat(cbor.serialise(required_signers))
    |> bytearray.concat(cbor.serialise(mint))
    |> bytearray.concat(cbor.serialise(script_witnesses))
    |> bytearray.concat(cbor.serialise(address_witnesses))
    |> bytearray.concat(cbor.serialise(redeemers))
}

pub fn decode_native_tx_field_preimage_lengths_v1(
  lengths_cbor: ByteArray,
) -> NativeTxFieldPreimageLengthsV1 {
  let offset = expect_byte(lengths_cbor, 0, 137)
  let Pair(offset, spend_inputs) = decode_canonical_uint_at(
    lengths_cbor,
    offset,
  )
  let Pair(offset, reference_inputs) = decode_canonical_uint_at(
    lengths_cbor,
    offset,
  )
  let Pair(offset, outputs) = decode_canonical_uint_at(lengths_cbor, offset)
  let Pair(offset, required_observers) = decode_canonical_uint_at(
    lengths_cbor,
    offset,
  )
  let Pair(offset, required_signers) = decode_canonical_uint_at(
    lengths_cbor,
    offset,
  )
  let Pair(offset, mint) = decode_canonical_uint_at(lengths_cbor, offset)
  let Pair(offset, script_witnesses) = decode_canonical_uint_at(
    lengths_cbor,
    offset,
  )
  let Pair(offset, address_witnesses) = decode_canonical_uint_at(
    lengths_cbor,
    offset,
  )
  let Pair(offset, redeemers) = decode_canonical_uint_at(lengths_cbor, offset)
  expect offset == bytearray.length(lengths_cbor)
  NativeTxFieldPreimageLengthsV1 {
    spend_inputs,
    reference_inputs,
    outputs,
    required_observers,
    required_signers,
    mint,
    address_witnesses,
    script_witnesses,
    redeemers,
  }
}

fn definite_bytes_encoded_size(payload_size: Int) -> Int {
  expect payload_size >= 0
  if payload_size < 24 {
    1 + payload_size
  } else if payload_size <= 255 {
    2 + payload_size
  } else if payload_size <= 65535 {
    3 + payload_size
  } else {
    5 + payload_size
  }
}

/// Exact size of the canonical native-V1 full transaction implied by the
/// compact fixed fields and the nine committed dynamic-field lengths.
pub fn native_tx_canonical_size_v1(
  compact: NativeTxCompact,
  lengths: NativeTxFieldPreimageLengthsV1,
) -> Int {
  let body = compact.body
  let body_size = 1 + definite_bytes_encoded_size(lengths.spend_inputs) + definite_bytes_encoded_size(
    lengths.reference_inputs,
  ) + definite_bytes_encoded_size(lengths.outputs) + bytearray.length(
    cbor.serialise(body.fee),
  ) + bytearray.length(cbor.serialise(body.validity_interval_start)) + bytearray.length(
    cbor.serialise(body.validity_interval_end),
  ) + definite_bytes_encoded_size(lengths.required_observers) + definite_bytes_encoded_size(
    lengths.required_signers,
  ) + definite_bytes_encoded_size(lengths.mint) + 34 + 34 + bytearray.length(
    cbor.serialise(body.network_id),
  )
  let witness_set_size = 1 + definite_bytes_encoded_size(
    lengths.address_witnesses,
  ) + definite_bytes_encoded_size(lengths.script_witnesses) + definite_bytes_encoded_size(
    lengths.redeemers,
  )
  1 + bytearray.length(cbor.serialise(native_tx_version_v1)) + body_size + witness_set_size + bytearray.length(
    cbor.serialise(compact.validity_code),
  )
}

/// The network minimum fee for a native-V1 transaction whose canonical full
/// serialization is `canonical_tx_size` bytes, under the header's fee
/// schedule: `min_fee_a * canonical_tx_size + min_fee_b`.
///
/// The one source of the formula. The validation machine's static-rules phase
/// (`reject_min_fee`) and the standalone `min-fee` fault-proof family both
/// adjudicate the same ledger rule, so both call this function and cannot
/// drift apart.
pub fn min_fee_lovelace_v1(
  min_fee_a: Int,
  min_fee_b: Int,
  canonical_tx_size: Int,
) -> Int {
  min_fee_a * canonical_tx_size + min_fee_b
}

pub fn encode_native_tx_compact_for_version(
  version: Int,
  tx: NativeTxCompact,
) -> ByteArray {
  let NativeTxCompact { body, witness_set_hash, validity_code } = tx

  let version = expect_supported_native_tx_version(version)
  expect _witness_set_hash = expect_h32(witness_set_hash)
  expect _validity_code = expect_validity_code(validity_code)

  #"84"
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(encode_native_tx_body_compact(body))
    |> bytearray.concat(encode_definite_bytes(witness_set_hash))
    |> bytearray.concat(cbor.serialise(validity_code))
}

pub fn encode_native_tx_compact_v1(tx: NativeTxCompact) -> ByteArray {
  encode_native_tx_compact_for_version(native_tx_version_v1, tx)
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
  let Pair(offset, reference_inputs_hash) = decode_hash32_at(
    compact_cbor,
    offset,
  )
  let Pair(offset, outputs_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, fee) = decode_canonical_uint_at(compact_cbor, offset)
  let Pair(offset, validity_interval_start) = decode_canonical_int_at(
    compact_cbor,
    offset,
  )
  let Pair(offset, validity_interval_end) = decode_canonical_int_at(
    compact_cbor,
    offset,
  )
  let Pair(offset, required_observers_hash) = decode_hash32_at(
    compact_cbor,
    offset,
  )
  let Pair(offset, required_signers_hash) = decode_hash32_at(
    compact_cbor,
    offset,
  )
  let Pair(offset, mint_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, script_integrity_hash) = decode_hash32_at(
    compact_cbor,
    offset,
  )
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

fn decode_native_tx_compact_for_version(
  expected_version: Int,
  compact_cbor: ByteArray,
) -> NativeTxCompact {
  let expected_version = expect_supported_native_tx_version(expected_version)
  let offset = expect_byte(compact_cbor, 0, 132)
  let offset = expect_byte(compact_cbor, offset, expected_version)
  let Pair(offset, body) = decode_native_tx_body_compact_cbor_at(
    compact_cbor,
    offset,
  )
  let Pair(offset, witness_set_hash) = decode_hash32_at(compact_cbor, offset)
  let Pair(offset, validity_code) = decode_canonical_uint_at(
    compact_cbor,
    offset,
  )
  expect offset == bytearray.length(compact_cbor)

  NativeTxCompact {
    body,
    witness_set_hash,
    validity_code: expect_validity_code(validity_code),
  }
}

pub fn decode_native_tx_compact_v1(compact_cbor: ByteArray) -> NativeTxCompact {
  decode_native_tx_compact_for_version(native_tx_version_v1, compact_cbor)
}

fn verify_native_tx_compact_for_version(
  version: Int,
  native_tx_id: ByteArray,
  native_tx: NativeTxCompact,
  native_tx_compact_cbor: ByteArray,
) -> VerifiedMidgardNativeTxCompact {
  let version = expect_supported_native_tx_version(version)
  let NativeTxCompact { body, witness_set_hash, validity_code } = native_tx
  let midgard_tx_body_cbor = encode_native_tx_body_compact(body)

  let computed_tx_id = native_tx_id_for_version(version, midgard_tx_body_cbor)
  expect computed_tx_id == native_tx_id
  expect _witness_set_hash = expect_h32(witness_set_hash)
  expect _validity_code = expect_validity_code(validity_code)

  let midgard_tx_compact_cbor = #"84"
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(midgard_tx_body_cbor)
    |> bytearray.concat(encode_definite_bytes(witness_set_hash))
    |> bytearray.concat(cbor.serialise(validity_code))

  expect midgard_tx_compact_cbor == native_tx_compact_cbor

  VerifiedMidgardNativeTxCompact {
    tx_id: native_tx_id,
    version,
    tx_compact: native_tx,
  }
}

pub fn verify_native_tx_compact_v1(
  native_tx_id: ByteArray,
  native_tx: NativeTxCompact,
  native_tx_compact_cbor: ByteArray,
) -> VerifiedMidgardNativeTxCompact {
  verify_native_tx_compact_for_version(
    native_tx_version_v1,
    native_tx_id,
    native_tx,
    native_tx_compact_cbor,
  )
}

fn verify_native_tx_compact_cbor_for_version(
  version: Int,
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
) -> VerifiedMidgardNativeTxCompact {
  let version = expect_supported_native_tx_version(version)
  let offset = expect_byte(native_tx_compact_cbor, 0, 132)
  let offset = expect_byte(native_tx_compact_cbor, offset, version)
  let body_start = offset
  let Pair(offset, body) = decode_native_tx_body_compact_cbor_at(
    native_tx_compact_cbor,
    offset,
  )
  let body_cbor = slice_len(
    native_tx_compact_cbor,
    body_start,
    offset - body_start,
  )
  expect native_tx_id_for_version(version, body_cbor) == native_tx_id

  let Pair(offset, witness_set_hash) = decode_hash32_at(
    native_tx_compact_cbor,
    offset,
  )
  let Pair(offset, validity_code) = decode_canonical_uint_at(
    native_tx_compact_cbor,
    offset,
  )
  expect offset == bytearray.length(native_tx_compact_cbor)

  VerifiedMidgardNativeTxCompact {
    tx_id: native_tx_id,
    version,
    tx_compact: NativeTxCompact {
      body,
      witness_set_hash,
      validity_code: expect_validity_code(validity_code),
    },
  }
}

pub fn verify_native_tx_compact_cbor_v1(
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
) -> VerifiedMidgardNativeTxCompact {
  verify_native_tx_compact_cbor_for_version(
    native_tx_version_v1,
    native_tx_id,
    native_tx_compact_cbor,
  )
}

pub fn verify_native_tx_proof_source_v1(
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
) -> Pair<VerifiedMidgardNativeTxCompact, NativeTxWitnessSetCompact> {
  let compact = verify_native_tx_compact_cbor_v1(
    native_tx_id,
    native_tx_compact_cbor,
  )
  let witness_set = decode_native_tx_witness_set_compact(
    witness_set_compact_cbor,
  )
  expect
    blake2b_256(witness_set_compact_cbor) == compact.tx_compact.witness_set_hash
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  expect
    encode_native_tx_field_preimage_lengths_v1(field_lengths) == field_preimage_lengths_cbor
  Pair(compact, witness_set)
}
