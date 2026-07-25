use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{
  decode_definite_array_header_at, decode_definite_bytes_at, decode_int_at,
  decode_uint_at, encode_definite_bytes, encode_native_byte_list,
  validity_from_code, validity_to_code,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, encode_native_tx_body_compact,
  encode_native_tx_compact_v1, encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
  native_tx_proof_commitment_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_redeemer_witness,
  encode_midgard_tx_input, encode_midgard_tx_output,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_inputs_preimage_cbor, decode_midgard_tx_mint_preimage_cbor,
  decode_midgard_tx_outputs_preimage_cbor,
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor,
  encode_address_witness_preimage, encode_input_preimage, encode_mint_preimage,
  encode_output_preimage, encode_redeemer_witness_preimage,
  encode_script_witness_preimage, mint_policy_items,
  verify_canonical_mint_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardRedeemerWitness, MidgardScriptWitness,
  MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionBodyPartialPreimages, MidgardTransactionBodyPartialView,
  MidgardTransactionPartialView, MidgardTransactionWitnessSet,
  MidgardTransactionWitnessSetPartialPreimages,
  MidgardTransactionWitnessSetPartialView, MidgardTxInput, MidgardTxOutput,
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxProofSourceCborV1, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact, VerifiedMidgardTransaction,
  native_tx_version_v1,
}

fn decode_midgard_tx_inputs_preimage(
  field_index: Int,
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardTxInput> {
  let decoded = decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)
  expect
    bounded_collection_v1.from_items(
      field_index,
      list.map(decoded, encode_midgard_tx_input),
    ) == expected_hash
  decoded
}

fn decode_midgard_tx_outputs_preimage(
  field_index: Int,
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardTxOutput> {
  let decoded = decode_midgard_tx_outputs_preimage_cbor(preimage_cbor)
  expect
    bounded_collection_v1.from_items(
      field_index,
      list.map(decoded, encode_midgard_tx_output),
    ) == expected_hash
  decoded
}

fn decode_midgard_tx_byte_list_preimage(
  field_index: Int,
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<ByteArray> {
  let decoded = decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)
  expect bounded_collection_v1.from_items(field_index, decoded) == expected_hash
  decoded
}

fn decode_midgard_tx_mint_preimage(
  field_index: Int,
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> Data {
  let decoded = decode_midgard_tx_mint_preimage_cbor(preimage_cbor)
  expect
    bounded_collection_v1.from_items(field_index, mint_policy_items(decoded)) == expected_hash
  decoded
}

fn decode_midgard_tx_address_witnesses_preimage(
  field_index: Int,
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardAddressWitness> {
  let decoded = decode_midgard_tx_address_witnesses_preimage_cbor(preimage_cbor)
  expect
    bounded_collection_v1.from_items(
      field_index,
      list.map(decoded, encode_midgard_address_witness),
    ) == expected_hash
  decoded
}

fn decode_midgard_tx_script_witnesses_preimage(
  field_index: Int,
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardScriptWitness> {
  let decoded = decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)
  expect
    bounded_collection_v1.from_items(
      field_index,
      list.map(decoded, encode_midgard_versioned_script),
    ) == expected_hash
  decoded
}

fn decode_midgard_tx_redeemer_witnesses_preimage(
  field_index: Int,
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardRedeemerWitness> {
  let decoded = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    preimage_cbor,
  )
  expect
    bounded_collection_v1.from_items(
      field_index,
      list.map(decoded, encode_midgard_redeemer_witness),
    ) == expected_hash
  decoded
}

pub fn encode_midgard_transaction_body_full(
  body: MidgardTransactionBody,
) -> ByteArray {
  #"8c"
    |> bytearray.concat(
        encode_definite_bytes(encode_input_preimage(body.inputs)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_input_preimage(body.reference_inputs)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_output_preimage(body.outputs)),
      )
    |> bytearray.concat(cbor.serialise(body.fee))
    |> bytearray.concat(cbor.serialise(body.validity_interval_start))
    |> bytearray.concat(cbor.serialise(body.validity_interval_end))
    |> bytearray.concat(
        encode_definite_bytes(encode_native_byte_list(body.required_observers)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_native_byte_list(body.required_signers)),
      )
    |> bytearray.concat(encode_definite_bytes(encode_mint_preimage(body.mint)))
    |> bytearray.concat(encode_definite_bytes(body.script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(body.auxiliary_data_hash))
    |> bytearray.concat(cbor.serialise(body.network_id))
}

pub fn encode_midgard_transaction_witness_set_full(
  witness_set: MidgardTransactionWitnessSet,
) -> ByteArray {
  #"83"
    |> bytearray.concat(
        encode_definite_bytes(
          encode_address_witness_preimage(witness_set.addr_tx_wits),
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          encode_script_witness_preimage(witness_set.script_tx_wits),
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          encode_redeemer_witness_preimage(witness_set.redeemer_tx_wits),
        ),
      )
}

pub fn midgard_transaction_body_to_compact(
  body: MidgardTransactionBody,
) -> NativeTxBodyCompact {
  NativeTxBodyCompact {
    spend_inputs_hash: bounded_collection_v1.from_items(
      0,
      list.map(body.inputs, encode_midgard_tx_input),
    ),
    reference_inputs_hash: bounded_collection_v1.from_items(
      1,
      list.map(body.reference_inputs, encode_midgard_tx_input),
    ),
    outputs_hash: bounded_collection_v1.from_items(
      2,
      list.map(body.outputs, encode_midgard_tx_output),
    ),
    fee: body.fee,
    validity_interval_start: body.validity_interval_start,
    validity_interval_end: body.validity_interval_end,
    required_observers_hash: bounded_collection_v1.from_items(
      3,
      body.required_observers,
    ),
    required_signers_hash: bounded_collection_v1.from_items(
      4,
      body.required_signers,
    ),
    mint_hash: bounded_collection_v1.from_items(5, mint_policy_items(body.mint)),
    script_integrity_hash: body.script_integrity_hash,
    auxiliary_data_hash: body.auxiliary_data_hash,
    network_id: body.network_id,
  }
}

pub fn midgard_transaction_witness_set_to_compact(
  witness_set: MidgardTransactionWitnessSet,
) -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: bounded_collection_v1.from_items(
      6,
      list.map(witness_set.addr_tx_wits, encode_midgard_address_witness),
    ),
    script_tx_wits_hash: bounded_collection_v1.from_items(
      7,
      list.map(witness_set.script_tx_wits, encode_midgard_versioned_script),
    ),
    redeemer_tx_wits_hash: bounded_collection_v1.from_items(
      8,
      list.map(witness_set.redeemer_tx_wits, encode_midgard_redeemer_witness),
    ),
  }
}

pub fn midgard_transaction_to_compact(tx: MidgardTransaction) -> NativeTxCompact {
  let witness_set_hash = tx.witness_set
    |> midgard_transaction_witness_set_to_compact
    |> encode_native_tx_witness_set_compact
    |> blake2b_256

  NativeTxCompact {
    body: tx.body |> midgard_transaction_body_to_compact,
    witness_set_hash,
    validity_code: tx.validity |> validity_to_code,
  }
}

pub fn midgard_transaction_proof_source_v1(
  tx: MidgardTransaction,
) -> NativeTxProofSourceCborV1 {
  expect tx.version == native_tx_version_v1
  let compact_cbor = tx
    |> midgard_transaction_to_compact
    |> encode_native_tx_compact_v1
  let witness_set_compact_cbor = tx.witness_set
    |> midgard_transaction_witness_set_to_compact
    |> encode_native_tx_witness_set_compact
  let field_preimage_lengths_cbor = encode_native_tx_field_preimage_lengths_v1(
    NativeTxFieldPreimageLengthsV1 {
      spend_inputs: bytearray.length(encode_input_preimage(tx.body.inputs)),
      reference_inputs: bytearray.length(
        encode_input_preimage(tx.body.reference_inputs),
      ),
      outputs: bytearray.length(encode_output_preimage(tx.body.outputs)),
      required_observers: bytearray.length(
        encode_native_byte_list(tx.body.required_observers),
      ),
      required_signers: bytearray.length(
        encode_native_byte_list(tx.body.required_signers),
      ),
      mint: bytearray.length(encode_mint_preimage(tx.body.mint)),
      address_witnesses: bytearray.length(
        encode_address_witness_preimage(tx.witness_set.addr_tx_wits),
      ),
      script_witnesses: bytearray.length(
        encode_script_witness_preimage(tx.witness_set.script_tx_wits),
      ),
      redeemers: bytearray.length(
        encode_redeemer_witness_preimage(tx.witness_set.redeemer_tx_wits),
      ),
    },
  )
  NativeTxProofSourceCborV1 {
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  }
}

pub fn midgard_transaction_proof_commitment_v1(
  tx: MidgardTransaction,
) -> ByteArray {
  let source = midgard_transaction_proof_source_v1(tx)
  native_tx_proof_commitment_v1(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
}

fn verify_midgard_transaction_field_preimage_against_source_v1(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
  preimage_cbor: ByteArray,
) -> Bool {
  let NativeTxCompact { body, .. } = verified.tx_compact

  when field_index is {
    0 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.spend_inputs
      let decoded = decode_midgard_tx_inputs_preimage(
        0,
        preimage_cbor,
        body.spend_inputs_hash,
      )
      encode_input_preimage(decoded) == preimage_cbor
    }
    1 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.reference_inputs
      let decoded = decode_midgard_tx_inputs_preimage(
        1,
        preimage_cbor,
        body.reference_inputs_hash,
      )
      encode_input_preimage(decoded) == preimage_cbor
    }
    2 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.outputs
      let decoded = decode_midgard_tx_outputs_preimage(
        2,
        preimage_cbor,
        body.outputs_hash,
      )
      encode_output_preimage(decoded) == preimage_cbor
    }
    3 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.required_observers
      let decoded = decode_midgard_tx_byte_list_preimage(
        3,
        preimage_cbor,
        body.required_observers_hash,
      )
      encode_native_byte_list(decoded) == preimage_cbor
    }
    4 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.required_signers
      let decoded = decode_midgard_tx_byte_list_preimage(
        4,
        preimage_cbor,
        body.required_signers_hash,
      )
      encode_native_byte_list(decoded) == preimage_cbor
    }
    5 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.mint
      verify_canonical_mint_preimage_cbor(preimage_cbor, body.mint_hash)
    }
    6 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.address_witnesses
      let decoded = decode_midgard_tx_address_witnesses_preimage(
        6,
        preimage_cbor,
        witness_set.addr_tx_wits_hash,
      )
      encode_address_witness_preimage(decoded) == preimage_cbor
    }
    7 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.script_witnesses
      let decoded = decode_midgard_tx_script_witnesses_preimage(
        7,
        preimage_cbor,
        witness_set.script_tx_wits_hash,
      )
      encode_script_witness_preimage(decoded) == preimage_cbor
    }
    8 -> {
      expect bytearray.length(preimage_cbor) == field_lengths.redeemers
      let decoded = decode_midgard_tx_redeemer_witnesses_preimage(
        8,
        preimage_cbor,
        witness_set.redeemer_tx_wits_hash,
      )
      encode_redeemer_witness_preimage(decoded) == preimage_cbor
    }
    _ -> False
  }
}

fn transaction_field_commitment_v1(
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
) -> ByteArray {
  let NativeTxCompact { body, .. } = verified.tx_compact
  when field_index is {
    0 -> body.spend_inputs_hash
    1 -> body.reference_inputs_hash
    2 -> body.outputs_hash
    3 -> body.required_observers_hash
    4 -> body.required_signers_hash
    5 -> body.mint_hash
    6 -> witness_set.addr_tx_wits_hash
    7 -> witness_set.script_tx_wits_hash
    8 -> witness_set.redeemer_tx_wits_hash
    _ -> fail
  }
}

/// Verifies one independently revealed V1 field against the compact
/// transaction source. Each branch decodes and re-encodes the field so a hash
/// match alone can never authenticate a non-canonical or unsupported shape.
///
/// Field indices are consensus ordered:
/// 0 spend inputs, 1 reference inputs, 2 outputs, 3 observers, 4 signers,
/// 5 mint, 6 address witnesses, 7 script witnesses, 8 redeemers.
pub fn verify_midgard_transaction_field_preimage_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  field_index: Int,
  preimage_cbor: ByteArray,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  expect
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment

  verify_midgard_transaction_field_preimage_against_source_v1(
    verified,
    witness_set,
    field_lengths,
    field_index,
    preimage_cbor,
  )
}

/// Verifies one bounded field chunk without revealing or hashing the complete
/// field preimage. The proof is bound to the exact compact transaction,
/// committed aggregate field length, field index, and proof-source
/// commitment.
pub fn verify_midgard_transaction_field_chunk_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  and {
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment,
    collection_proof.field_index == proof.field_index,
    collection_proof.item_index == proof.item_index,
    collection_proof.item_length == proof.total_length,
    bounded_collection_v1.verify_item(
      transaction_field_commitment_v1(verified, witness_set, proof.field_index),
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, proof),
  }
}

/// Authenticates the bounded mint field against the V1 compact source
/// without recursively validating the entire map in one L1 step. Callers must
/// subsequently stream the canonical CBOR structure through a bounded state
/// machine before treating any mint semantics as valid.
pub fn verify_midgard_transaction_mint_preimage_commitment_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  preimage_cbor: ByteArray,
) -> Bool {
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  and {
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment,
    bytearray.length(preimage_cbor) == field_lengths.mint,
    verify_canonical_mint_preimage_cbor(
      preimage_cbor,
      verified.tx_compact.body.mint_hash,
    ),
  }
}

/// Verifies the complete forced-order fragment bundle while decoding the
/// compact proof source only once. Exactly one canonical fragment is required
/// for each of the nine consensus-ordered dynamic fields.
pub fn verify_midgard_transaction_field_preimages_v1(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  preimages: List<ByteArray>,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  expect
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == transaction_commitment
  expect
      [
        spend_inputs, reference_inputs, outputs, required_observers,
        required_signers, mint, address_witnesses, script_witnesses, redeemers,
      ]
    = preimages

  and {
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      0,
      spend_inputs,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      1,
      reference_inputs,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      2,
      outputs,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      3,
      required_observers,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      4,
      required_signers,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      5,
      mint,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      6,
      address_witnesses,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      7,
      script_witnesses,
    ),
    verify_midgard_transaction_field_preimage_against_source_v1(
      verified,
      witness_set,
      field_lengths,
      8,
      redeemers,
    ),
  }
}

fn encode_midgard_transaction_for_version(
  expected_version: Int,
  tx: MidgardTransaction,
) -> ByteArray {
  expect tx.version == expected_version
  #"84"
    |> bytearray.concat(cbor.serialise(tx.version))
    |> bytearray.concat(encode_midgard_transaction_body_full(tx.body))
    |> bytearray.concat(
        encode_midgard_transaction_witness_set_full(tx.witness_set),
      )
    |> bytearray.concat(cbor.serialise(tx.validity |> validity_to_code))
}

pub fn encode_midgard_transaction_v1(tx: MidgardTransaction) -> ByteArray {
  encode_midgard_transaction_for_version(native_tx_version_v1, tx)
}

pub fn decode_midgard_transaction_body_data(
  data: Data,
) -> MidgardTransactionBody {
  expect
      [
        inputs_preimage_cbor_data, reference_inputs_preimage_cbor_data,
        outputs_preimage_cbor_data, fee_data, validity_interval_start_data,
        validity_interval_end_data, required_observers_preimage_cbor_data,
        required_signers_preimage_cbor_data, mint_preimage_cbor_data,
        script_integrity_hash_data, auxiliary_data_hash_data, network_id_data,
      ]
    = builtin.un_list_data(data)

  MidgardTransactionBody {
    inputs: inputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_inputs_preimage_cbor,
    reference_inputs: reference_inputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_inputs_preimage_cbor,
    outputs: outputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_outputs_preimage_cbor,
    fee: builtin.un_i_data(fee_data),
    validity_interval_start: builtin.un_i_data(validity_interval_start_data),
    validity_interval_end: builtin.un_i_data(validity_interval_end_data),
    required_observers: required_observers_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_byte_list_preimage_cbor,
    required_signers: required_signers_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_byte_list_preimage_cbor,
    mint: mint_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_mint_preimage_cbor,
    script_integrity_hash: builtin.un_b_data(script_integrity_hash_data),
    auxiliary_data_hash: builtin.un_b_data(auxiliary_data_hash_data),
    network_id: builtin.un_i_data(network_id_data),
  }
}

pub fn decode_midgard_transaction_witness_set_data(
  data: Data,
) -> MidgardTransactionWitnessSet {
  expect
      [
        addr_tx_wits_preimage_cbor_data, script_tx_wits_preimage_cbor_data,
        redeemer_tx_wits_preimage_cbor_data,
      ]
    = builtin.un_list_data(data)

  MidgardTransactionWitnessSet {
    addr_tx_wits: addr_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_address_witnesses_preimage_cbor,
    script_tx_wits: script_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_script_witnesses_preimage_cbor,
    redeemer_tx_wits: redeemer_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  }
}

fn decode_midgard_transaction_body_at(
  tx_cbor: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardTransactionBody> {
  let Pair(offset, field_count) = decode_definite_array_header_at(
    tx_cbor,
    offset,
  )
  expect field_count == 12

  let Pair(offset, inputs_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, reference_inputs_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, outputs_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, fee) = decode_uint_at(tx_cbor, offset)
  let Pair(offset, validity_interval_start) = decode_int_at(tx_cbor, offset)
  let Pair(offset, validity_interval_end) = decode_int_at(tx_cbor, offset)
  let Pair(offset, required_observers_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, required_signers_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, mint_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, script_integrity_hash) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, auxiliary_data_hash) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, network_id) = decode_uint_at(tx_cbor, offset)

  Pair(
    offset,
    MidgardTransactionBody {
      inputs: inputs_preimage_cbor |> decode_midgard_tx_inputs_preimage_cbor,
      reference_inputs: reference_inputs_preimage_cbor
        |> decode_midgard_tx_inputs_preimage_cbor,
      outputs: outputs_preimage_cbor |> decode_midgard_tx_outputs_preimage_cbor,
      fee,
      validity_interval_start,
      validity_interval_end,
      required_observers: required_observers_preimage_cbor
        |> decode_midgard_tx_byte_list_preimage_cbor,
      required_signers: required_signers_preimage_cbor
        |> decode_midgard_tx_byte_list_preimage_cbor,
      mint: mint_preimage_cbor |> decode_midgard_tx_mint_preimage_cbor,
      script_integrity_hash,
      auxiliary_data_hash,
      network_id,
    },
  )
}

fn decode_midgard_transaction_witness_set_at(
  tx_cbor: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardTransactionWitnessSet> {
  let Pair(offset, field_count) = decode_definite_array_header_at(
    tx_cbor,
    offset,
  )
  expect field_count == 3

  let Pair(offset, addr_tx_wits_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, script_tx_wits_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, redeemer_tx_wits_preimage_cbor) = decode_definite_bytes_at(
    tx_cbor,
    offset,
  )

  Pair(
    offset,
    MidgardTransactionWitnessSet {
      addr_tx_wits: addr_tx_wits_preimage_cbor
        |> decode_midgard_tx_address_witnesses_preimage_cbor,
      script_tx_wits: script_tx_wits_preimage_cbor
        |> decode_midgard_tx_script_witnesses_preimage_cbor,
      redeemer_tx_wits: redeemer_tx_wits_preimage_cbor
        |> decode_midgard_tx_redeemer_witnesses_preimage_cbor,
    },
  )
}

fn decode_midgard_transaction_for_version(
  expected_version: Int,
  tx_cbor: ByteArray,
) -> MidgardTransaction {
  let Pair(offset, field_count) = decode_definite_array_header_at(tx_cbor, 0)
  expect field_count == 4
  let Pair(offset, version) = decode_uint_at(tx_cbor, offset)
  expect version == expected_version
  let Pair(offset, body) = decode_midgard_transaction_body_at(tx_cbor, offset)
  let Pair(offset, witness_set) = decode_midgard_transaction_witness_set_at(
    tx_cbor,
    offset,
  )
  let Pair(offset, validity_code) = decode_uint_at(tx_cbor, offset)
  expect offset == bytearray.length(tx_cbor)

  MidgardTransaction {
    version,
    validity: validity_code |> validity_from_code,
    body,
    witness_set,
  }
}

pub fn decode_midgard_transaction_v1(tx_cbor: ByteArray) -> MidgardTransaction {
  decode_midgard_transaction_for_version(native_tx_version_v1, tx_cbor)
}

fn verify_midgard_transaction_for_version(
  version: Int,
  tx_id: ByteArray,
  tx_cbor: ByteArray,
) -> VerifiedMidgardTransaction {
  let tx = decode_midgard_transaction_for_version(version, tx_cbor)
  let compact = midgard_transaction_to_compact(tx)
  expect version == native_tx_version_v1
  let compact_cbor = encode_native_tx_compact_v1(compact)
  expect
    native_tx_id_for_version(
      version,
      encode_native_tx_body_compact(compact.body),
    ) == tx_id

  VerifiedMidgardTransaction { tx_id, transaction: tx, compact, compact_cbor }
}

pub fn verify_midgard_transaction_v1(
  tx_id: ByteArray,
  tx_cbor: ByteArray,
) -> VerifiedMidgardTransaction {
  verify_midgard_transaction_for_version(native_tx_version_v1, tx_id, tx_cbor)
}

pub fn partial_view_from_compact(
  compact: VerifiedMidgardNativeTxCompact,
) -> MidgardTransactionPartialView {
  let NativeTxCompact { body, validity_code, .. } = compact.tx_compact
  MidgardTransactionPartialView {
    version: Some(compact.version),
    validity: Some(validity_code |> validity_from_code),
    body: Some(
      MidgardTransactionBodyPartialView {
        inputs: None,
        reference_inputs: None,
        outputs: None,
        fee: Some(body.fee),
        validity_interval_start: Some(body.validity_interval_start),
        validity_interval_end: Some(body.validity_interval_end),
        required_observers: None,
        required_signers: None,
        mint: None,
        script_integrity_hash: Some(body.script_integrity_hash),
        auxiliary_data_hash: Some(body.auxiliary_data_hash),
        network_id: Some(body.network_id),
      },
    ),
    witness_set: None,
  }
}

fn decode_optional_inputs_preimage(
  field_index: Int,
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardTxInput>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_inputs_preimage(
          field_index,
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_outputs_preimage(
  field_index: Int,
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardTxOutput>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_outputs_preimage(
          field_index,
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_byte_list_preimage(
  field_index: Int,
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<ByteArray>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_byte_list_preimage(
          field_index,
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_mint_preimage(
  field_index: Int,
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<Data> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_mint_preimage(
          field_index,
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_address_witnesses_preimage(
  field_index: Int,
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardAddressWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_address_witnesses_preimage(
          field_index,
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_script_witnesses_preimage(
  field_index: Int,
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardScriptWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_script_witnesses_preimage(
          field_index,
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_redeemer_witnesses_preimage(
  field_index: Int,
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardRedeemerWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_redeemer_witnesses_preimage(
          field_index,
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

pub fn partial_body_view_from_compact_and_preimages(
  body: NativeTxBodyCompact,
  preimages: MidgardTransactionBodyPartialPreimages,
) -> MidgardTransactionBodyPartialView {
  MidgardTransactionBodyPartialView {
    inputs: decode_optional_inputs_preimage(
      0,
      preimages.inputs,
      body.spend_inputs_hash,
    ),
    reference_inputs: decode_optional_inputs_preimage(
      1,
      preimages.reference_inputs,
      body.reference_inputs_hash,
    ),
    outputs: decode_optional_outputs_preimage(
      2,
      preimages.outputs,
      body.outputs_hash,
    ),
    fee: Some(body.fee),
    validity_interval_start: Some(body.validity_interval_start),
    validity_interval_end: Some(body.validity_interval_end),
    required_observers: decode_optional_byte_list_preimage(
      3,
      preimages.required_observers,
      body.required_observers_hash,
    ),
    required_signers: decode_optional_byte_list_preimage(
      4,
      preimages.required_signers,
      body.required_signers_hash,
    ),
    mint: decode_optional_mint_preimage(5, preimages.mint, body.mint_hash),
    script_integrity_hash: Some(body.script_integrity_hash),
    auxiliary_data_hash: Some(body.auxiliary_data_hash),
    network_id: Some(body.network_id),
  }
}

pub fn partial_witness_set_view_from_compact_and_preimages(
  witness_set: NativeTxWitnessSetCompact,
  preimages: MidgardTransactionWitnessSetPartialPreimages,
) -> MidgardTransactionWitnessSetPartialView {
  MidgardTransactionWitnessSetPartialView {
    addr_tx_wits: decode_optional_address_witnesses_preimage(
      6,
      preimages.addr_tx_wits,
      witness_set.addr_tx_wits_hash,
    ),
    script_tx_wits: decode_optional_script_witnesses_preimage(
      7,
      preimages.script_tx_wits,
      witness_set.script_tx_wits_hash,
    ),
    redeemer_tx_wits: decode_optional_redeemer_witnesses_preimage(
      8,
      preimages.redeemer_tx_wits,
      witness_set.redeemer_tx_wits_hash,
    ),
  }
}

pub fn partial_view_from_compact_and_preimages(
  compact: VerifiedMidgardNativeTxCompact,
  body_preimages: MidgardTransactionBodyPartialPreimages,
  witness_set: Option<NativeTxWitnessSetCompact>,
  witness_set_preimages: MidgardTransactionWitnessSetPartialPreimages,
) -> MidgardTransactionPartialView {
  let witness_set_view = when witness_set is {
    Some(witness_set) -> {
      let NativeTxCompact { witness_set_hash, .. } = compact.tx_compact
      expect
        blake2b_256(encode_native_tx_witness_set_compact(witness_set)) == witness_set_hash
      Some(
        partial_witness_set_view_from_compact_and_preimages(
          witness_set,
          witness_set_preimages,
        ),
      )
    }
    None -> None
  }
  let NativeTxCompact { body, validity_code, .. } = compact.tx_compact

  MidgardTransactionPartialView {
    version: Some(compact.version),
    validity: Some(validity_code |> validity_from_code),
    body: Some(
      partial_body_view_from_compact_and_preimages(body, body_preimages),
    ),
    witness_set: witness_set_view,
  }
}
