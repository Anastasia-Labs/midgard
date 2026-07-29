use aiken/builtin
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, decode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_body_compact, encode_native_tx_compact_v1,
  encode_native_tx_field_preimage_lengths_v1, encode_native_tx_proof_source_v1,
  encode_native_tx_witness_set_compact, native_tx_full_hash_v1,
  native_tx_id_for_version, native_tx_proof_commitment_v1,
  verify_native_tx_compact_cbor_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/transaction
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxWitnessSetCompact, native_tx_version_v1,
}
use midgard/validation_merkle_v1

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

fn sample_compact() -> NativeTxCompact {
  NativeTxCompact {
    body: NativeTxBodyCompact {
      spend_inputs_hash: zero_hash,
      reference_inputs_hash: zero_hash,
      outputs_hash: zero_hash,
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers_hash: zero_hash,
      required_signers_hash: zero_hash,
      mint_hash: zero_hash,
      script_integrity_hash: zero_hash,
      auxiliary_data_hash: zero_hash,
      network_id: 255,
    },
    witness_set_hash: zero_hash,
    validity_code: 0,
  }
}

fn sample_witness_set_compact() -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: zero_hash,
    script_tx_wits_hash: zero_hash,
    redeemer_tx_wits_hash: zero_hash,
  }
}

fn sample_proof_compact() -> NativeTxCompact {
  let witness_set_cbor = encode_native_tx_witness_set_compact(
    sample_witness_set_compact(),
  )
  NativeTxCompact {
    ..sample_compact(),
    witness_set_hash: blake2b_256(witness_set_cbor),
  }
}

test v1_field_preimage_lengths_encode_scripts_before_address_witnesses() {
  let lengths = NativeTxFieldPreimageLengthsV1 {
    spend_inputs: 0,
    reference_inputs: 1,
    outputs: 2,
    required_observers: 3,
    required_signers: 4,
    mint: 5,
    address_witnesses: 7,
    script_witnesses: 6,
    redeemers: 8,
  }
  let encoded = encode_native_tx_field_preimage_lengths_v1(lengths)
  and {
    encoded == #"89000102030405060708",
    decode_native_tx_field_preimage_lengths_v1(encoded) == lengths,
  }
}

test v1_transaction_id_matches_typescript_domain_vector() {
  native_tx_id_for_version(native_tx_version_v1, #"80") == #"7d3e96630caf36e88e59bd85692050f6f69c0be4b28550e1671b9fd1942bdd0c"
}

test v1_full_transaction_hash_matches_typescript_domain_vector() {
  native_tx_full_hash_v1(#"80") == #"00039b53f81dc1d7d1e55c12c2369f2cbe7aa4c2943f990ddf7faee2377b3178"
}

test long_bytearray_serialisation_matches_typescript_chunking() {
  builtin.serialise_data(
    #"abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab",
  ) == #"5f5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5820ababababababababababababababababababababababababababababababababff"
}

test v1_compact_round_trips_through_exact_v1_entry_points() {
  let compact = sample_compact()
  let compact_cbor = encode_native_tx_compact_v1(compact)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let verified = verify_native_tx_compact_cbor_v1(tx_id, compact_cbor)

  and {
    decode_native_tx_compact_v1(compact_cbor) == compact,
    verified.version == native_tx_version_v1,
    verified.tx_compact == compact,
  }
}

test v1_proof_source_binds_compact_body_and_all_witness_field_hashes() {
  let compact = sample_proof_compact()
  let witness_set = sample_witness_set_compact()
  let compact_cbor = encode_native_tx_compact_v1(compact)
  let witness_set_cbor = encode_native_tx_witness_set_compact(witness_set)
  let field_preimage_lengths_cbor = #"89000000000000000000"
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let Pair(verified, verified_witness_set) = verify_native_tx_proof_source_v1(
    tx_id,
    compact_cbor,
    witness_set_cbor,
    field_preimage_lengths_cbor,
  )
  and {
    verified.tx_compact == compact,
    verified_witness_set == witness_set,
    bytearray.length(
      encode_native_tx_proof_source_v1(
        compact_cbor,
        witness_set_cbor,
        field_preimage_lengths_cbor,
      ),
    ) < 1024,
    bytearray.length(
      native_tx_proof_commitment_v1(
        compact_cbor,
        witness_set_cbor,
        field_preimage_lengths_cbor,
      ),
    ) == 32,
  }
}

test v1_transaction_field_chunk_is_bound_to_the_exact_proof_source() {
  let preimage = #"80"
  let item_leaf = bounded_item_v1.hash_chunk(0, 0, 0, preimage)
  let item_built = validation_merkle_v1.build_frontier([item_leaf])
  let item_commitment = bounded_item_v1.commitment(0, 0, 1, item_built.peaks)
  let collection_leaf = bounded_collection_v1.hash_item(
    0,
    0,
    1,
    item_commitment,
  )
  let collection_built = validation_merkle_v1.build_frontier([collection_leaf])
  let collection_commitment = bounded_collection_v1.commitment(
    0,
    1,
    collection_built.peaks,
  )
  let witness_set = sample_witness_set_compact()
  let witness_set_cbor = encode_native_tx_witness_set_compact(witness_set)
  let compact = NativeTxCompact {
    ..sample_compact(),
    body: NativeTxBodyCompact {
      ..sample_compact().body,
      spend_inputs_hash: collection_commitment,
    },
    witness_set_hash: blake2b_256(witness_set_cbor),
  }
  let compact_cbor = encode_native_tx_compact_v1(compact)
  let field_preimage_lengths_cbor = #"89030000000000000000"
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let tx_commitment = native_tx_proof_commitment_v1(
    compact_cbor,
    witness_set_cbor,
    field_preimage_lengths_cbor,
  )
  let collection_proof = bounded_collection_v1.ItemProofV1 {
    version: bounded_collection_v1.version,
    field_index: 0,
    item_count: 1,
    item_index: 0,
    item_length: 1,
    item_commitment,
    frontier: collection_built.peaks,
    siblings: [],
  }
  let proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 0,
    item_index: 0,
    total_length: 1,
    chunk_index: 0,
    chunk: preimage,
    frontier: item_built.peaks,
    siblings: [],
  }
  and {
    transaction.verify_midgard_transaction_field_chunk_v1(
      tx_id,
      tx_commitment,
      compact_cbor,
      witness_set_cbor,
      field_preimage_lengths_cbor,
      collection_proof,
      proof,
    ),
    !transaction.verify_midgard_transaction_field_chunk_v1(
      tx_id,
      tx_commitment,
      compact_cbor,
      witness_set_cbor,
      field_preimage_lengths_cbor,
      collection_proof,
      bounded_item_v1.ChunkProofV1 { ..proof, field_index: 1 },
    ),
  }
}
