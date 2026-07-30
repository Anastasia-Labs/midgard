use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec as native_codec
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, decode_native_tx_field_preimage_lengths_v1,
  decode_native_tx_witness_set_compact, encode_native_tx_body_compact,
  encode_native_tx_compact_v1, encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_proof_source_v1, encode_native_tx_witness_set_compact,
  native_tx_canonical_size_v1, native_tx_full_hash_v1, native_tx_id_for_version,
  native_tx_proof_commitment_v1, verify_native_tx_compact_cbor_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor,
  encode_address_witness_preimage, encode_script_witness_preimage,
}
use midgard/fraud_proofs/native_tx/transaction
use midgard/fraud_proofs/native_tx/types.{
  FailedScript, FeeTooLow, InvalidSignature, MidgardAddressWitness,
  MidgardV1Script, MidgardVersionedScript, NativeCardanoScript,
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxWitnessSetCompact, NonExistentInputUtxo, PlutusV3Script, TxIsValid,
  UnbalancedTx, native_tx_version_v1,
}
use midgard/validation_merkle_v1

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const n01_canonical_full_cbor =
  #"84018c418041804180172018184180418041805820111111111111111111111111111111111111111111111111111111111111111158202222222222222222222222222222222222222222222222222222222222222222018341804180418003"

const n03_compact_body_cbor =
  #"8c5820eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec765820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58203e8a3792d0d05e816cc08e97bdbd1f0e4375a9ef83f196c8c51705ec6f463e20172018185820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582011111111111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222201"

const n02_compact_cbor =
  #"84018c5820eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec765820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58203e8a3792d0d05e816cc08e97bdbd1f0e4375a9ef83f196c8c51705ec6f463e20172018185820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d58201111111111111111111111111111111111111111111111111111111111111111582022222222222222222222222222222222222222222222222222222222222222220158206ab07ab512fb3a2860ddc7f57ca940073e9300c01e62c9b51cf2de2a0d7d964303"

const n04_canonical_body_cbor =
  #"8c41804180418017201818418041804180582011111111111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222201"

const n05_compact_witness_cbor =
  #"835820a8c162c7cb77ea61f1195a083ecb8a64b97410652666d380b0e52f90eb74bb9e5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"

const n06_canonical_witness_cbor = #"83418041804180"

const n08_field_preimage_lengths_cbor = #"89010101010101010101"

const n07_proof_source_cbor =
  #"8359013b84018c5820eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec765820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58203e8a3792d0d05e816cc08e97bdbd1f0e4375a9ef83f196c8c51705ec6f463e20172018185820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d58201111111111111111111111111111111111111111111111111111111111111111582022222222222222222222222222222222222222222222222222222222222222220158206ab07ab512fb3a2860ddc7f57ca940073e9300c01e62c9b51cf2de2a0d7d9643035867835820a8c162c7cb77ea61f1195a083ecb8a64b97410652666d380b0e52f90eb74bb9e5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef34a89010101010101010101"

const n02_transaction_id =
  #"7d098ce4ee22ae378974ca4560877d3ae850c90ff3b36b5ddcc4675dbb77dedc"

const n07_proof_commitment =
  #"b5abe2283df143ce1d2da68a8c439ebf826af7d43324216cec3e1068f824a8a2"

const n09_full_hash =
  #"aee3bfa02625d824be4c2b786d5e6f4d159a1903ef282ab3434634e4da2735e1"

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

fn singleton_field_proofs(
  field_index: Int,
  item: ByteArray,
) -> Pair<bounded_collection_v1.ItemProofV1, bounded_item_v1.ChunkProofV1> {
  let item_leaf = bounded_item_v1.hash_chunk(field_index, 0, 0, item)
  let item_frontier = validation_merkle_v1.build_frontier([item_leaf])
  let item_commitment = bounded_item_v1.commitment(
    field_index,
    0,
    bytearray.length(item),
    item_frontier.peaks,
  )
  let collection_leaf = bounded_collection_v1.hash_item(
    field_index,
    0,
    bytearray.length(item),
    item_commitment,
  )
  let collection_frontier = validation_merkle_v1.build_frontier(
    [collection_leaf],
  )
  Pair(
    bounded_collection_v1.ItemProofV1 {
      version: bounded_collection_v1.version,
      field_index,
      item_count: 1,
      item_index: 0,
      item_length: bytearray.length(item),
      item_commitment,
      frontier: collection_frontier.peaks,
      siblings: [],
    },
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index,
      item_index: 0,
      total_length: bytearray.length(item),
      chunk_index: 0,
      chunk: item,
      frontier: item_frontier.peaks,
      siblings: [],
    },
  )
}

test v1_network_id_language_matches_typescript_exactly() {
  and {
    native_codec.expect_network_id(0) == 0,
    native_codec.expect_network_id(1) == 1,
    native_codec.expect_network_id(255) == 255,
  }
}

test v1_network_id_adjacent_unknown_rejects() fail {
  native_codec.expect_network_id(2) == 2
}

test v1_n11_validity_code_and_plutus_constructor_bijection_is_exact() {
  and {
    native_codec.validity_from_code(0) == TxIsValid,
    native_codec.validity_from_code(1) == NonExistentInputUtxo,
    native_codec.validity_from_code(2) == InvalidSignature,
    native_codec.validity_from_code(3) == FailedScript,
    native_codec.validity_from_code(4) == FeeTooLow,
    native_codec.validity_from_code(5) == UnbalancedTx,
    native_codec.validity_to_code(TxIsValid) == 0,
    native_codec.validity_to_code(NonExistentInputUtxo) == 1,
    native_codec.validity_to_code(InvalidSignature) == 2,
    native_codec.validity_to_code(FailedScript) == 3,
    native_codec.validity_to_code(FeeTooLow) == 4,
    native_codec.validity_to_code(UnbalancedTx) == 5,
    builtin.serialise_data(native_codec.validity_to_plutus_data(TxIsValid)) == #"d87980",
    builtin.serialise_data(
      native_codec.validity_to_plutus_data(NonExistentInputUtxo),
    ) == #"d87a80",
    builtin.serialise_data(
      native_codec.validity_to_plutus_data(InvalidSignature),
    ) == #"d87b80",
    builtin.serialise_data(native_codec.validity_to_plutus_data(FailedScript)) == #"d87c80",
    builtin.serialise_data(native_codec.validity_to_plutus_data(FeeTooLow)) == #"d87d80",
    builtin.serialise_data(native_codec.validity_to_plutus_data(UnbalancedTx)) == #"d87e80",
    native_codec.validity_from_plutus_data(
      native_codec.expect_deserialise(#"d87980"),
    ) == TxIsValid,
    native_codec.validity_from_plutus_data(
      native_codec.expect_deserialise(#"d87a80"),
    ) == NonExistentInputUtxo,
    native_codec.validity_from_plutus_data(
      native_codec.expect_deserialise(#"d87b80"),
    ) == InvalidSignature,
    native_codec.validity_from_plutus_data(
      native_codec.expect_deserialise(#"d87c80"),
    ) == FailedScript,
    native_codec.validity_from_plutus_data(
      native_codec.expect_deserialise(#"d87d80"),
    ) == FeeTooLow,
    native_codec.validity_from_plutus_data(
      native_codec.expect_deserialise(#"d87e80"),
    ) == UnbalancedTx,
  }
}

test v1_n11_negative_validity_code_rejects() fail {
  native_codec.validity_from_code(-1) == TxIsValid
}

test v1_n11_adjacent_validity_code_and_constructor_rejects() fail {
  native_codec.validity_from_plutus_data(
    native_codec.expect_deserialise(#"d87f80"),
  ) == UnbalancedTx
}

test v1_n11_non_nullary_validity_constructor_rejects() fail {
  native_codec.validity_from_plutus_data(
    native_codec.expect_deserialise(#"d8798100"),
  ) == TxIsValid
}

test v1_n11_noncanonical_compact_validity_code_rejects() fail {
  decode_native_tx_compact_v1(
    bytearray.concat(
      bytearray.take(n02_compact_cbor, bytearray.length(n02_compact_cbor) - 1),
      #"1803",
    ),
  ).validity_code == 3
}

test v1_n11_out_of_range_compact_validity_code_rejects() fail {
  decode_native_tx_compact_v1(
    bytearray.concat(
      bytearray.take(n02_compact_cbor, bytearray.length(n02_compact_cbor) - 1),
      #"06",
    ),
  ).validity_code == 6
}

test v1_n01_canonical_full_transaction_exact_vector() {
  let tx = transaction.decode_midgard_transaction_v1(n01_canonical_full_cbor)
  and {
    tx.version == native_tx_version_v1,
    transaction.encode_midgard_transaction_body_full(tx.body) == n04_canonical_body_cbor,
    transaction.encode_midgard_transaction_witness_set_full(tx.witness_set) == n06_canonical_witness_cbor,
    transaction.encode_midgard_transaction_v1(tx) == n01_canonical_full_cbor,
  }
}

test v1_n01_obsolete_v2_full_transaction_rejects() fail {
  transaction.decode_midgard_transaction_v1(
    bytearray.concat(#"8402", bytearray.drop(n01_canonical_full_cbor, 2)),
  ).version == 2
}

test v1_n02_obsolete_v3_compact_transaction_rejects() fail {
  decode_native_tx_compact_v1(
    bytearray.concat(#"8403", bytearray.drop(n02_compact_cbor, 2)),
  ).validity_code == 0
}

test v1_n01_noncanonical_version_encoding_rejects() fail {
  transaction.decode_midgard_transaction_v1(
    bytearray.concat(#"841801", bytearray.drop(n01_canonical_full_cbor, 2)),
  ).version == native_tx_version_v1
}

test v1_n02_n06_compact_and_preimage_exact_vector() {
  let tx = transaction.decode_midgard_transaction_v1(n01_canonical_full_cbor)
  let compact = transaction.midgard_transaction_to_compact(tx)
  let witness_set = transaction.midgard_transaction_witness_set_to_compact(
    tx.witness_set,
  )
  and {
    encode_native_tx_body_compact(compact.body) == n03_compact_body_cbor,
    encode_native_tx_compact_v1(compact) == n02_compact_cbor,
    decode_native_tx_compact_v1(n02_compact_cbor) == compact,
    encode_native_tx_witness_set_compact(witness_set) == n05_compact_witness_cbor,
    blake2b_256(n05_compact_witness_cbor) == compact.witness_set_hash,
    native_tx_id_for_version(native_tx_version_v1, n03_compact_body_cbor) == n02_transaction_id,
    verify_native_tx_compact_cbor_v1(n02_transaction_id, n02_compact_cbor).tx_compact == compact,
  }
}

test v1_n02_adjacent_transaction_id_mutation_rejects() fail {
  verify_native_tx_compact_cbor_v1(
    #"7d098ce4ee22ae378974ca4560877d3ae850c90ff3b36b5ddcc4675dbb77dedb",
    n02_compact_cbor,
  ).version == native_tx_version_v1
}

test v1_n07_n09_proof_source_and_full_hash_exact_vector() {
  let tx = transaction.decode_midgard_transaction_v1(n01_canonical_full_cbor)
  let source = transaction.midgard_transaction_proof_source_v1(tx)
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    source.field_preimage_lengths_cbor,
  )
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    n02_transaction_id,
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
  and {
    source.compact_cbor == n02_compact_cbor,
    source.witness_set_compact_cbor == n05_compact_witness_cbor,
    source.field_preimage_lengths_cbor == n08_field_preimage_lengths_cbor,
    encode_native_tx_field_preimage_lengths_v1(lengths) == n08_field_preimage_lengths_cbor,
    lengths.spend_inputs == 1,
    lengths.reference_inputs == 1,
    lengths.outputs == 1,
    lengths.required_observers == 1,
    lengths.required_signers == 1,
    lengths.mint == 1,
    lengths.address_witnesses == 1,
    lengths.script_witnesses == 1,
    lengths.redeemers == 1,
    encode_native_tx_proof_source_v1(
      source.compact_cbor,
      source.witness_set_compact_cbor,
      source.field_preimage_lengths_cbor,
    ) == n07_proof_source_cbor,
    native_tx_proof_commitment_v1(
      source.compact_cbor,
      source.witness_set_compact_cbor,
      source.field_preimage_lengths_cbor,
    ) == n07_proof_commitment,
    native_tx_canonical_size_v1(verified.tx_compact, lengths) == 96,
    encode_native_tx_witness_set_compact(witness_set) == n05_compact_witness_cbor,
    native_tx_full_hash_v1(n01_canonical_full_cbor) == n09_full_hash,
    native_tx_proof_commitment_v1(
      source.compact_cbor,
      source.witness_set_compact_cbor,
      #"89020101010101010101",
    ) != n07_proof_commitment,
    native_tx_full_hash_v1(bytearray.concat(n01_canonical_full_cbor, #"00")) != n09_full_hash,
  }
}

test v1_c20_6_c20_7_field_directory_and_witness_tuple_are_exact() {
  let script_item = #"8300581c111111111111111111111111111111111111111111111111111111114100"
  let address_item = #"8258202222222222222222222222222222222222222222222222222222222222222222584033333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333"
  let redeemer_hash = #"4444444444444444444444444444444444444444444444444444444444444444"
  let Pair(script_proof, script_chunk) = singleton_field_proofs(6, script_item)
  let Pair(address_proof, address_chunk) = singleton_field_proofs(
    7,
    address_item,
  )
  let Pair(address_as_script_proof, address_as_script_chunk) = singleton_field_proofs(
    6,
    address_item,
  )
  let Pair(script_as_address_proof, script_as_address_chunk) = singleton_field_proofs(
    7,
    script_item,
  )
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: bounded_collection_v1.from_items(7, [address_item]),
    script_tx_wits_hash: bounded_collection_v1.from_items(6, [script_item]),
    redeemer_tx_wits_hash: redeemer_hash,
  }
  let witness_set_cbor = encode_native_tx_witness_set_compact(witness_set)
  let compact = NativeTxCompact {
    ..sample_compact(),
    witness_set_hash: blake2b_256(witness_set_cbor),
  }
  let compact_cbor = encode_native_tx_compact_v1(compact)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let lengths = NativeTxFieldPreimageLengthsV1 {
    spend_inputs: 0,
    reference_inputs: 0,
    outputs: 0,
    required_observers: 0,
    required_signers: 0,
    mint: 0,
    address_witnesses: 7,
    script_witnesses: 6,
    redeemers: 8,
  }
  let lengths_cbor = encode_native_tx_field_preimage_lengths_v1(lengths)
  let decoded_lengths = decode_native_tx_field_preimage_lengths_v1(lengths_cbor)
  let transaction_commitment = native_tx_proof_commitment_v1(
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
  )
  let expected_witness_set_cbor = #"835820"
    |> bytearray.concat(witness_set.addr_tx_wits_hash)
    |> bytearray.concat(#"5820")
    |> bytearray.concat(witness_set.script_tx_wits_hash)
    |> bytearray.concat(#"5820")
    |> bytearray.concat(redeemer_hash)

  and {
    // The named witness-set tuple ABI remains [address, script, redeemer].
    witness_set_cbor == expected_witness_set_cbor,
    // The dynamic field-length directory is [..., script(6), address(7), redeemer(8)].
    lengths_cbor == #"89000000000000060708",
    decoded_lengths.script_witnesses == 6,
    decoded_lengths.address_witnesses == 7,
    script_proof.item_commitment == bounded_item_v1.from_bytes(
      6,
      0,
      script_item,
    ),
    address_proof.item_commitment == bounded_item_v1.from_bytes(
      7,
      0,
      address_item,
    ),
    bounded_collection_v1.verify_item(
      witness_set.script_tx_wits_hash,
      script_proof,
    ),
    bounded_collection_v1.verify_item(
      witness_set.addr_tx_wits_hash,
      address_proof,
    ),
    bounded_item_v1.verify_chunk(script_proof.item_commitment, script_chunk),
    bounded_item_v1.verify_chunk(address_proof.item_commitment, address_chunk),
    transaction.verify_midgard_transaction_field_chunk_v1(
      tx_id,
      transaction_commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      script_proof,
      script_chunk,
    ),
    transaction.verify_midgard_transaction_field_chunk_v1(
      tx_id,
      transaction_commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      address_proof,
      address_chunk,
    ),
    !transaction.verify_midgard_transaction_field_chunk_v1(
      tx_id,
      transaction_commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      address_as_script_proof,
      address_as_script_chunk,
    ),
    !transaction.verify_midgard_transaction_field_chunk_v1(
      tx_id,
      transaction_commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      script_as_address_proof,
      script_as_address_chunk,
    ),
  }
}

test v1_n08_adjacent_eight_field_length_tuple_rejects() fail {
  decode_native_tx_field_preimage_lengths_v1(#"880101010101010101").spend_inputs == 1
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

// === C20-6 / C20-7: field 6 script witnesses and field 7 vkey witnesses ===
//
// Cross-substitution, wrong-arity/wrong-tag adjacency, duplicate/reorder,
// canonical-form, and accepted-Cardano-maximum coverage for the complete
// full-preimage verification path. Maximum-fixture constants are derived by
// the genuine Cardano boundary fixtures in
// demo/midgard-validation/tests/ordered-collection-signer-witness-boundary-v1.test.ts
// (fields 4/7, accepted 124 vkey witnesses, adjacent 125 rejected at
// 16,482 > 16,384 signed Cardano bytes) and
// demo/midgard-validation/tests/ordered-collection-observer-native-script-boundary-v1.test.ts
// (fields 3/6, accepted 224 native-script witnesses, adjacent 225 rejected at
// 16,410 > 16,384 signed Cardano bytes).

const cardano_max_transaction_bytes = 16384

fn c20_addr_witness_1() -> MidgardAddressWitness {
  MidgardAddressWitness {
    verification_key: #"1111111111111111111111111111111111111111111111111111111111111111",
    signature: #"22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
  }
}

fn c20_addr_witness_2() -> MidgardAddressWitness {
  MidgardAddressWitness {
    verification_key: #"3333333333333333333333333333333333333333333333333333333333333333",
    signature: #"44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444",
  }
}

fn c20_script_native() -> MidgardVersionedScript {
  MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"8200581c55555555555555555555555555555555555555555555555555555555",
  }
}

fn c20_script_plutus() -> MidgardVersionedScript {
  MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"6666666666666666666666666666666666666666666666666666666666666666",
  }
}

fn c20_script_midgard() -> MidgardVersionedScript {
  MidgardVersionedScript {
    language: MidgardV1Script,
    script_bytes: #"777777777777777777777777777777777777777777777777",
  }
}

/// A PlutusV3 script whose one-item field-6 preimage is exactly as long as the
/// one-item field-7 preimage of `c20_addr_witness_1` (104 bytes), so the
/// cross-substitution rejections below cannot be explained by the committed
/// length gate alone.
fn c20_script_equal_length() -> MidgardVersionedScript {
  MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"ababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab",
  }
}

/// Builds an exact V1 proof source binding the given witness fields. All six
/// body fields are committed as empty collections with one-byte (`80`)
/// canonical preimages so the full nine-field bundle path stays replayable.
fn c20_witness_proof_source(
  addr_wits: List<MidgardAddressWitness>,
  script_wits: List<MidgardVersionedScript>,
  claimed_script_witnesses_length: Int,
  claimed_address_witnesses_length: Int,
) -> (ByteArray, ByteArray, ByteArray, ByteArray, ByteArray) {
  let witness_set = NativeTxWitnessSetCompact {
    addr_tx_wits_hash: bounded_collection_v1.from_items(
      7,
      list.map(addr_wits, encode_midgard_address_witness),
    ),
    script_tx_wits_hash: bounded_collection_v1.from_items(
      6,
      list.map(script_wits, encode_midgard_versioned_script),
    ),
    redeemer_tx_wits_hash: bounded_collection_v1.from_items(8, []),
  }
  let witness_set_cbor = encode_native_tx_witness_set_compact(witness_set)
  let compact = NativeTxCompact {
    ..sample_compact(),
    body: NativeTxBodyCompact {
      ..sample_compact().body,
      spend_inputs_hash: bounded_collection_v1.from_items(0, []),
      reference_inputs_hash: bounded_collection_v1.from_items(1, []),
      outputs_hash: bounded_collection_v1.from_items(2, []),
      required_observers_hash: bounded_collection_v1.from_items(3, []),
      required_signers_hash: bounded_collection_v1.from_items(4, []),
      mint_hash: bounded_collection_v1.from_items(5, []),
    },
    witness_set_hash: blake2b_256(witness_set_cbor),
  }
  let compact_cbor = encode_native_tx_compact_v1(compact)
  let lengths_cbor = encode_native_tx_field_preimage_lengths_v1(
    NativeTxFieldPreimageLengthsV1 {
      spend_inputs: 1,
      reference_inputs: 1,
      outputs: 1,
      required_observers: 1,
      required_signers: 1,
      mint: 1,
      address_witnesses: claimed_address_witnesses_length,
      script_witnesses: claimed_script_witnesses_length,
      redeemers: 1,
    },
  )
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let commitment = native_tx_proof_commitment_v1(
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
  )
  (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor)
}

test v1_c20_6_full_preimage_reveals_native_and_plutus_script_witnesses() {
  let scripts = [c20_script_native(), c20_script_plutus(), c20_script_midgard()]
  let preimage = encode_script_witness_preimage(scripts)
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    [],
    scripts,
    bytearray.length(preimage),
    1,
  )
  let decoded = decode_midgard_tx_script_witnesses_preimage_cbor(preimage)
  expect [native_item,
        plutus_item,
        midgard_item] = list.map(decoded, encode_midgard_versioned_script)
  and {
    decoded == scripts,
    bytearray.take(native_item, 2) == #"8200",
    bytearray.take(plutus_item, 2) == #"8203",
    bytearray.take(midgard_item, 3) == #"821880",
    transaction.verify_midgard_transaction_field_preimage_v1(
      tx_id,
      commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      6,
      preimage,
    ),
  }
}

test v1_c20_7_full_preimage_reveals_exact_signer_identities() {
  let witnesses = [c20_addr_witness_1(), c20_addr_witness_2()]
  let preimage = encode_address_witness_preimage(witnesses)
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    witnesses,
    [],
    1,
    bytearray.length(preimage),
  )
  expect [first,
        second] = decode_midgard_tx_address_witnesses_preimage_cbor(preimage)
  and {
    first.verification_key == #"1111111111111111111111111111111111111111111111111111111111111111",
    first.signature == #"22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
    second.verification_key == #"3333333333333333333333333333333333333333333333333333333333333333",
    second.signature == #"44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444",
    bytearray.take(preimage, 3) == #"825865",
    transaction.verify_midgard_transaction_field_preimage_v1(
      tx_id,
      commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      7,
      preimage,
    ),
  }
}

test v1_c20_6_script_preimage_presented_as_field_7_rejects() fail {
  let script_preimage = encode_script_witness_preimage(
    [c20_script_equal_length()],
  )
  let addr_preimage = encode_address_witness_preimage([c20_addr_witness_1()])
  expect bytearray.length(script_preimage) == bytearray.length(addr_preimage)
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    [c20_addr_witness_1()],
    [c20_script_equal_length()],
    bytearray.length(script_preimage),
    bytearray.length(addr_preimage),
  )
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    7,
    script_preimage,
  )
}

test v1_c20_7_address_preimage_presented_as_field_6_rejects() fail {
  let script_preimage = encode_script_witness_preimage(
    [c20_script_equal_length()],
  )
  let addr_preimage = encode_address_witness_preimage([c20_addr_witness_1()])
  expect bytearray.length(script_preimage) == bytearray.length(addr_preimage)
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    [c20_addr_witness_1()],
    [c20_script_equal_length()],
    bytearray.length(script_preimage),
    bytearray.length(addr_preimage),
  )
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    6,
    addr_preimage,
  )
}

test v1_c20_6_c20_7_bundle_reveals_all_nine_fields_exactly() {
  let witnesses = [c20_addr_witness_1(), c20_addr_witness_2()]
  let scripts = [c20_script_native(), c20_script_plutus()]
  let script_preimage = encode_script_witness_preimage(scripts)
  let addr_preimage = encode_address_witness_preimage(witnesses)
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    witnesses,
    scripts,
    bytearray.length(script_preimage),
    bytearray.length(addr_preimage),
  )
  transaction.verify_midgard_transaction_field_preimages_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    [
      #"80", #"80", #"80", #"80", #"80", #"80", script_preimage, addr_preimage,
      #"80",
    ],
  )
}

test v1_c20_6_c20_7_bundle_with_swapped_witness_fields_rejects() fail {
  let witnesses = [c20_addr_witness_1(), c20_addr_witness_2()]
  let scripts = [c20_script_native(), c20_script_plutus()]
  let script_preimage = encode_script_witness_preimage(scripts)
  let addr_preimage = encode_address_witness_preimage(witnesses)
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    witnesses,
    scripts,
    bytearray.length(script_preimage),
    bytearray.length(addr_preimage),
  )
  transaction.verify_midgard_transaction_field_preimages_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    [
      #"80", #"80", #"80", #"80", #"80", #"80", addr_preimage, script_preimage,
      #"80",
    ],
  )
}

fn c20_two_witness_source() -> (
  ByteArray,
  ByteArray,
  ByteArray,
  ByteArray,
  ByteArray,
) {
  let witnesses = [c20_addr_witness_1(), c20_addr_witness_2()]
  let scripts = [c20_script_native(), c20_script_plutus()]
  c20_witness_proof_source(
    witnesses,
    scripts,
    bytearray.length(encode_script_witness_preimage(scripts)),
    bytearray.length(encode_address_witness_preimage(witnesses)),
  )
}

test v1_c20_7_duplicate_address_witness_rejects() fail {
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_two_witness_source()
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    7,
    encode_address_witness_preimage(
      [c20_addr_witness_1(), c20_addr_witness_1()],
    ),
  )
}

test v1_c20_7_reordered_address_witnesses_reject() fail {
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_two_witness_source()
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    7,
    encode_address_witness_preimage(
      [c20_addr_witness_2(), c20_addr_witness_1()],
    ),
  )
}

test v1_c20_7_omitted_address_witness_rejects() fail {
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_two_witness_source()
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    7,
    encode_address_witness_preimage([c20_addr_witness_1()]),
  )
}

test v1_c20_6_duplicate_script_witness_rejects() fail {
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_two_witness_source()
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    6,
    encode_script_witness_preimage([c20_script_native(), c20_script_native()]),
  )
}

test v1_c20_6_reordered_script_witnesses_reject() fail {
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_two_witness_source()
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    6,
    encode_script_witness_preimage([c20_script_plutus(), c20_script_native()]),
  )
}

test v1_c20_6_omitted_script_witness_rejects() fail {
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_two_witness_source()
  transaction.verify_midgard_transaction_field_preimage_v1(
    tx_id,
    commitment,
    compact_cbor,
    witness_set_cbor,
    lengths_cbor,
    6,
    encode_script_witness_preimage([c20_script_native()]),
  )
}

// --- wrong-arity and wrong-tag adjacent item shapes ---

test v1_c20_7_vkey_witness_arity_one_rejects() fail {
  decode_midgard_tx_address_witnesses_preimage_cbor(
    #"8158238158201111111111111111111111111111111111111111111111111111111111111111",
  ) == []
}

test v1_c20_7_vkey_witness_arity_three_rejects() fail {
  decode_midgard_tx_address_witnesses_preimage_cbor(
    #"815866835820111111111111111111111111111111111111111111111111111111111111111158402222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222240",
  ) == []
}

test v1_c20_7_vkey_length_31_rejects() fail {
  decode_midgard_tx_address_witnesses_preimage_cbor(
    #"81586482581f11111111111111111111111111111111111111111111111111111111111111584022222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
  ) == []
}

test v1_c20_7_vkey_length_33_rejects() fail {
  decode_midgard_tx_address_witnesses_preimage_cbor(
    #"815866825821111111111111111111111111111111111111111111111111111111111111111111584022222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
  ) == []
}

test v1_c20_7_signature_length_63_rejects() fail {
  decode_midgard_tx_address_witnesses_preimage_cbor(
    #"8158648258201111111111111111111111111111111111111111111111111111111111111111583f222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
  ) == []
}

test v1_c20_7_signature_length_65_rejects() fail {
  decode_midgard_tx_address_witnesses_preimage_cbor(
    #"815866825820111111111111111111111111111111111111111111111111111111111111111158412222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
  ) == []
}

test v1_c20_6_script_witness_arity_one_rejects() fail {
  decode_midgard_tx_script_witnesses_preimage_cbor(#"818100") == []
}

test v1_c20_6_script_witness_arity_three_rejects() fail {
  decode_midgard_tx_script_witnesses_preimage_cbor(#"81830341aa40") == []
}

test v1_c20_6_language_tag_bijection_matches_typescript() {
  let preimage = #"8382004082034082188040"
  let decoded = decode_midgard_tx_script_witnesses_preimage_cbor(preimage)
  and {
    decoded == [
      MidgardVersionedScript {
        language: NativeCardanoScript,
        script_bytes: #"",
      },
      MidgardVersionedScript { language: PlutusV3Script, script_bytes: #"" },
      MidgardVersionedScript { language: MidgardV1Script, script_bytes: #"" },
    ],
    encode_script_witness_preimage(decoded) == preimage,
  }
}

test v1_c20_6_language_tag_one_rejects() fail {
  decode_midgard_tx_script_witnesses_preimage_cbor(#"81820140") == []
}

test v1_c20_6_language_tag_four_rejects() fail {
  decode_midgard_tx_script_witnesses_preimage_cbor(#"81820440") == []
}

test v1_c20_6_language_tag_127_rejects() fail {
  decode_midgard_tx_script_witnesses_preimage_cbor(#"8182187f40") == []
}

test v1_c20_6_language_tag_129_rejects() fail {
  decode_midgard_tx_script_witnesses_preimage_cbor(#"8182188140") == []
}

// --- canonical-form guards: matching hashes cannot authenticate a
// --- non-canonical byte representation of the same semantic items

test v1_c20_6_noncanonical_language_tag_encoding_is_not_canonical() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let canonical_preimage = encode_script_witness_preimage([script])
  let noncanonical_preimage = #"8182180343010203"
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    [],
    [script],
    bytearray.length(canonical_preimage),
    1,
  )
  let (nc_tx_id, nc_commitment, nc_compact, nc_witness_set, nc_lengths) = c20_witness_proof_source(
    [],
    [script],
    bytearray.length(noncanonical_preimage),
    1,
  )
  and {
    canonical_preimage == #"81820343010203",
    transaction.verify_midgard_transaction_field_preimage_v1(
      tx_id,
      commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      6,
      canonical_preimage,
    ),
    !transaction.verify_midgard_transaction_field_preimage_v1(
      nc_tx_id,
      nc_commitment,
      nc_compact,
      nc_witness_set,
      nc_lengths,
      6,
      noncanonical_preimage,
    ),
  }
}

test v1_c20_7_noncanonical_item_wrapper_is_not_canonical() {
  let witness = c20_addr_witness_1()
  let canonical_preimage = encode_address_witness_preimage([witness])
  let noncanonical_preimage = #"81590065"
    |> bytearray.concat(encode_midgard_address_witness(witness))
  let (tx_id, commitment, compact_cbor, witness_set_cbor, lengths_cbor) = c20_witness_proof_source(
    [witness],
    [],
    1,
    bytearray.length(canonical_preimage),
  )
  let (nc_tx_id, nc_commitment, nc_compact, nc_witness_set, nc_lengths) = c20_witness_proof_source(
    [witness],
    [],
    1,
    bytearray.length(noncanonical_preimage),
  )
  and {
    transaction.verify_midgard_transaction_field_preimage_v1(
      tx_id,
      commitment,
      compact_cbor,
      witness_set_cbor,
      lengths_cbor,
      7,
      canonical_preimage,
    ),
    !transaction.verify_midgard_transaction_field_preimage_v1(
      nc_tx_id,
      nc_commitment,
      nc_compact,
      nc_witness_set,
      nc_lengths,
      7,
      noncanonical_preimage,
    ),
  }
}

// --- accepted Cardano maximum: genuine boundary-fixture vectors ---

const c20_7_maximum_cardano_vkey_witness_count = 124

const c20_7_maximum_cardano_field_bytes = 12774

const c20_7_maximum_cardano_signed_cardano_bytes = 16351

const c20_7_adjacent_cardano_signed_cardano_bytes = 16482

const c20_7_maximum_cardano_canonical_bytes = 16685

const c20_7_maximum_cardano_transaction_id =
  #"0297901027ac0e7df5aeefe14961ca4fcebcdf27d69bc9d9ab2638ee2c86b71e"

const c20_7_maximum_cardano_transaction_commitment =
  #"c0ef3ffbbef5147e9ebc16ace943689df4f1ba1ef4521fc62af33fc8bb0d4b67"

const c20_7_maximum_cardano_collection_commitment =
  #"6368be82071b67667a46025edba8a97b1311d8f561553682bd05d001c1d4492f"

const c20_7_maximum_cardano_preimage_hash =
  #"689afcab7a4406fa8da9a4f97b325f34458bd2114d6b2ae9eb357e681acc0e97"

const c20_7_maximum_cardano_compact_cbor =
  #"84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f582010b1e0f0eedf2fd59f6e885d21a7291fbeeaa435b794343c0d44c6222886d53f1a000d594920205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820df206ad9e3c76e42f59400a5bf8ffb6d74ef4d4c82dad8b11b36df6de3db39325820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820a41a40b9671b9e0bf7705ea9fe39e4ff4162d25391a7837f17ee3c48aaf446b700"

const c20_7_maximum_cardano_witness_set_compact_cbor =
  #"8358206368be82071b67667a46025edba8a97b1311d8f561553682bd05d001c1d4492f5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"

const c20_7_maximum_cardano_field_preimage_lengths_cbor =
  #"89182701183001190e8a01011931e601"

const c20_7_maximum_cardano_address_witnesses_preimage_cbor =
  #"987c5865825820fc01d2918e4aab0ab1ccbd479e975f23acd834a3578f688c7dfbdcc2ba0a63c45840ca5ea2377ecd1bdcd087f2e5fbee6097dea28b516f675d5cb9dd92759f1a94cb615c92db73a789b37e7e55f2094ee50caac010cca090ac67c6ed8ebb9aea770f5865825820d737962b9712c607ba7e6d9def57b6646058f99cc81a207d73e0a72454144b4e58408ac1e02019925d2b8aeceb1e9246ba22d99037fcc3269f913eaeca553df5b95883d3af2fb7910fe1f2cd4c88ea0d152cdeec085e8c3647eb3cc592e4b5968d055865825820c39aba265170b6d6126b65b41ec31d16425ebc5f362f7d66fbb7a0f4f412bfc95840f0d4f64f44ed14399efc31fbde5966554049cd6e191324002fd443124d589347c60d33031799f7bc05a423af170822f9ad8c1b80c38ed5142533a17b38996b01586582582088ee9ea166f7e7f9a8106962169f1f062d13e4e7abb7b14b7ff61eb2d46cfb2c5840f8931a57b550590c58de7a80cdb424608a7597309f47b01b71c223c16c3c1029181e03c9f318125957efdd8dfa395f61d2474b1b82b4ec2ace3551b18b48a50558658258202463b23879dd9402ce8d474e18b7b2bdef680e67044055eaec93edc24926cb6a5840c6e5a40d6d4c425a97e3b81ba9f5c8bfbe06425c3154c3377c7e43bfcbd6bf012936ac24c867990eace0607b1f7d90d18b543c00a2c6d7ad1caa8894eb8fea095865825820b382bc80d05902589dd873eabb2a737600dc0c0517b386c16dc25c1fd1c7fedf5840530eeceac03820a0f42674a3e27ae1a3c9f9737c56716bfc487fc3fa89ee560688ebac632a5a887726bcb3511608b887beccdaec91750e036b86475a74c0fb025865825820013430ad1b98307f29cf35ec75abfad1a92ba5f3a0c366349c47fbfb370c003b58405bfe58f01ac9346291a4694ae3f46bf2a17b5780c77e827fa5d0f8989eec08ce701889f9212f0c89ef73cec15e9fc93fd180b9a9bcea9deb34a136ce5e3d7b07586582582047f76ada31f5f6540fc31a2509ce93d66c85a7f2d5836f34d279732e85bef66f58409d5a353d84c656340f172fe18e08858932f2664bfd9a2dab82c189d21c2828159bac6f0e10d4f257b03c9ea461d4c37526a2a09d34540a8a6a850101985b7b0d5865825820154a661fc31fa7ddbbef72d8e57ca902020d8669a19da966a224a7e26569b8545840ddc6f34d82c2efe39f995fc9725c779094f49fa00a512a3e9122226201224bd076924266bb2ff19bd448650cb8eb012aa1fd951fea4ba2a58310175a4d529c0c58658258200dfffda5de64aa5e02ad0a150f78afae5e173531dfdece8c37ec53dfd64b1f105840c94b5ad737be5f12f9689f11d82d73f02159b38694a7c13022e64ab4aad148ca0f85d9f423a7ad12e547e70f84b5ba814ca6097a07a46f422d3310c749585f04586582582008651f722579ca008eebaae70cf5c02f86483af3340df7ddf0395f3ed0550eaa58403fab1c773b4f1fd582d6e7732b77abdb53153be592274b5d51cb6854db5237df86c661d7f70109234b629caf7d9eaf7296192526b0721c9850e504499ae62c0a58658258208b04e4cc7380ccc064bad8840eb099cb4e64c6599cb7a74d0248721b751d08e658409c2c620d8bf7b67866f2697da03ac2b722297420ed7330ee26afa8bfa6dd8568961c2daf9a2276261ba785d50577b54da306d470d9440bbec2684be2f36d3f005865825820a5f56a950e3de72b1bb11ba609b13afdd01df5cd50f72bbe0fb0b5ae294241c958408b9f85efdbf78d0c82c653cd52bb4534d71b262aa40c8693b0a41d406f44d6cb88970f69a4a06fdb5be16663cdec4ef978c01173c568d631609e27ab71b6b502586582582076b37b8c183665c1697cf75179f71fc03246ae0fdb941d697c5b3f00d4a2c09a5840cde89735e8188edd7fd9ab89157a8e9b729fb214e4e540cbc546f7893798d2c156afd6ba1b67cf86df88ff40fabe016cb7d98c7335ee2b67510647969849ba0c58658258205643028ac1b82e891d28ab5263ba50ff592fab8a2c0c7148f28444dd22d51ea0584030bfe8bc092bb519ac878089cc8d2f5c5a3b2469a27ebd2017b60c9fa5b6308a63ca6ab2374272d79b3b2ceef225f4fd72216c141c266d49c0e3c33f58b3b50a5865825820c179c7037e066f70e68d568ad8eed5d26549c70a9f14b0d9bee9a467ee7c31ee5840e4486003958122d22fea796deca825b38d867f3050122a1e888dc915f1dba60d39c5a4cc79750ce286919e4a78490f6f6a0c50d827361c7e6b73fbec944b120158658258201a14407be4349104097699fedda095fa488e77956ff38131cad7278073f2a91558408399332ddab52130915b2edb2656b2551cabae8bc036ec42192879002e5fdff664a1f7949ccc60b7df429859656361852e1dd7c060bcf9a1c2528a542c510e0158658258202785caf8343db849ec5b2841521e6ca963e22cc56f599650c45f812015fefde058407d31b04085a61a0e32b081e8d259dc983b8fc89299d9c7b50dde420fa643f14049b993af8f851be5a7fed580463d3d079a05d7bc1c9855bbac44bc61bb8846065865825820870982a225839aa16e5a28477ff121d303b512cd689275f7f1b346af2eb810455840fce56d1083e20ed8e2675cb8aa7fd063efe33410d392ea8add39f6c6c67dd258938535a14690d03cdfbbec590c6b88f9ecd439225a4ba3198fd9b6c94db6200d5865825820cdff532260f696df014ffd74007b6f0af8d17766c3534648754c735304b8780c5840b4b02a2921511c97f29f867b5e9c8bc989eaed52f369ccdb6db670380caea137d525ec289968451d36d1bdac61a1802f87e978cc10f84b85be09afd99d56f7075865825820ed46f54e8a4f9006ba0b94e2888be031433592a1b00743f0f41a7f5e212460715840397a88e484134e28a3ada07cf75678b2f1e17c29af7f44ead5fb37561f5b9bd7988ad32ce08e0df49cc3d0a8a9db70220a03d2c55b2e6b152b769b100d90090e58658258200f77044763047868fe73625a2b23eeae65ce073da394591c18871dc7e40b430e5840679e4815d4e4e5575d01eadbcf2b11068c2b1cae78cbfff2e692dba179ac423a73fd022bcfea95e8ef68ccf5b6b5cad1d37c85c8b20e44ead7215d6138e80b0c58658258207e7d3e33dde9a20ddcf1ab2cf4584eb628bd901c1fbae0f794d11633f44c844d5840d353cdde94c81f6f528aae619c34a25a5d3eb24a17f5b40b05282128eef24e5ad1c8254d533d8f2367dc555f4ab2f8858090548462df40b6d6320e1034d7a9005865825820702aca718b9ccfbe20502a574db1ce4625a4df28259d6d4af62fa680b6e08ff758409ed7fbe1835769feac554e4633bf2c3ef5c46b8abbb2b9214bd50281f42f117c4e3f7acc0470e1de3fbc4248b32967d76efd78d78f674a5d5ca7e6f14d2b010f58658258205923b1d884720276e697a613cacaf3a0f2b262032f2c1ff7bcb41b45474b313d58403edb5961c679684e876baea8ec820fff377a8b0dc5e2b073dc88f0b5209a7defe15b53492929c1c107288ecafe8195c212bf102d03477909c85a988375325d025865825820291356e217c6893dd0e336c2e3a3385db4371b1ecb757eadb148d4b458bd567d58402a81d3dc04333bfbf99d3cb6d5272db66010db47fe3702b6a2f91bd2088a5da9bb7296675963423974e7e48a099368561f48ca0a5963ea3a5723fcdd7c21c80a58658258200a8e25b9f159f9bfce141d10cab1d4824e8a04ebe31a856164daf94ff1ab86315840426e3def09ba57b1364c67cbeef03bddafaabe5bf0cefd3e09bade39aafca9d84790e7057e692dbc4d5afdd54cd04e452f9f34ca34df7a0e05d330c80524f9045865825820021e748f351f42fdbae8c0f329c1477d8110f27ef0251940132bf156f60f20b158400a88a53f9325baec78fe2f2bc83bbf7afd0619294f555171e679841d684480e86f86392d079dadc78ec0cce5b6d4b17eccdcbfba50c0cc150f36a57d02f8ad0558658258202983cd742e1c8fd59bcdff7126b15fff0a104be8afc24307e37bc117cb00930058402f6c025de7c12d8787b7f02c486526b40bb0b1364d7229ad4b7658b9573c385d6fca97a6686ba39052468ae589061b207607e17cc611a7ffa848fd9a2dd207065865825820a1dde258d5f56e08e4c9623b9ecc44d23cb655fd295c7e59f2478dde705dce0a5840e29569df49500658d9b17f06e879bb7ec80b77325727a261b7995aac470548ad9377e9758ad882ff09f2884803ced7b159d9364fd85cfc16b8ad8705307904025865825820753d46e5cbfccec38a81c70150ccd72d32443bbaa77096eac5beb7f85a6ba9ac58405d282386f720ef73ed3f0cbafb6562e2c0b20e92c8c8e6b34f4e9bcc96873e458fcb65305fa6353531d2e1729fd28cc87144f5fb0eacd7ac7b79628cd10da1025865825820d402e19921bf5c10fe529b073c5e65d6c9fc468b4a23808b009f65b4c3a250d75840f30828c5281903154698345c52ca7556ab6b3924c5b5d46ea284be4a388dfeb5449f50498a12beb157d16958aa8a4f21c618213a27a5f6a9892b9fca293c86015865825820068abebe3c8715a9ddfad6d17a803b5e3dcb0dc006c5790f765faef766d3052b5840a6f613774f8f7284f2653f4057b6f19a31b7a40f0c45034f1d03412a822b0161344c18e959146045a5617bd7122c1f131585c898f2fcce94649a840074aacf08586582582002f5100581f963a13d7c275a454935abc3b86e2572dce22c07edecf850a75fa35840c09b2c96fd44679d3def856ad79e806e8c65d92335787a424468eae766d4e2b252369249bf0ff321a35de93015722879ee1e0ad3122e767bcdaf705ad00a8e0f5865825820cac6e8a7b9aa542cd5c2eb7e938b854ded26d4c447826f2520da80741fc2195258401cbdc251fde44d5dcdfbab6de5f0a42aaf9da7717f526582106b48e9e22aaad04e2c9246c897c92ad11d86acccbe701390c0b1b884afc3d9d109cdd8f44d7a0c5865825820544407d7fe3fc3147f6499d5a052f175997332a4d0a1a4515801c50cac7a0c4858404fef767409706aeed93561305faf57e862729d788d95d8c2724d9a48cc0784b7f628008e2a4f093a484e67302cf6b32ab81c452e80b30341385a8ab09d00740458658258203dc2c6533f9adfb85088927177e8d249852c0e0be6c8836893ce3d18b04790bc5840e945172eea7c8654c7275bebe1254b64f1e0fc83e2a02482b98bd235472e5cc36128ab21a477af5edfd62b7dede332f9e8b99d3a2feaf90c29f021916f8e860358658258204733bf6951d3e3d13759e1b3dd53f3b5c3ec02e24b3ae70d2364200af90aaf755840416a7c0560fa1117a05a0bbc647253559938cec4503c0a0742c8413ce4bee4a95266efff35b2d9c1617e337cfe5a0041454484d2e6a25dc8062b4bd0d80ffb025865825820553940d889de18da0048b8028cd4bf563597111c4079fbee40a9223d9d69fd485840694570d7873e3a7c965c9deda419957f5e40040e847910173f610aee409c92051509f2d2975e62ae237cee40f98fe94e5e4818731cc63a8d893ca245ea99ed0858658258209acf3388ff6f4c6703d8a6b602feeaea853be12bc2b32c31c07e2c0a93b5dff65840a2b4b9bc6bcaae7831249d51712e5e72350a1e50cd63255eafa02473bf0ffa7bcbf438e55a3f5e582b72329c3b0d8b388432e0c561805c751de76987186fe809586582582099c183ef047a0f8caa2a911ea557a625c7d11fef03a43b4586ec74e9357d4bed5840fd0d049786060c05bd21fbcf46ede23ec0dfb6e5245db0925bc4ab43f4723a423e078c9e2771bb2574a9d952ea09068360830c654cfc175fa131ed80cf705e065865825820fb1c623c04cb6383063f308c24820feece11d772f92dd1c0ec12e4cf8ca759a35840b75e4971125060a0f6f58610797c0be42cbbe94772d743932175dc860d78f62384d367924b1780ed7fb2708fb2185b08290f56b65058d2d034e07cda845bb607586582582062f79048cd48a949bfd28e5a62193573c7ddd20a537116da0ecddba217011b4c5840cbb57882f4e4b861dd18870fe285056694d8d8ea073534f9f646589a0e96b1f0a989f658fe5bd1ef226eae1a7f69f6d4bb048c2fc7eb138c10db867782803a035865825820099fab47694196e9597191b497bf115ac03d6a0a66a75a57c0555903671eb047584097582f6f1bdf5c4d382d521759c49ba0bd170274d381c791eea1870b7449e4b517bc8291fc6240f81aa533f83d822b50661933e17e7aa052af364b736651250f58658258200f647ac72b940b22fda2b8dd7a0e6fae196d995c9ac23a154d83fdb17903c62d58405f04d46802b65d0a50074d52a5f2226aa6a0cf69a5383d1571a3eae8df285558017668c8298419593b78ed0a1e26896fc552f66685d136ab3d3bdde553565d0a5865825820d44f1f5961a1ac1d9bd41ccc831fb57b54889ee5e5b9f645eb92374dd97980185840aaaa6994ba3e5b7708dfbb2f06787176fc4823f46112b08644e92d259fdfb0f61185e65a9eb78b8a1b93dcd67b6b0759d50b436d416a821d3b15e4ac644d28025865825820acc17efd4c45a788448f1243c657acf8dffb4efd8ddb67b6a5dcc9fd5b930a1958404e0f3d94b8b83826f22264beff5a133725df7a3e99430e0a16a87a437db2622e15600b25b5e0b8d6d0e9710e76301178d2387f420eb274c1e4e23813a37b560c5865825820137e0f1ad42af0af976455f2ea4ca8e3af8d90c4b1067c5a64c649752de886785840baa745d50caf0c28f1fe63ccc68bbf220cbfb5bd95ccbe678f409d6d764411db3759654cfb1715f72830ecf0c3883ce1805cc5be2da557602b22286258b663055865825820926e0dfca457a20173214a0c8fd3dbae6381adb3ae185ab1eda17bdf49f9dc6758403efcb76766179734841fe2fd3340d68d43fb054149a904d51105e8c8cc759efaf167b1a89ca871b8eba55ef9f1785c80b30bcf299f9e05c6c5a46d948e50190f58658258202d8f18c019304cd260252a097d6ba7ef79ccd5ed0528a7c2303bc91298b186555840590347969ecce656ea8f9a3952edb7953c393d93f341e7706e754556bb43c59b71b2ed402d669bb0d19066a88805737559c10299bf6875aa863ddf0831b5ec0558658258207f5d6e52ba7c2410ec3aedf16af0bcd7b9cf254147d2a2e8ba8ecddf9963af24584091cbd60785a6013f370e47ed270c33d11978a63880d4732ee28255639a6d433844b395b65a793c9b217631286d1bd8a16b65ee2d100d22088efe846917f13c0f5865825820ff45cc6c6420800a84bf8006498f168716e3fb27a3913fe1591eb2086c952d2158401e4cbe9b6a1ca06a9f92361b2b12022a810bc43d647118681453412db8038e05b8fe83327557870a4faa43d00a42c88290c9b6b2050fd5c4397a25953b8b16085865825820c47ae173cb3c30ec56765c2151b582ad7e1767350781cb48b347f3bb221568dc5840a188f512241c70b3636fbae0e25de3f2354139c317743516e76712238529a790b1c8ffa5cab9e1cb1ce8a05aeb5dc13144224a3f681c3f3e6785099671c4a7025865825820e5fe440b81ffc3579b1edb3eb936408e93a962918c3266f58e2527741b98b76d5840461e321d595c304d124a4a1eb1d1a12712f1a7ad5f316de8bafdf5e1c8ef5eb94516bd6c6919d768bfbe3f4802645c595dad2b962495989a2eed8b10ae9fcc0958658258209ced6b26606dd84588125508ae64cf0e594a31e40b95f6caece2e0d9dfd4325358402d2a02c5fc0bce7caa06ec209f697c1a1c1699bc38c617c934307c71351ac04bcb7ba66e1a0d757f9460b3dbbfac56bc5e755fbd6fa01957d97970319e18590458658258206049ecbbc934708fbe74c3512b40ff0521a6e47ffaa0426d6db58a88392afcd15840e888d928570fc8b2a5700de6ade839cbe9ca201a351102f6433efd5977d9fca5728518e954b84b9772813862e47db1f7e534802974c56fe9f58dbcc7c3acbd0558658258202c4ea63274adb84153c9fa44ed7708b6499c2f20e6cfd5131cd13f390638ed405840a21c0f3903bf43f2c9386a48ca0713a3b39902adb057fce5778c8334d576e5a4e1a8c0f068d8d7ca1e7bfaa34e2105ac89ee0dcac3f0c6fc2a10a568d05873075865825820b14a65b2d8899bf9c2434e1580b8114e4ac2fc5ece85134579a84d83312838125840e3cf2e151f0cf27fc2099e5c2607a917592a4dd4ddb11e44781ed453c7ffd7ce1f83112d4435ff85a9924f8ffc4807f2ad4e7982413feb4568b8528d4f62b3045865825820a62bd0b56f9ce45234679f7ae67439ebe5ea30b61a29c134958ed5c4121992985840d2c95dc308880e1883d76264cbf30bc2c0304e8c1782a9c21fecad36844f054531b9077d0dec4111fa2d7e2430030b1e785aaf1bbccabd7b95205819eb0a6a025865825820e16e8abf8a8a16d207ad2e6392016a21b7e4dabe1ac9599a5db6ac667d5057f65840b02c488425cfc9e22ce66a2575af8b7fc1f589dbf5f5d4c518be39204d0d89df6d9dda4b39e5b07cac0348b67879a4c15b77443a3f9bf3f7e0e5e2c62f061504586582582022ac941c8cae642aa72a339f49d33103527ea5aa34ef485e085da24e12d539d258405a77e9fc9097bd71cd77d417332ca927192eab918543a6ce5e8ea9affaaf218c8031fc30b5ae7c19d4d8f0c47ecd619fdf6da97d4fff21e2b67f7a2f93820f0b5865825820467c56b5458e49c886ed7e82a4fd5e7782b63d96fded0cc4f9ce4ba68c5f1e045840fc5cd27a95a54a13206ee3a8a95c2d350eec62856135eb85e2c824272c84b91a4d084ed969963580dd0da3a8e3de5c74d9bd419b433f347325414c2c9b4a16055865825820dd30377deb99072c9a29f550af32344ec37c172a77dd0047956efc02510c98e15840635f3c29ad4e1ab3dc6489eaba783c70d9d10477d6ec3617b23052d3f4fb90dbeca2194b90ff1d56e03933029dbe27267636a9a150131af66197fd904139f30558658258206a8bb4076e635d7a035ac77d261b26ef2f1eddd9009cdea16958e486de1585cf5840f53dc540e7a67ad172d520b88bf601aa0ae16d334fabd537ba9e10e9dce6daa51a585f52db489195019127d94418473dadc3e0fbe875d2e68fa7e77f6a17bc065865825820b33b1b66bd06ae15321a938b1786cf91e10ebc95beeeb0675a54736700435356584072a37dfbcd43bac89ed1297206ec26d3b633aea62b1a97d746e5401899b2e0f67cd5d186d1f0862548ccb71b22d895385cdfb809072115b8ea77c8a57d8b6c0e5865825820efd619acd5b2d09346c099ab351246eb6403d58fc50f8ed0b0cd931d1917cda2584098419eaa6448b451e4e5bc77c68eed69ec57f152d56cd829ef0c6b680c319c87664bdc88fdcd1a7819113ee85f9715d340e4fd77d2d88e0eb87a51d00577f80b5865825820a9b9a07349357704c64b4ced3520f8423911b937d12b5d570e3d57d85b4a0c8c5840b634a81c17502ff076fba4ad5aa638e10ec4ffb00ee06f61bb3cd2947e1165df6d0b3a592f7a4feacad12a4a612d9dfa7e37fc168d2c7ad0ae26d5346ddfbf045865825820e29651a64f7a4ee5cb53420f2c4364b9c3929e2482e3a317945b720de9622866584091f2e84da54b4279d9e2808ed26ccbe0bbf900bfcc6549266e09294aa96c87e86289bf80f7273aebd7dcda866956036385abd7516af9bc4411c040466f190d0e5865825820a67a3d056eda628654bd12c630366fc49babde3889303c9f29355aaa20ed947f58405a77b98a7770269759d84e824784d5c7baa4e29f3e8b947593ba6d3d8f648a7b6b16f3b52a24474bcb823b25c150336325313b6c44fbdaab8a2ffca54b7b510a5865825820449df448848440466f0971962d09e6ac45bb3e9da6d256db0defd8d89ffbc1cb5840d5d6bd6722c89de242a581ef935f59724efd58ae6f5de28f5e23b553b603045ece75b4d840e5c84622726d4524e7dc803d804791d8560758bcbb212dd5fd92045865825820547706a60a3aac6071bd08667f96d730c446310fe5ede1b69ad8cf7482057aa85840b0a0104a53a50fde9939704a10f67cabfbfa83e0ef03df782894b8bc2f7afcc049a6fe6cc91dd245d812602a79f1f37848c73aa88272a877ad54bb2c78b403035865825820e5ebe10e48e30a4555cbbf1eaecb02380780cbc48c505e9921761782b19331b4584095e7ebe77d9b301a9dfc51a5ff89bd620001ed6afb083e8f406d41b751b5a482e63453ecea531f08661ee2da53aff6cecab9701d470266b44960bbd23744d4005865825820d3570c513fc13010e95d78b65dc119c0d244f80c2d305acb669be3301deaa36d5840ff9ce72cb5c2f357d7ae590f0368aefe52c644b113e37c8780facdd5860143a38d695ab16c9ab79bde0d4ba2f28ab7569a4e58b0d2eb5023899b0d1969fb580b5865825820527d2f07c34a0e00c4525ca74c9bc655b7de9b0677dc3c7cddbb144c0dee38e858409792a2c9a0a580d79cfc169094fcacddb93023f79af27ae18507e51ebfcffc6cc273e6df70da310771c072b2b6b49b0c3503c94a6f991c20ba29f4f9e4c3ef0a5865825820119c6ed3a3d5c2ebc7707bc22a353c46f2a75efca522ce1ec5f0eea02c2be5bc5840a122ff74fdd18a9ba74c748d8365d92ffe539f58bdbb05492fef0c927a1ededa2ce9159c4f34ece18207a5fcb27befcb9ea519f11228a98690845f9f5562580158658258209a558ce7d14d42a3f619298e7e905a6e744c3730d5eeb7724622dae8fef764395840698e971ec428c5d9b8fe29da39d7eea90ad1b0670ee1d7e714696d03e99d3463bcfb6bb30ea5ee64f4741c7762f335df5dc95f9e8862cb9c560b9b9829fc590658658258201fe9f8ffdab81665de989cf86241ce5e0e3026e605eae877ad77df8cbc3b20f258402da9aa840939f54e3dc22f3298daec77c80ca0ba0f3ac56956792cef205a47b6911c46f7c97a7f440c0b639dab14fb8e8409e65c5c1a4f5205bd69ae11bd9a0c5865825820ddbd8e74123bdee77634c8481c8879eeba45630063e65d4bccf21376c4667e71584083b6af9527efaf6889810290ec6cc7559db67fb5b0808a0cea0b65c5887360932f59f77cd2ce538d6af682f6c0139d268357df7dd132516bb2746ce5e400420758658258207fb04ec907a0c4ff9e02df558488697c9335236f1e939c4e00ebe978bf7c36ce5840cf5c80f181d19b64521cdd3875a66c0c8e7e89bf135b0fcccbc13ae76b3f8af7b591195ac40bee11ebcb34553276d523caab0d5e533a3c774310d6469658820f5865825820847c27cefd0a7c4e4703dfa7edb69610c8b2cd1511d3c7fbb3c7474b5564c9b55840bf69ca916c88b540139b32e5ad788d1d80361a75ecceb5057c8768afa33ba95b182e7f74c313c3bfba64e4243b42803cae09e6bab9715218ba48d7875076f80758658258200ef0aa58323894172928d5ddd1c7933ee6e1e4ec30f75d9fbbe9ebceec2e45fe5840c5f51694accb1f41e12eccdea1fcf5361af18323b9e35e34f6106a64771ace625226b018a1dbda4bb367c63832cdd0f084079c6c5e4ee4af630689c8b83d8e0a5865825820bbf3b07c64a44f9556a90918d0a3c363485334dbac9b7929919205f2b1f8a1f458408c25ea2d40ac26beee3d6e748803d365c75d95758a7f6b3e987b0d569c255bfbaa275c9b15511d50244e39bd1d01e5addd5df9bbcf035c8fdb3c0fb1c204b90a5865825820184867c552ef912066120d8c96ab430f37ca7dfc6447e390bb0836425b26e3915840d28a2753800af3cf4462a7a2331eaf4be67622007d448233c0533a45f9d765b62bf63d7ee7f5766250dea9c183d576594e5488d385d718203131b73bc20d9f0d586582582005b01d3e78c81e3a2e81a76a595ef8a3c9d0f216dc0db1a4bd7790419672801658403a85a6d972c9eb69ca7e3208c90c690785de5261e1a3839662b11fff587386bb9c74fb69622b2b486225a4f9b85bc4e630c3b3ae76de7a146f6cb6b3e3ce570a5865825820d18bf72e4c190c968fe2afa2d2df72636a73d4021aa5ecdc23e8efd1b119630b58409cabb3b1184dd7cc55f40d94e0bfa52b38f6e33eeea4add07c8c722569cd929cd2f5126ebfc271805f5adecf2e491387023a52a92f960d54eca1b5eb3a0bb10b5865825820b8224ac22824f95b6430bc1052acee4f418233e371bb2fc14408957e73c5de135840db82b0ae69c3fe038672376bf99ece8ad9fb50055d5c136d3786d51f92d2726bc5b99ebe4ffa676f075089b38f54cb99dee126293a7acd86df6d40d72acbbe0558658258202f5124752371950bc268ff097717dd27f6efd7aec12ae37ba09ddf285f8ad810584051dca0a33b027ffab2e3701eca228a33a7987f99b06a39345a30cd41e68ec994a5c1bd399772c7536e3ea10859328c35ac8ea55b90537b609bc568615603950c58658258206eececc797e67e0261d5b6c0e4c1f8c00749f0fcc85129c18b5bfb1c9014d6d15840d515b25f1f9bdeca149cc585cf314d978f9f029010a6ba7dcd5055b9a479cc088631e91800bcd92e4e0376dfc28d13ee2f156a67241aaf5df22ed995df51950f5865825820e89123778725cae4097ddcc895360870805973d779c53f3a061bd794497e800b5840b2291f1c54c60e6f1149887ad85562e41d41f3affdaa2b51ff14cb832e3279cc52ff0175bcc2825de9f87742be0bbf6a461c20b43b7fd8f6304411fa4f16680e5865825820312790df3126ad77ffe4d0fadf5fb2ec0a6a33f63594c69dea923e141148ba3d58401ac0fc96279a3d7b8f7ed397495ce7706f0c493c41efcccc7e2301e117ebc504a5783a43f344539a1dc4be66406e1af8d37b876d13e6f0e83cd6f1c14303840f586582582064fa0a1cda099745ae7e830f94ce94628638a1fdc90df26ff356b71f2930bff15840ea655b8a5cac76ea3a431633d67f6b394dd79deb78184e04dedc22e2f31aa63fbe94b68a63a168f39524158c8039bc743aeda2b60001282970f223173d65f20e5865825820f7e7a9cfe8c404bb3a5ceaabe56e579f62bc91e518b3b9417d13a03d91db026e58406d39bad30c83ebecfc06b7c69a109b58631fe4cbbc2acea0b50d8d1ebf549560f26000b3a494c8ff2774a41313064ef9640746c162f9d7874cdb556247745d04586582582067354d94b297b9415d4232713fd2ecf06377675b00016d94a05addeecb25052d58401d785575c39443a1472639aaa1925f2f62c2d477be0b082e277947d6302b6f9d69aa18ccecff0d7b8b1a5443d3a44ee88a059bd7ed99d29a5382ec2c417211035865825820333533982bdea65af14cc469efc3796ca8a40e697f9236a2d0a151d2bc4628605840995a31028abd3ebc37133503be0c4334f2804c6c5346b9b95aae14cc7df1c71b59e6627bf273ecbf3ef344fc9833a9d532c18ee806ff8cc6ba5575a2fed5de055865825820816dcf7ccd7445e4b6f8bd802a369f6ad9f836b84d9a2cbb90adaaa7a80ed0bf5840d1c8a504eb4fa4c1f295d8fd5068d71f172d7f069425e524036cb4a527d61620fc59111a3899da053fbfeff509fc827beb9a971042c76b1f648980615af1010558658258206107ccf054cf7d2a8370bcd92324f8c5aa97eafc6d4eca3fa99d0a9cf1a14ea25840f4779a599aae5fa61f1930ddc4cba9898a97d9aaa491f5e9aa1147baa2616ed8cbf1f6b41e300c34084a5e8731c7cf6148583ef2c90503c7d154c79165e1dd095865825820edd9fe1bae4ff9ff3ad09f75c38eb78b1c7d0df57f08dfc5bb77f4de0e23058658406570499e2c2d03bfc297d50ada9f5179fecaf8bb03ea1bb928f9fbb9c47a380b481352e84ff9b15ae341c1e3d5c28be3f43604e3f673045ee9858289662b5706586582582016059ac068cd4cdffab61f1cae092b70d16efef55bcb8af3bcd2e36f648d2a475840228ea9d40a33c6e2c012d1dc5e2c2a6b951a6fd2d715d497d845c03f1072079c3b66938387f07d3c9ff3f87ebc9487d73acf3fc09c7ef679573ebebc8b02370958658258205c08f541ab076a7035b1d93d9b4ad8e663dda5ad9f1f1f789cd5fa2bdd344cbb584005d18f1fd232e0fbfa34b65c7913a16799faba287e0bf1895eccf65707f460161aab24ae204c14319cae03ede930c1cddb7263d8a1f1d68b1cf5c19a5caa57085865825820d450bda792ed5091e62e0d0efb27a232de5821c823c29bd2711cf0b25786dbb4584036d8ce4b1192dec05dbfbf94bbb640511588a968c67505ac07b097d83141a8ad4af812b3c5a06e15051dfac7571b9886884845d50a827356a1da248a0aa9ef0a58658258203104028ec79794a1acb3a4a1b9a90ca54a82d3190caa2aa1c173a56643322fb558405f9d0e9c1cc34e2ac135f448e03c710f3986a5e12d00334c828db3781a0361e825cfe17993cb08d4b878c3f6aacec5f9c91f4bcef35d702b17f9a45bca02f40758658258202c36ae0b329ca7c168458fe470a74ce04ad7907771208d3bcc049894933ce4af584087248e151e516bc225e2083f812654a6f3faea0b74d030a2d2b0793383ef263b38019384f4f1a4f3ec30301995904b573858392ccaecbdbb1496ec99e870c00d58658258204fb9eae85cbb9af99d2fc97a090651233515c2d7c0d3a11624e231f5899e69e15840e679940edb2cbb5416ec8d804f3e3c26656ff5e8cc77d0bd1a17d987f161ae7bc7630e293a2dc5d778e25367ae26b424b998668aa47cdc65dc249a2dd18014075865825820c30500ed08d79ae3d74b3f78a48996c403b4bc652d605c9301cf653221a5078b58407e3ab142801612fe8d7f0793e8018d08b9f0e6bb81aca8acb38941c4581a459e640e67a8b80d0dd2e74ee8711f1ec99e8817989dee3d1e3e0a47c2a836a7710f58658258206345cbbcaaf3baba4672eff86404bdd7a71c6d51d9c486a30138bbe4c669977658408ab12f9a8b17b1483375708fa653a3085c325fb6509df805b8506ffbd57efbdc6c43017b11cc02bc355c613f99208088ffd03c71a3f3bddb579b74afcea61d0b5865825820d8183c2a4d64fcd22161060e1599948ca47462a7f3628055be5b072133b3649558404f29024038b299ed2f369fd290ad0b31db249a00b2507293bd1a1a497d3fda347fca7bb2690779444610320d83badf9fdc17746e5183b5352330c0050908e60d5865825820871cdfd42b43ead4d3f233bb8d725693a435212017655d66ba57344ac64f27a1584029b2c714751c15bdadabc173306877b8c25994c981c25873681848fdfa1ee3aa4fa378031757680dccfc5eeabc75430a5eab7c79336219272230ee87906cfd0b5865825820b309b6cc2943825e876b36ec7de9c32583842b82b84517ece1a38d84da7663a458405ef6704e2fd5771b109c83a93ad2cee4c305d4580d8e0a0c4a64d267266c25d3f7640b25327b1e4c8c703ee1876787d5e3dcd6fcd939492c7f01d6a0509606065865825820f323785c38344005a2d259c393116a0b1295328e78da9e665aba83db749493275840b08faf4059650889557ed1c20af897a64e82e104c72bbf7eba01aa1810393e6a409f90b0322ff64428a8dc33e8e4a4f28a8d0a22ed0a77bf6f6a603f612657015865825820b65d2fa6acdab4dd8919933fcf30a6534bf5035fb25afc435e6c17490ad95cb358405b3ade6136da5fe2598b6f563de3ca76f2a322ffa1acb43bf3140aafa4426df4496153b92bc41e3c6e20dae21e04bb5bb591134ea919b6072f3a8e6bda9823005865825820842de8a459515caba441bebb58cec290c978a7aa567ef4e55c158c5caee5081958407a53e806efab12a26ee494e16ade66d0e105779681f57742a4438c65d95cdb60c3c969c123d052e2b7f893179180db8fb14da4d5b65cb67c95132b97a6e04c0c5865825820e75709f09beb7f618f2bef8f1a339d0eda0d42fd07372f97bcb3403a0e99547358406fb616b0a213505d96676160e97732c5ca5b13f17c5a18903fd3a658535741d82f97179402577007e3558dc87ce96d1f24d89a96e97165f9725d8f0812d684075865825820ce8dafe26b21ee015ae1e775054d3084e5020a92069833ca1b08a04af4a7f229584066d1500a902cdf1f039be57ef9fe516edc406cbababf40603531bef092ed75a39e86f106f04d0b8f21b4c77b84af5321737792adceb18e7d8f23f0a925bddd0c5865825820168536a034cbab490a2032178667b14586112739073d6ac5f863a4f02baceaac5840117c39073ff17865d53555fd697de86509142d1794248be6e28b25ab6382a8846986bc8b1baa8681d7684551ed8417eeb2559c30ed289891056a4870f2e63801586582582075b263a6e900a41d6af3e256d8c44bee8fa0ab4f2580732b4d4f474b338ba0235840046e4683d5b110772ef64c6a331a20bda3b8c00fed73e5fa8ec131fc47c97f271f76140d7554f45205b186a9476728cb3c8cf5750eab5dfa513428878bf8320558658258209732d69d24b7b0de23910853dcfa09756e5b1579add31dccff1809fbda6690eb5840916e9a991ef79765d75514a8495e21845fe71645bc4954eb0a7fb9943f62234cfb91b873e74d24be19fad0fce163b2c23bf5f073943a699ef3818f8b221a9d0c5865825820b0751e519f03588fb42f6f05609469de616b588717bc4d131c09e2b1a2843ee2584028214e4732eeb68fb516038e5f0e25f9ccf2a904cac33b58224eba7b7e945c90a9f904e49928ce5423757246ba07408749973ef211a79dd0159b2e816e67c705586582582061faa11fb32645dabb8c2d41c333912592de24c586574d5e824633eab2188c275840c4dc4517894b71e87c553c96e64bec2411717878a17418e3194f5446aea7d8ebb622e512d4040bbde7abd7477fe68fd139fc5aa0bb0705677f96c68e404b9d06586582582088b6c3fb3a588dd7fb6502deaeeb362cb18f198006aa965f2e1aafbfa65a9e935840f0910edf330fa253f3f69cd6a6a887d889d9be055f12006a836b5f20def974b291e22b3a51e369fd98cbd40feac5b042f3a9927afce649a5115ea6f34f5a550258658258208f7c489e3a9e1e9f8107a3972d6f8600cb3016fb7879c9bc32fae25a2e4bab8458408f2f0b94aa9f3500935683eeaa9c86ba3938a79ce315f82e4b485a8b3a6f190c476982653b6ad0f70345d673bcab7fe94c53836d14cf394d0caec39231d93e0e5865825820f08761bf79577fef7a7322ff93654382c7e04af48930ffb6439914c431f884d158406336c41c679228dd66c87b66df6857b5143b782d5999355b04da1bdb102f2bb39c39d40182cd32e7f49074a150baf42b99dd71b6808e0196ad75c93653d24f0f5865825820713496beb58eba0eb105d9c65b13220db4572db85ce7ec74a2ac3dc685ee98c7584094e115a4179f6fdea3442c5aa04fc6f709c0bb05d7731b516e5a8f03e79a9ebd82af4aee6512b0f74287a161b42fb5eb54486f4f706ba439204c4b589af9e30958658258209e5b817e0f8df210b46b1b7045514ce405d173aa31ece60a00238d8f503992f6584037e1a7bfa1f0a70b89c927f77332c3ec98ed3898abd12983c9cdf8495b80edf10be518f81fa15f1e93e5090d2f275922d7d05cd5fc4749e9276cba5a495db3045865825820fa14eba7e0bec653a0fcdb28a961b7649a7772b28665d83ffc111d8a38cef01758405e681b4fd18d9d570bc6d5df3fd4d0bb14876a753cba9fd09488a1aacf830536e228ca2c09ed0b5015528713b88a5c5f994412ee141e552cfdd7b94cbe14cb02"

const c20_6_maximum_cardano_native_script_witness_count = 224

const c20_6_maximum_cardano_field_bytes = 9858

const c20_6_maximum_cardano_signed_cardano_bytes = 16338

const c20_6_adjacent_cardano_signed_cardano_bytes = 16410

const c20_6_maximum_cardano_canonical_bytes = 16871

const c20_6_maximum_cardano_transaction_id =
  #"9e6740ff958462051886f6d64a5fb7d03ee47dc50bd627c5765734f1c32f4bde"

const c20_6_maximum_cardano_transaction_commitment =
  #"7c0bebc279806e82bfdda06d46c06e6917e132e13325e48d2f3eb5ad7b7509da"

const c20_6_maximum_cardano_collection_commitment =
  #"ad4dcd868783831d5bd321d25528c3295f55b1ea6c8d61d85c3216be9a73ea3d"

const c20_6_maximum_cardano_preimage_hash =
  #"149831e894a70eabc4f171af54b47bb7f67e1530f782272a610d874a04f3b406"

const c20_6_maximum_cardano_compact_cbor =
  #"84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f5820598bbaa08e9cc6dc4d9634b23089ead14091f45c5e1165dcf8a6288be95a1b001a000d570d201927105820e127f848e4bda8c1e9b42ddf4c89dfbd1479301dd90551baeff900fdfcec2e975820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58205c66a9cb2310e13ca74d861c7d29f5b996030ad755920f1dfc2c325ec3cb015d00"

const c20_6_maximum_cardano_witness_set_compact_cbor =
  #"8358209c3c9f949b41759fc4d9ea024e36e2aa7659f3d5dbe41611256f4dfc80a9a62d5820ad4dcd868783831d5bd321d25528c3295f55b1ea6c8d61d85c3216be9a73ea3d5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3"

const c20_6_maximum_cardano_field_preimage_lengths_cbor =
  #"89182701182c191a420101192682186801"

const c20_6_maximum_cardano_script_witnesses_preimage_cbor =
  #"98e0820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ecc820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef7820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e77820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e81820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e7d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e7e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e47820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e2a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e49820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee2820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e82820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e4d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e8a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e92820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e21820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e38820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e95820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ebe820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea0820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea4820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e53820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e50820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee5820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e91820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec0820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e8d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eef820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e3e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e51820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec2820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e9c820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e27820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e86820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee3820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e7b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee0820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e28820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e24820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e44820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e99820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ecd820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194efb820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e9e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee8820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e59820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eec820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e2f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef8820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e87820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec5820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e62820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e79820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e8f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea9820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e36820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e6e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e2e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e64820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e97820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e96820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ebf820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e32820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e65820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec3820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e66820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e9a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e3c820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194edc820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef0820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea5820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e20820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eac820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e6f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ebd820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eaa820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ede820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e35820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ecf820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec7820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef9820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eda820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e8e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e40820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e5d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e52820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e31820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e93820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb8820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194efe820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eab820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e6a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e80820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e4f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e78820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e4c820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e75820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e5e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e33820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e68820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e26820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e73820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eee820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e6d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e45820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb1820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef5820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e71820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194efc820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e39820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee6820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb7820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e22820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e98820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e37820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea2820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194efd820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e30820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e8b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e72820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e61820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e7c820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e3f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e6b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e8c820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ecb820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ece820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e5b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e3b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194edf820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea8820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e70820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed3820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed8820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e85820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eed820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eae820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eba820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ebc820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e4e820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb0820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb5820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb3820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e63820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e88820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea7820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e5c820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e4a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e84820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed2820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e2b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e3d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194efa820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e94820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb2820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec1820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef1820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e5a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea6820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed9820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e46820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e2c820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec6820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e5f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec4820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e3a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef3820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed5820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e4b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194edb820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e69820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ebb820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea1820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb6820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed1820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e9f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e83820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eea820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef2820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee4820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed7820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e29820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e57820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec8820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e90820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e43820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed0820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194edd820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef6820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e48820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ef4820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed4820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eca820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e9b820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb9820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e58820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eeb820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e55820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ec9820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e76820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e67820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e23820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e2d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ea3820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e42820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e7f820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e56820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eff820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e89820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e9d820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e34820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e41820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee7820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e74820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ed6820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eaf820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e54820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e7a820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e60820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ead820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194eb4820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e25820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee9820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194ee1820058288201828200581cfcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d8205194e6c"

/// The deterministic boundary signer hash committed by every native-script
/// witness of the field-6 maximum fixture.
const c20_6_maximum_cardano_signer_hash =
  #"fcd3d823b8e5a45e68b4780be80ab233fe78ee6b73e8edf49872173d"

fn c20_synthetic_vkey_witnesses(
  index: Int,
  count: Int,
) -> List<MidgardAddressWitness> {
  if index >= count {
    []
  } else {
    [
      MidgardAddressWitness {
        verification_key: bytearray.concat(
          #"c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7",
          bytearray.from_int_big_endian(index, 4),
        ),
        signature: bytearray.concat(
          #"c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7",
          bytearray.from_int_big_endian(index, 4),
        ),
      },
      ..c20_synthetic_vkey_witnesses(index + 1, count)
    ]
  }
}

fn c20_synthetic_native_scripts(
  index: Int,
  count: Int,
) -> List<MidgardVersionedScript> {
  if index >= count {
    []
  } else {
    [
      MidgardVersionedScript {
        language: NativeCardanoScript,
        script_bytes: #"8201828200581c"
          |> bytearray.concat(c20_6_maximum_cardano_signer_hash)
          |> bytearray.concat(#"8205")
          |> bytearray.concat(cbor.serialise(20000 + index)),
      },
      ..c20_synthetic_native_scripts(index + 1, count)
    ]
  }
}

fn all_vkey_witness_shapes_are_exact(
  witnesses: List<MidgardAddressWitness>,
) -> Bool {
  list.all(
    witnesses,
    fn(witness) {
      and {
        bytearray.length(witness.verification_key) == 32,
        bytearray.length(witness.signature) == 64,
      }
    },
  )
}

test v1_c20_7_accepted_cardano_maximum_matches_typescript_boundary_fixture() {
  let preimage = c20_7_maximum_cardano_address_witnesses_preimage_cbor
  let decoded = decode_midgard_tx_address_witnesses_preimage_cbor(preimage)
  let item_cbors = list.map(decoded, encode_midgard_address_witness)
  let witness_set = decode_native_tx_witness_set_compact(
    c20_7_maximum_cardano_witness_set_compact_cbor,
  )
  let verified = verify_native_tx_compact_cbor_v1(
    c20_7_maximum_cardano_transaction_id,
    c20_7_maximum_cardano_compact_cbor,
  )
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    c20_7_maximum_cardano_field_preimage_lengths_cbor,
  )
  expect Some(first) = list.at(decoded, 0)
  expect Some(last) = list.last(decoded)
  and {
    list.length(decoded) == c20_7_maximum_cardano_vkey_witness_count,
    all_vkey_witness_shapes_are_exact(decoded),
    first.verification_key == #"fc01d2918e4aab0ab1ccbd479e975f23acd834a3578f688c7dfbdcc2ba0a63c4",
    last.verification_key == #"fa14eba7e0bec653a0fcdb28a961b7649a7772b28665d83ffc111d8a38cef017",
    list.all(item_cbors, fn(item) { bytearray.length(item) == 101 }),
    bytearray.length(preimage) == c20_7_maximum_cardano_field_bytes,
    lengths.address_witnesses == c20_7_maximum_cardano_field_bytes,
    blake2b_256(preimage) == c20_7_maximum_cardano_preimage_hash,
    bounded_collection_v1.from_items(7, item_cbors) == c20_7_maximum_cardano_collection_commitment,
    witness_set.addr_tx_wits_hash == c20_7_maximum_cardano_collection_commitment,
    encode_address_witness_preimage(decoded) == preimage,
    native_tx_canonical_size_v1(verified.tx_compact, lengths) == c20_7_maximum_cardano_canonical_bytes,
    transaction.verify_midgard_transaction_field_preimage_v1(
      c20_7_maximum_cardano_transaction_id,
      c20_7_maximum_cardano_transaction_commitment,
      c20_7_maximum_cardano_compact_cbor,
      c20_7_maximum_cardano_witness_set_compact_cbor,
      c20_7_maximum_cardano_field_preimage_lengths_cbor,
      7,
      preimage,
    ),
  }
}

test v1_c20_7_adjacent_witness_count_exceeds_the_cardano_envelope() {
  let accepted = c20_synthetic_vkey_witnesses(
    0,
    c20_7_maximum_cardano_vkey_witness_count,
  )
  let adjacent = c20_synthetic_vkey_witnesses(
    0,
    c20_7_maximum_cardano_vkey_witness_count + 1,
  )
  and {
    bytearray.length(encode_address_witness_preimage(accepted)) == c20_7_maximum_cardano_field_bytes,
    bytearray.length(encode_address_witness_preimage(adjacent)) == c20_7_maximum_cardano_field_bytes + 103,
    c20_7_maximum_cardano_signed_cardano_bytes <= cardano_max_transaction_bytes,
    c20_7_adjacent_cardano_signed_cardano_bytes > cardano_max_transaction_bytes,
  }
}

test v1_c20_7_maximum_fixture_witness_preimage_rejects_at_signer_position() fail {
  transaction.verify_midgard_transaction_field_preimage_v1(
    c20_7_maximum_cardano_transaction_id,
    c20_7_maximum_cardano_transaction_commitment,
    c20_7_maximum_cardano_compact_cbor,
    c20_7_maximum_cardano_witness_set_compact_cbor,
    c20_7_maximum_cardano_field_preimage_lengths_cbor,
    4,
    c20_7_maximum_cardano_address_witnesses_preimage_cbor,
  )
}

test v1_c20_6_accepted_cardano_maximum_matches_typescript_boundary_fixture() {
  let preimage = c20_6_maximum_cardano_script_witnesses_preimage_cbor
  let decoded = decode_midgard_tx_script_witnesses_preimage_cbor(preimage)
  let item_cbors = list.map(decoded, encode_midgard_versioned_script)
  let witness_set = decode_native_tx_witness_set_compact(
    c20_6_maximum_cardano_witness_set_compact_cbor,
  )
  let verified = verify_native_tx_compact_cbor_v1(
    c20_6_maximum_cardano_transaction_id,
    c20_6_maximum_cardano_compact_cbor,
  )
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    c20_6_maximum_cardano_field_preimage_lengths_cbor,
  )
  let synthetic_item_cbors = list.map(
    c20_synthetic_native_scripts(
      0,
      c20_6_maximum_cardano_native_script_witness_count,
    ),
    encode_midgard_versioned_script,
  )
  and {
    list.length(decoded) == c20_6_maximum_cardano_native_script_witness_count,
    list.all(
      decoded,
      fn(script) {
        and {
          script.language == NativeCardanoScript,
          bytearray.length(script.script_bytes) == 40,
          bytearray.take(script.script_bytes, 7) == #"8201828200581c",
        }
      },
    ),
    list.all(item_cbors, fn(item) { bytearray.length(item) == 44 }),
    // The hash-sorted fixture items are exactly the 224 deterministic
    // signer/expiry native scripts with expiry slots 20,000..20,223.
    list.sort(item_cbors, bytearray.compare) == list.sort(
      synthetic_item_cbors,
      bytearray.compare,
    ),
    bytearray.length(preimage) == c20_6_maximum_cardano_field_bytes,
    lengths.script_witnesses == c20_6_maximum_cardano_field_bytes,
    blake2b_256(preimage) == c20_6_maximum_cardano_preimage_hash,
    bounded_collection_v1.from_items(6, item_cbors) == c20_6_maximum_cardano_collection_commitment,
    witness_set.script_tx_wits_hash == c20_6_maximum_cardano_collection_commitment,
    encode_script_witness_preimage(decoded) == preimage,
    native_tx_canonical_size_v1(verified.tx_compact, lengths) == c20_6_maximum_cardano_canonical_bytes,
    transaction.verify_midgard_transaction_field_preimage_v1(
      c20_6_maximum_cardano_transaction_id,
      c20_6_maximum_cardano_transaction_commitment,
      c20_6_maximum_cardano_compact_cbor,
      c20_6_maximum_cardano_witness_set_compact_cbor,
      c20_6_maximum_cardano_field_preimage_lengths_cbor,
      6,
      preimage,
    ),
  }
}

test v1_c20_6_adjacent_script_count_exceeds_the_cardano_envelope() {
  let accepted = c20_synthetic_native_scripts(
    0,
    c20_6_maximum_cardano_native_script_witness_count,
  )
  let adjacent = c20_synthetic_native_scripts(
    0,
    c20_6_maximum_cardano_native_script_witness_count + 1,
  )
  and {
    bytearray.length(encode_script_witness_preimage(accepted)) == c20_6_maximum_cardano_field_bytes,
    bytearray.length(encode_script_witness_preimage(adjacent)) == c20_6_maximum_cardano_field_bytes + 44,
    c20_6_maximum_cardano_signed_cardano_bytes <= cardano_max_transaction_bytes,
    c20_6_adjacent_cardano_signed_cardano_bytes > cardano_max_transaction_bytes,
  }
}

test v1_c20_6_maximum_fixture_script_preimage_rejects_at_observer_position() fail {
  transaction.verify_midgard_transaction_field_preimage_v1(
    c20_6_maximum_cardano_transaction_id,
    c20_6_maximum_cardano_transaction_commitment,
    c20_6_maximum_cardano_compact_cbor,
    c20_6_maximum_cardano_witness_set_compact_cbor,
    c20_6_maximum_cardano_field_preimage_lengths_cbor,
    3,
    c20_6_maximum_cardano_script_witnesses_preimage_cbor,
  )
}
