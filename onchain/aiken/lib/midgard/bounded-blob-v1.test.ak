use aiken/builtin
use aiken/primitive/bytearray
use midgard/bounded_blob_v1
use midgard/validation_merkle_v1

test chunk_commitment_binds_exact_lengths_and_membership() {
  let first = builtin.replicate_byte(4095, 90)
  let second = #"010203"
  let first_hash = bounded_blob_v1.hash_chunk(2, 0, first)
  let second_hash = bounded_blob_v1.hash_chunk(2, 1, second)
  let built = validation_merkle_v1.build_frontier([first_hash, second_hash])
  let expected = bounded_blob_v1.commitment(2, 4098, built.peaks)
  let proof = bounded_blob_v1.ChunkProofV1 {
    version: bounded_blob_v1.version,
    field_index: 2,
    total_length: 4098,
    chunk_index: 1,
    chunk: second,
    frontier: built.peaks,
    siblings: [first_hash],
  }
  and {
    built.count == 2,
    first_hash == #"0d56e8ba6350807a8760329ede34888c9c63884515d40ff9a809a40c9122198a",
    second_hash == #"08790d8ac85ae960107a2dcded04a2935110432c13f1ebbbf5a7c9de8eadc290",
    expected == #"61a3c28584495eef1a5c76661f772158a460356921e30d5ffbbdae23d4894959",
    bounded_blob_v1.verify_chunk(expected, proof),
    !bounded_blob_v1.verify_chunk(
      expected,
      bounded_blob_v1.ChunkProofV1 { ..proof, field_index: 3 },
    ),
    !bounded_blob_v1.verify_chunk(
      expected,
      bounded_blob_v1.ChunkProofV1 { ..proof, total_length: 4099 },
    ),
    !bounded_blob_v1.verify_chunk(
      expected,
      bounded_blob_v1.ChunkProofV1 { ..proof, chunk: #"010204" },
    ),
    bounded_blob_v1.from_preimage(2, builtin.append_bytearray(first, second)) == expected,
  }
}

test empty_blob_has_a_commitment_but_no_chunk_proof() {
  let expected = bounded_blob_v1.commitment(
    0,
    0,
    validation_merkle_v1.empty_frontier(),
  )
  and {
    bytearray.length(expected) == 32,
    bounded_blob_v1.from_preimage(0, #"") == expected,
    !bounded_blob_v1.verify_chunk(
      expected,
      bounded_blob_v1.ChunkProofV1 {
        version: bounded_blob_v1.version,
        field_index: 0,
        total_length: 0,
        chunk_index: 0,
        chunk: #"",
        frontier: [],
        siblings: [],
      },
    ),
  }
}
