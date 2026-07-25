use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/validation_merkle_v1

pub const version: Int = 1

pub const chunk_bytes: Int = 4095

pub const field_count: Int = 9

const chunk_domain = #"4d696467617264426f756e646564426c6f624368756e6b5631"

const commitment_domain =
  #"4d696467617264426f756e646564426c6f62436f6d6d69746d656e745631"

pub type ChunkProofV1 {
  version: Int,
  field_index: Int,
  total_length: Int,
  chunk_index: Int,
  chunk: ByteArray,
  frontier: List<validation_merkle_v1.FrontierPeak>,
  siblings: List<ByteArray>,
}

fn field_index_is_valid(field_index: Int) -> Bool {
  and {
    field_index >= 0,
    field_index < field_count,
  }
}

pub fn chunk_count(total_length: Int) -> Int {
  expect total_length >= 0
  if total_length == 0 {
    0
  } else {
    ( total_length + chunk_bytes - 1 ) / chunk_bytes
  }
}

pub fn expected_chunk_length(total_length: Int, chunk_index: Int) -> Int {
  let count = chunk_count(total_length)
  expect chunk_index >= 0
  expect chunk_index < count
  if chunk_index + 1 < count {
    chunk_bytes
  } else {
    total_length - chunk_index * chunk_bytes
  }
}

pub fn hash_chunk(
  field_index: Int,
  chunk_index: Int,
  chunk: ByteArray,
) -> ByteArray {
  expect field_index_is_valid(field_index)
  expect chunk_index >= 0
  expect bytearray.length(chunk) <= chunk_bytes
  chunk_domain
    |> bytearray.concat(encode_definite_array_header(4))
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(chunk_index))
    |> bytearray.concat(encode_definite_bytes(chunk))
    |> blake2b_256
}

pub fn commitment(
  field_index: Int,
  total_length: Int,
  frontier: List<validation_merkle_v1.FrontierPeak>,
) -> ByteArray {
  expect field_index_is_valid(field_index)
  let count = chunk_count(total_length)
  expect validation_merkle_v1.frontier_is_well_formed(count, frontier)
  commitment_domain
    |> bytearray.concat(encode_definite_array_header(4))
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(total_length))
    |> bytearray.concat(
        encode_definite_bytes(
          validation_merkle_v1.frontier_commitment(count, frontier),
        ),
      )
    |> blake2b_256
}

pub fn from_preimage(field_index: Int, preimage: ByteArray) -> ByteArray {
  let total_length = bytearray.length(preimage)
  let count = chunk_count(total_length)
  let frontier = if count == 0 {
    validation_merkle_v1.empty_frontier()
  } else {
    build_preimage_frontier(field_index, preimage, 0, count, [])
  }
  commitment(field_index, total_length, frontier)
}

fn build_preimage_frontier(
  field_index: Int,
  preimage: ByteArray,
  chunk_index: Int,
  count: Int,
  frontier: List<validation_merkle_v1.FrontierPeak>,
) -> List<validation_merkle_v1.FrontierPeak> {
  if chunk_index >= count {
    frontier
  } else {
    let offset = chunk_index * chunk_bytes
    let length = expected_chunk_length(bytearray.length(preimage), chunk_index)
    let chunk = bytearray.slice(preimage, offset, offset + length - 1)
    build_preimage_frontier(
      field_index,
      preimage,
      chunk_index + 1,
      count,
      validation_merkle_v1.append_leaf(
        chunk_index,
        frontier,
        hash_chunk(field_index, chunk_index, chunk),
      ),
    )
  }
}

pub fn verify_chunk(expected_commitment: ByteArray, proof: ChunkProofV1) -> Bool {
  if and {
    proof.version == version,
    field_index_is_valid(proof.field_index),
    proof.total_length > 0,
  } {
    let count = chunk_count(proof.total_length)
    and {
      bytearray.length(expected_commitment) == 32,
      proof.chunk_index >= 0,
      proof.chunk_index < count,
      bytearray.length(proof.chunk) == expected_chunk_length(
        proof.total_length,
        proof.chunk_index,
      ),
      validation_merkle_v1.verify_membership(
        count,
        proof.frontier,
        proof.chunk_index,
        hash_chunk(proof.field_index, proof.chunk_index, proof.chunk),
        proof.siblings,
      ),
      commitment(proof.field_index, proof.total_length, proof.frontier) == expected_commitment,
    }
  } else {
    False
  }
}
