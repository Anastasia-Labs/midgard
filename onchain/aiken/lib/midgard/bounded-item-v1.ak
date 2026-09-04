use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/native_tx_field_access_v1.{field_count}
use midgard/validation_merkle_v1

pub const version: Int = 1

pub const chunk_bytes: Int = 4095

const chunk_domain = #"4d696467617264426f756e6465644974656d4368756e6b5631"

const commitment_domain =
  #"4d696467617264426f756e6465644974656d436f6d6d69746d656e745631"

pub type ChunkProofV1 {
  version: Int,
  field_index: Int,
  item_index: Int,
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
    1
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
  item_index: Int,
  chunk_index: Int,
  chunk: ByteArray,
) -> ByteArray {
  expect field_index_is_valid(field_index)
  expect item_index >= 0
  expect chunk_index >= 0
  expect bytearray.length(chunk) <= chunk_bytes
  chunk_domain
    |> bytearray.concat(encode_definite_array_header(5))
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(item_index))
    |> bytearray.concat(cbor.serialise(chunk_index))
    |> bytearray.concat(encode_definite_bytes(chunk))
    |> blake2b_256
}

pub fn commitment(
  field_index: Int,
  item_index: Int,
  total_length: Int,
  frontier: List<validation_merkle_v1.FrontierPeak>,
) -> ByteArray {
  expect field_index_is_valid(field_index)
  expect item_index >= 0
  let count = chunk_count(total_length)
  expect validation_merkle_v1.frontier_is_well_formed(count, frontier)
  commitment_domain
    |> bytearray.concat(encode_definite_array_header(5))
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(item_index))
    |> bytearray.concat(cbor.serialise(total_length))
    |> bytearray.concat(
        encode_definite_bytes(
          validation_merkle_v1.frontier_commitment(count, frontier),
        ),
      )
    |> blake2b_256
}

pub fn from_bytes(
  field_index: Int,
  item_index: Int,
  bytes: ByteArray,
) -> ByteArray {
  let total_length = bytearray.length(bytes)
  let count = chunk_count(total_length)
  let frontier = build_frontier(field_index, item_index, bytes, 0, count, [])
  commitment(field_index, item_index, total_length, frontier)
}

fn build_frontier(
  field_index: Int,
  item_index: Int,
  bytes: ByteArray,
  chunk_index: Int,
  count: Int,
  frontier: List<validation_merkle_v1.FrontierPeak>,
) -> List<validation_merkle_v1.FrontierPeak> {
  if chunk_index >= count {
    frontier
  } else {
    let offset = chunk_index * chunk_bytes
    let length = expected_chunk_length(bytearray.length(bytes), chunk_index)
    let chunk = if length == 0 {
      #""
    } else {
      bytearray.slice(bytes, offset, offset + length - 1)
    }
    build_frontier(
      field_index,
      item_index,
      bytes,
      chunk_index + 1,
      count,
      validation_merkle_v1.append_leaf(
        chunk_index,
        frontier,
        hash_chunk(field_index, item_index, chunk_index, chunk),
      ),
    )
  }
}

pub fn verify_chunk(expected_commitment: ByteArray, proof: ChunkProofV1) -> Bool {
  if and {
    proof.version == version,
    field_index_is_valid(proof.field_index),
    proof.item_index >= 0,
    proof.total_length >= 0,
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
        hash_chunk(
          proof.field_index,
          proof.item_index,
          proof.chunk_index,
          proof.chunk,
        ),
        proof.siblings,
      ),
      commitment(
        proof.field_index,
        proof.item_index,
        proof.total_length,
        proof.frontier,
      ) == expected_commitment,
    }
  } else {
    False
  }
}
