//// **RETIRED counted-scheme surface** (docs/spec/midgard-tx.md section 4).
////
//// Midgard's nine compact-transaction field commitments used to be built
//// here as counted bounded-collection Merkle roots. They are now flat
//// blake2b-256 hashes over the enveloped field-preimage bytes, produced
//// and opened solely through `midgard/native_tx_field_access_v1` -- the one
//// lib-level field-access door.
////
//// Nothing below produces or verifies a field commitment any more. A flat
//// hash has no leaves, so `verify_item` cannot be satisfied against one:
//// the item-proof surface is unreachable from the live path and is kept
//// only so the dispute machine (issue 570) and the Phase-5 family-rebind
//// lanes keep compiling until they delete their counted call sites. Do not
//// bind new surface to it -- the domains below are prohibited legacy
//// surface.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/native_tx_field_access_v1.{field_count}
use midgard/validation_merkle_v1

pub const version: Int = 1

/// One byte per item is the absolute lower encoding floor under the preserved
/// 16,384-byte Cardano transaction envelope.
pub const max_tx_size_derived_item_count: Int = 16384

const item_domain =
  #"4d696467617264426f756e646564436f6c6c656374696f6e4974656d5631"

const commitment_domain =
  #"4d696467617264426f756e646564436f6c6c656374696f6e436f6d6d69746d656e745631"

pub type ItemProofV1 {
  version: Int,
  field_index: Int,
  item_count: Int,
  item_index: Int,
  item_length: Int,
  item_commitment: ByteArray,
  frontier: List<validation_merkle_v1.FrontierPeak>,
  siblings: List<ByteArray>,
}

fn field_index_is_valid(field_index: Int) -> Bool {
  and {
    field_index >= 0,
    field_index < field_count,
  }
}

pub fn hash_item(
  field_index: Int,
  item_index: Int,
  item_length: Int,
  item_commitment: ByteArray,
) -> ByteArray {
  expect field_index_is_valid(field_index)
  expect item_index >= 0
  expect item_length >= 0
  expect bytearray.length(item_commitment) == 32
  item_domain
    |> bytearray.concat(encode_definite_array_header(5))
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(item_index))
    |> bytearray.concat(cbor.serialise(item_length))
    |> bytearray.concat(encode_definite_bytes(item_commitment))
    |> blake2b_256
}

pub fn commitment(
  field_index: Int,
  item_count: Int,
  frontier: List<validation_merkle_v1.FrontierPeak>,
) -> ByteArray {
  expect field_index_is_valid(field_index)
  expect item_count >= 0
  expect validation_merkle_v1.frontier_is_well_formed(item_count, frontier)
  commitment_domain
    |> bytearray.concat(encode_definite_array_header(4))
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(item_count))
    |> bytearray.concat(
        encode_definite_bytes(
          validation_merkle_v1.frontier_commitment(item_count, frontier),
        ),
      )
    |> blake2b_256
}

pub fn verify_item(expected_commitment: ByteArray, proof: ItemProofV1) -> Bool {
  if and {
    proof.version == version,
    field_index_is_valid(proof.field_index),
    proof.item_count > 0,
    proof.item_length >= 0,
    bytearray.length(proof.item_commitment) == 32,
  } {
    and {
      bytearray.length(expected_commitment) == 32,
      proof.item_index >= 0,
      proof.item_index < proof.item_count,
      validation_merkle_v1.verify_membership(
        proof.item_count,
        proof.frontier,
        proof.item_index,
        hash_item(
          proof.field_index,
          proof.item_index,
          proof.item_length,
          proof.item_commitment,
        ),
        proof.siblings,
      ),
      commitment(proof.field_index, proof.item_count, proof.frontier) == expected_commitment,
    }
  } else {
    False
  }
}
