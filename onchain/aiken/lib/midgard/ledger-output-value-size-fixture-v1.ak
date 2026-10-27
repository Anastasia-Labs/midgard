//// Test fixtures: canonical two-entry outputs whose Cardano value is a chosen
//// number of bytes, spelled with long asset names, and the chunk material
//// of a two-chunk output item.
////
//// The value is `[2_000_000, {policy: {141 32-byte names ↦ 1, one 23-byte
//// name ↦ tail_quantity}}]`:
////
////   array head 1 + lovelace 5 + policy map head 1 + policy 30
////   + asset map head (142 entries) 2 = 39
////   + 141 full entries × (name 34 + quantity 1) = 4,935
////   + tail entry: name 24 + quantity (2 for 24, 3 for 256)
////
//// so `tail_quantity` 24 gives 5,000 bytes and 256 gives 5,001. The output
//// `{0: address(29), 1: value}` adds 34 bytes in front of the value, so the
//// value runs across the 4,095-byte chunk boundary.

use aiken/builtin
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/validation_merkle_v1 as merkle

pub const address =
  #"6011111111111111111111111111111111111111111111111111111111"

pub const policy_id =
  #"55555555555555555555555555555555555555555555555555555555"

/// The tail quantity of a 5,000-byte value.
pub const quantity_at_cap: Int = 24

/// The tail quantity of a 5,001-byte value.
pub const quantity_over_cap: Int = 256

const full_name_count: Int = 141

/// The full-width asset entries `index ..< full_name_count`: name
/// `index ‖ 0xaa × 31` (32 bytes), quantity 1. The leading byte orders them.
fn full_entries(index: Int) -> ByteArray {
  if index >= full_name_count {
    #""
  } else {
    #"5820"
      |> bytearray.concat(builtin.cons_bytearray(index, #""))
      |> bytearray.concat(builtin.replicate_byte(31, 170))
      |> bytearray.concat(#"01")
      |> bytearray.concat(full_entries(index + 1))
  }
}

/// The 23-byte tail name `0xff ‖ 0xbb × 22`, ordered after every full name.
fn tail_entry(tail_quantity: Int) -> ByteArray {
  let quantity = if tail_quantity == quantity_at_cap {
    #"1818"
  } else {
    expect tail_quantity == quantity_over_cap
    #"190100"
  }
  #"57ff"
    |> bytearray.concat(builtin.replicate_byte(22, 187))
    |> bytearray.concat(quantity)
}

/// The Cardano value CBOR described in the module header.
pub fn long_name_value(tail_quantity: Int) -> ByteArray {
  #"821a001e8480a1581c"
    |> bytearray.concat(policy_id)
    |> bytearray.concat(#"b88e")
    |> bytearray.concat(full_entries(0))
    |> bytearray.concat(tail_entry(tail_quantity))
}

/// The canonical output `{0: address, 1: long_name_value(tail_quantity)}`.
pub fn long_name_output(tail_quantity: Int) -> ByteArray {
  #"a200581d"
    |> bytearray.concat(address)
    |> bytearray.concat(#"01")
    |> bytearray.concat(long_name_value(tail_quantity))
}

/// The commitment and the chunk proofs, in chunk order, of the two-chunk
/// output item `output` at `output_index` of field 2.
pub fn two_chunk_material(
  output_index: Int,
  output: ByteArray,
) -> Pair<ByteArray, List<bounded_item_v1.ChunkProofV1>> {
  let total_length = bytearray.length(output)
  expect bounded_item_v1.chunk_count(total_length) == 2
  let first = bytearray.take(output, bounded_item_v1.chunk_bytes)
  let second = bytearray.drop(output, bounded_item_v1.chunk_bytes)
  let first_leaf = bounded_item_v1.hash_chunk(2, output_index, 0, first)
  let second_leaf = bounded_item_v1.hash_chunk(2, output_index, 1, second)
  let frontier = merkle.build_frontier([first_leaf, second_leaf]).peaks
  let chunk_proof = fn(index, chunk, siblings) {
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 2,
      item_index: output_index,
      total_length,
      chunk_index: index,
      chunk,
      frontier,
      siblings,
    }
  }
  Pair(
    bounded_item_v1.commitment(2, output_index, total_length, frontier),
    [chunk_proof(0, first, [second_leaf]), chunk_proof(1, second, [first_leaf])],
  )
}
