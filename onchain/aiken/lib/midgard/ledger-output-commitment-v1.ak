use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}

pub const version: Int = 1

pub const output_field_index: Int = 2

pub type LedgerOutputCommitmentV1 {
  version: Int,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
}

pub fn encode(descriptor: LedgerOutputCommitmentV1) -> ByteArray {
  expect descriptor.version == version
  expect descriptor.output_index >= 0
  expect descriptor.output_index <= 65535
  expect descriptor.total_length >= 0
  expect bytearray.length(descriptor.item_commitment) == 32
  encode_definite_array_header(4)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(descriptor.output_index))
    |> bytearray.concat(cbor.serialise(descriptor.total_length))
    |> bytearray.concat(encode_definite_bytes(descriptor.item_commitment))
}

pub fn decode(descriptor_cbor: ByteArray) -> LedgerOutputCommitmentV1 {
  expect Some(data) = cbor.deserialise(descriptor_cbor)
  expect
    [
      version_data, output_index_data, total_length_data,
      item_commitment_data,
    ] = builtin.un_list_data(data)
  let descriptor = LedgerOutputCommitmentV1 {
    version: builtin.un_i_data(version_data),
    output_index: builtin.un_i_data(output_index_data),
    total_length: builtin.un_i_data(total_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
  }
  expect encode(descriptor) == descriptor_cbor
  descriptor
}

pub fn from_output(
  output_index: Int,
  output_cbor: ByteArray,
) -> LedgerOutputCommitmentV1 {
  LedgerOutputCommitmentV1 {
    version,
    output_index,
    total_length: bytearray.length(output_cbor),
    item_commitment: bounded_item_v1.from_bytes(
      output_field_index,
      output_index,
      output_cbor,
    ),
  }
}

pub fn verify_chunk(
  descriptor: LedgerOutputCommitmentV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  and {
    descriptor.version == version,
    descriptor.output_index >= 0,
    descriptor.output_index <= 65535,
    descriptor.total_length >= 0,
    bytearray.length(descriptor.item_commitment) == 32,
    proof.field_index == output_field_index,
    proof.item_index == descriptor.output_index,
    proof.total_length == descriptor.total_length,
    bounded_item_v1.verify_chunk(descriptor.item_commitment, proof),
  }
}
