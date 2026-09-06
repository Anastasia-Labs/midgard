use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/transaction.{InlineDatum, Input}

pub const chunk_bytes = 4096

pub type Commitment {
  hash: ByteArray,
}

pub fn chunks(indices: List<Int>, references: List<Input>) -> ByteArray {
  expect [index, ..remaining] = indices
  let input = list.expect_at(references, index)
  expect InlineDatum(data) = input.output.datum
  expect bytes: ByteArray = data
  expect bytearray.length(bytes) > 0
  if remaining == [] {
    expect bytearray.length(bytes) <= chunk_bytes
    bytes
  } else {
    expect bytearray.length(bytes) == chunk_bytes
    bytearray.concat(bytes, chunks(remaining, references))
  }
}

pub fn open(indices: List<Int>, references: List<Input>) -> (Commitment, Data) {
  let bytes = chunks(indices, references)
  expect Some(data) = cbor.deserialise(bytes)
  // This is reference-byte transport, not a consensus serialiseData preimage.
  // Its exact bytes are frozen by the thread commitment; native preimages
  // contained within it still pass their canonical field/output doors.
  (Commitment { hash: blake2b_256(bytes) }, data)
}

pub fn read(
  commitment: Commitment,
  indices: List<Int>,
  references: List<Input>,
) -> Data {
  let (actual, data) = open(indices, references)
  expect actual == commitment
  data
}

pub fn fields(data: Data) -> (Data, Data) {
  expect [_, header, fault] = builtin.unconstr_fields(data)
  (header, fault)
}
