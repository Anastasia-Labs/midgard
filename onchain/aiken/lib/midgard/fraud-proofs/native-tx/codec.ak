use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/types.{
  FailedScript, FeeTooLow, InvalidSignature, MidgardTxValidity,
  NonExistentInputUtxo, TxIsValid, UnbalancedTx,
}

pub fn expect_h32(bytes: ByteArray) -> ByteArray {
  expect bytearray.length(bytes) == 32
  bytes
}

pub fn expect_h28(bytes: ByteArray) -> ByteArray {
  expect bytearray.length(bytes) == 28
  bytes
}

pub fn expect_non_negative(value: Int) -> Int {
  expect value >= 0
  value
}

pub fn expect_validity_code(value: Int) -> Int {
  expect _non_negative = expect_non_negative(value)
  expect value <= 5
  value
}

pub fn validity_from_code(value: Int) -> MidgardTxValidity {
  expect _validity_code = expect_validity_code(value)
  if value == 0 {
    TxIsValid
  } else if value == 1 {
    NonExistentInputUtxo
  } else if value == 2 {
    InvalidSignature
  } else if value == 3 {
    FailedScript
  } else if value == 4 {
    FeeTooLow
  } else {
    UnbalancedTx
  }
}

pub fn validity_to_code(validity: MidgardTxValidity) -> Int {
  when validity is {
    TxIsValid -> 0
    NonExistentInputUtxo -> 1
    InvalidSignature -> 2
    FailedScript -> 3
    FeeTooLow -> 4
    UnbalancedTx -> 5
  }
}

/// Explicit bridge between the scalar native-transaction validity language
/// and the corresponding nullary PlutusData constructor language.
pub fn validity_to_plutus_data(validity: MidgardTxValidity) -> Data {
  builtin.constr_data(validity_to_code(validity), [])
}

pub fn validity_from_plutus_data(data: Data) -> MidgardTxValidity {
  expect builtin.unconstr_fields(data) == []
  validity_from_code(builtin.unconstr_index(data))
}

pub fn expect_network_id(value: Int) -> Int {
  expect _non_negative = expect_non_negative(value)
  expect or {
      value == 0,
      value == 1,
      value == 255,
    }
  value
}

pub fn slice_len(bytes: ByteArray, offset: Int, length: Int) -> ByteArray {
  expect length >= 0
  if length == 0 {
    #""
  } else {
    bytearray.slice(bytes, offset, offset + length - 1)
  }
}

pub fn byte_at(bytes: ByteArray, offset: Int) -> Int {
  bytearray.at(bytes, offset)
}

pub fn expect_byte(bytes: ByteArray, offset: Int, expected: Int) -> Int {
  expect byte_at(bytes, offset) == expected
  offset + 1
}

pub fn decode_uint_at(bytes: ByteArray, offset: Int) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if tag <= 23 {
    Pair(offset + 1, tag)
  } else if tag == 24 {
    Pair(offset + 2, byte_at(bytes, offset + 1))
  } else if tag == 25 {
    Pair(
      offset + 3,
      byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2),
    )
  } else if tag == 26 {
    Pair(
      offset + 5,
      (
        ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
          bytes,
          offset + 3,
        )
      ) * 256 + byte_at(bytes, offset + 4),
    )
  } else {
    expect tag == 27
    Pair(
      offset + 9,
      (
        (
          (
            (
              (
                (
                  byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2)
                ) * 256 + byte_at(bytes, offset + 3)
              ) * 256 + byte_at(bytes, offset + 4)
            ) * 256 + byte_at(bytes, offset + 5)
          ) * 256 + byte_at(bytes, offset + 6)
        ) * 256 + byte_at(bytes, offset + 7)
      ) * 256 + byte_at(bytes, offset + 8),
    )
  }
}

pub fn decode_canonical_uint_at(bytes: ByteArray, offset: Int) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if tag <= 23 {
    Pair(offset + 1, tag)
  } else if tag == 24 {
    let value = byte_at(bytes, offset + 1)
    expect value >= 24
    Pair(offset + 2, value)
  } else if tag == 25 {
    let value = byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2)
    expect value > 0xff
    Pair(offset + 3, value)
  } else if tag == 26 {
    let value = (
      ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
        bytes,
        offset + 3,
      )
    ) * 256 + byte_at(bytes, offset + 4)
    expect value > 0xffff
    Pair(offset + 5, value)
  } else {
    expect tag == 27
    let value = (
      (
        (
          (
            (
              ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
                bytes,
                offset + 3,
              )
            ) * 256 + byte_at(bytes, offset + 4)
          ) * 256 + byte_at(bytes, offset + 5)
        ) * 256 + byte_at(bytes, offset + 6)
      ) * 256 + byte_at(bytes, offset + 7)
    ) * 256 + byte_at(bytes, offset + 8)
    expect value > 0xffffffff
    Pair(offset + 9, value)
  }
}

pub fn decode_int_at(bytes: ByteArray, offset: Int) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if tag <= 27 {
    decode_uint_at(bytes, offset)
  } else if and {
    tag >= 32,
    tag <= 55,
  } {
    Pair(offset + 1, -1 - ( tag - 32 ))
  } else if tag == 56 {
    Pair(offset + 2, -1 - byte_at(bytes, offset + 1))
  } else if tag == 57 {
    Pair(
      offset + 3,
      -1 - ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ),
    )
  } else if tag == 58 {
    Pair(
      offset + 5,
      -1 - (
        (
          ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
            bytes,
            offset + 3,
          )
        ) * 256 + byte_at(bytes, offset + 4)
      ),
    )
  } else {
    expect tag == 59
    Pair(
      offset + 9,
      -1 - (
        (
          (
            (
              (
                (
                  (
                    byte_at(bytes, offset + 1) * 256 + byte_at(
                      bytes,
                      offset + 2,
                    )
                  ) * 256 + byte_at(bytes, offset + 3)
                ) * 256 + byte_at(bytes, offset + 4)
              ) * 256 + byte_at(bytes, offset + 5)
            ) * 256 + byte_at(bytes, offset + 6)
          ) * 256 + byte_at(bytes, offset + 7)
        ) * 256 + byte_at(bytes, offset + 8)
      ),
    )
  }
}

pub fn decode_canonical_int_at(bytes: ByteArray, offset: Int) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if tag <= 27 {
    decode_canonical_uint_at(bytes, offset)
  } else if and {
    tag >= 32,
    tag <= 55,
  } {
    Pair(offset + 1, -1 - ( tag - 32 ))
  } else if tag == 56 {
    let value = byte_at(bytes, offset + 1)
    expect value >= 24
    Pair(offset + 2, -1 - value)
  } else if tag == 57 {
    let value = byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2)
    expect value > 0xff
    Pair(offset + 3, -1 - value)
  } else if tag == 58 {
    let value = (
      ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
        bytes,
        offset + 3,
      )
    ) * 256 + byte_at(bytes, offset + 4)
    expect value > 0xffff
    Pair(offset + 5, -1 - value)
  } else {
    expect tag == 59
    let value = (
      (
        (
          (
            (
              ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
                bytes,
                offset + 3,
              )
            ) * 256 + byte_at(bytes, offset + 4)
          ) * 256 + byte_at(bytes, offset + 5)
        ) * 256 + byte_at(bytes, offset + 6)
      ) * 256 + byte_at(bytes, offset + 7)
    ) * 256 + byte_at(bytes, offset + 8)
    expect value > 0xffffffff
    Pair(offset + 9, -1 - value)
  }
}

pub fn decode_definite_bytes_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, ByteArray> {
  let tag = byte_at(bytes, offset)
  if and {
    tag >= 64,
    tag <= 87,
  } {
    let length = tag - 64
    Pair(offset + 1 + length, slice_len(bytes, offset + 1, length))
  } else if tag == 88 {
    let length = byte_at(bytes, offset + 1)
    Pair(offset + 2 + length, slice_len(bytes, offset + 2, length))
  } else if tag == 89 {
    let length = byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2)
    Pair(offset + 3 + length, slice_len(bytes, offset + 3, length))
  } else {
    expect tag == 90
    let length = (
      ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
        bytes,
        offset + 3,
      )
    ) * 256 + byte_at(bytes, offset + 4)
    Pair(offset + 5 + length, slice_len(bytes, offset + 5, length))
  }
}

pub fn decode_definite_array_header_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if and {
    tag >= 128,
    tag <= 151,
  } {
    Pair(offset + 1, tag - 128)
  } else if tag == 152 {
    Pair(offset + 2, byte_at(bytes, offset + 1))
  } else if tag == 153 {
    Pair(
      offset + 3,
      byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2),
    )
  } else {
    expect tag == 154
    Pair(
      offset + 5,
      (
        ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
          bytes,
          offset + 3,
        )
      ) * 256 + byte_at(bytes, offset + 4),
    )
  }
}

pub fn decode_definite_map_header_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if and {
    tag >= 160,
    tag <= 183,
  } {
    Pair(offset + 1, tag - 160)
  } else if tag == 184 {
    Pair(offset + 2, byte_at(bytes, offset + 1))
  } else if tag == 185 {
    Pair(
      offset + 3,
      byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2),
    )
  } else {
    expect tag == 186
    Pair(
      offset + 5,
      (
        ( byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2) ) * 256 + byte_at(
          bytes,
          offset + 3,
        )
      ) * 256 + byte_at(bytes, offset + 4),
    )
  }
}

pub fn encode_definite_array_header(length: Int) -> ByteArray {
  expect length >= 0
  if length <= 23 {
    bytearray.push(#"", 128 + length)
  } else if length <= 255 {
    bytearray.concat(#"98", bytearray.from_int_big_endian(length, 1))
  } else if length <= 65535 {
    bytearray.concat(#"99", bytearray.from_int_big_endian(length, 2))
  } else {
    bytearray.concat(#"9a", bytearray.from_int_big_endian(length, 4))
  }
}

pub fn encode_definite_map_header(length: Int) -> ByteArray {
  expect length >= 0
  if length <= 23 {
    bytearray.push(#"", 160 + length)
  } else if length <= 255 {
    bytearray.concat(#"b8", bytearray.from_int_big_endian(length, 1))
  } else if length <= 65535 {
    bytearray.concat(#"b9", bytearray.from_int_big_endian(length, 2))
  } else {
    bytearray.concat(#"ba", bytearray.from_int_big_endian(length, 4))
  }
}

pub fn encode_definite_bytes(bytes: ByteArray) -> ByteArray {
  let length = bytearray.length(bytes)
  if length <= 23 {
    bytearray.push(bytes, 64 + length)
  } else if length <= 255 {
    #"58"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 1))
      |> bytearray.concat(bytes)
  } else if length <= 65535 {
    #"59"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 2))
      |> bytearray.concat(bytes)
  } else {
    #"5a"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 4))
      |> bytearray.concat(bytes)
  }
}

pub fn encode_native_byte_list(items: List<ByteArray>) -> ByteArray {
  bytearray.concat(
    encode_definite_array_header(list.length(items)),
    list.foldr(
      items,
      #"",
      fn(item, encoded_tail) {
        bytearray.concat(encode_definite_bytes(item), encoded_tail)
      },
    ),
  )
}

pub fn expect_deserialise(bytes: ByteArray) -> Data {
  expect Some(data) = cbor.deserialise(bytes)
  data
}

pub fn expect_preimage_hash(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> ByteArray {
  expect _expected_hash = expect_h32(expected_hash)
  expect blake2b_256(preimage_cbor) == expected_hash
  preimage_cbor
}
