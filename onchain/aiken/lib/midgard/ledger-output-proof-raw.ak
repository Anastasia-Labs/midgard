//// The canonical LOP control grammar, independent of sub-control semantics.
//// Callers must bind these bytes to their authenticated carrier before use.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_array_header}

pub opaque type Frame {
  Frame { items: List<Data> }
}

fn encode_items(items: List<Data>) -> ByteArray {
  when items is {
    [] -> #""
    [item, ..rest] -> bytearray.concat(encode_item(item), encode_items(rest))
  }
}

pub fn encode_item(item: Data) -> ByteArray {
  builtin.choose_data(
    item,
    {
      let Pair(tag, fields) = builtin.un_constr_data(item)
      when Pair(tag, fields) is {
        Pair(1, []) -> #"d87a80"
        Pair(0, [value]) ->
          #"d8799f"
            |> bytearray.concat(encode_item(value))
            |> bytearray.concat(#"ff")
        _ -> fail
      }
    },
    fail,
    {
      let items = builtin.un_list_data(item)
      encode_definite_array_header(list.length(items))
        |> bytearray.concat(encode_items(items))
    },
    cbor.serialise(builtin.un_i_data(item)),
    cbor.serialise(item),
  )
}

pub fn open(control_cbor: ByteArray) -> Frame {
  expect Some(data) = cbor.deserialise(control_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 12
  let frame = Frame { items }
  expect integer(frame, 0) == 1
  expect integer(frame, 1) >= 0 && integer(frame, 1) <= 6
  expect integer(frame, 2) >= 0
  expect integer(frame, 3) > 0
  expect bytearray.length(bytes(frame, 4)) == 32
  expect encode(frame) == control_cbor
  frame
}

pub fn item(frame: Frame, index: Int) -> Data {
  expect Some(value) = list.at(frame.items, index)
  value
}

pub fn integer(frame: Frame, index: Int) -> Int {
  builtin.un_i_data(item(frame, index))
}

pub fn bytes(frame: Frame, index: Int) -> ByteArray {
  builtin.un_b_data(item(frame, index))
}

pub fn encode(frame: Frame) -> ByteArray {
  #"8c" |> bytearray.concat(encode_items(frame.items))
}

pub fn replace(frame: Frame, index: Int, value: Data) -> Frame {
  expect index >= 0 && index < 12
  Frame {
    items: list.indexed_map(
      frame.items,
      fn(i, old) {
        if i == index {
          value
        } else {
          old
        }
      },
    ),
  }
}
