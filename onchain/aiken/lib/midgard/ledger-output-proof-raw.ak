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

/// The 17-item LOP control frame: items 0-11 are the proof sub-controls,
/// item 12 the authenticated-output-span window record
/// (`Option<[start, length, digest]>`) and items 13-16 the four descriptor
/// fact commitments (`Option<ByteArray>`, in `descriptor_roles` order).
pub const item_count: Int = 17

pub const span_window_index: Int = 12

pub const first_fact_index: Int = 13

pub fn open(control_cbor: ByteArray) -> Frame {
  expect Some(data) = cbor.deserialise(control_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == item_count
  let frame = Frame { items }
  expect integer(frame, 0) == 1
  expect integer(frame, 1) >= 0 && integer(frame, 1) <= 6
  expect integer(frame, 2) >= 0
  expect integer(frame, 3) > 0
  expect bytearray.length(bytes(frame, 4)) == 32
  expect encode(frame) == control_cbor
  frame
}

/// The recorded span window, decoded from item 12: `(start, length, digest)`
/// of the once-verified authenticated output span whose bytes later stage
/// steps bind by digest.
pub fn span_window(frame: Frame) -> Option<(Int, Int, ByteArray)> {
  let Pair(tag, fields) = builtin.un_constr_data(item(frame, span_window_index))
  if tag == 1 {
    expect fields == []
    None
  } else {
    expect tag == 0
    expect [record] = fields
    expect [start, length, digest] = builtin.un_list_data(record)
    Some(
      (
        builtin.un_i_data(start),
        builtin.un_i_data(length),
        builtin.un_b_data(digest),
      ),
    )
  }
}

pub fn span_window_data(start: Int, length: Int, digest: ByteArray) -> Data {
  builtin.constr_data(
    0,
    [
      builtin.list_data(
        [builtin.i_data(start), builtin.i_data(length), builtin.b_data(digest)],
      ),
    ],
  )
}

/// The recorded descriptor fact for role `role` (0-3), from items 13-16.
pub fn fact(frame: Frame, role: Int) -> Option<ByteArray> {
  expect role >= 0 && role < 4
  let Pair(tag, fields) = builtin.un_constr_data(
    item(frame, first_fact_index + role),
  )
  if tag == 1 {
    expect fields == []
    None
  } else {
    expect tag == 0
    expect [value] = fields
    Some(builtin.un_b_data(value))
  }
}

pub fn fact_data(commitment: ByteArray) -> Data {
  builtin.constr_data(0, [builtin.b_data(commitment)])
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
  #"91" |> bytearray.concat(encode_items(frame.items))
}

pub fn replace(frame: Frame, index: Int, value: Data) -> Frame {
  expect index >= 0 && index < item_count
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
