//// CanonicalDecode's total fixed-stride item predicate, shared with FIWI.
//// `item` and `previous` include their two-byte field-preimage wrappers.
//// The caller authenticates the field and derives these exact stride slices.
//// This module returns False for committed garbage; it never parses by expect.

use aiken/builtin
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray

pub const max_serialized_output_preimage_bytes: Int = 16384

/// Width half shared by CanonicalDecode and FIWI. Fixed-item callers still
/// run fixed_item_is_legal_v1 for the current and preceding encoded shapes.
pub fn item_width_is_legal_v1(field_index: Int, item_width: Int) -> Bool {
  if item_width < 0 {
    False
  } else if field_index == 0 || field_index == 1 {
    item_width == 38
  } else if field_index == 3 || field_index == 4 {
    item_width == 28
  } else if field_index == 7 {
    item_width == 101
  } else if field_index == 2 {
    item_width <= max_serialized_output_preimage_bytes
  } else if field_index == 5 {
    item_width > 0
  } else {
    field_index == 6 || field_index == 8
  }
}

pub fn is_fixed_field_v1(field_index: Int) -> Bool {
  or {
    field_index == 0,
    field_index == 1,
    field_index == 3,
    field_index == 4,
    field_index == 7,
  }
}

fn shape_is_legal(field_index: Int, item: ByteArray) -> Bool {
  let length = bytearray.length(item)
  if field_index == 0 || field_index == 1 {
    if length != 40 {
      False
    } else {
      and {
        bytearray.slice(item, 0, 4) == #"5826825820",
        bytearray.slice(item, 37, 37) == #"19",
      }
    }
  } else if field_index == 3 || field_index == 4 {
    and {
      length == 30,
      bytearray.slice(item, 0, 1) == #"581c",
    }
  } else if field_index == 7 {
    if length != 103 {
      False
    } else {
      and {
        bytearray.slice(item, 0, 4) == #"5865825820",
        bytearray.slice(item, 37, 38) == #"5840",
      }
    }
  } else {
    False
  }
}

/// CanonicalDecode's item order over two adjacent fixed-stride payloads
/// (without the two-byte field-preimage wrapper) of one field. Fields 0 and 1
/// ascend by item bytes, repeats allowed (`InputSets` reports them as
/// `DuplicateInput`). Field 7 ascends strictly by
/// `(blake2b_224(vkey), item bytes)`; the vkey is payload bytes 3..35 of
/// `82 5820 <vkey> 5840 <signature>`. Every other field is unordered here.
pub fn fixed_payload_follows_v1(
  field_index: Int,
  previous: ByteArray,
  current: ByteArray,
) -> Bool {
  if field_index == 7 {
    when
      bytearray.compare(
        blake2b_224(builtin.slice_bytearray(3, 32, previous)),
        blake2b_224(builtin.slice_bytearray(3, 32, current)),
      )
    is {
      Less -> True
      Equal -> bytearray.compare(previous, current) == Less
      Greater -> False
    }
  } else if field_index == 0 || field_index == 1 {
    bytearray.compare(previous, current) != Greater
  } else {
    True
  }
}

pub fn fixed_item_is_legal_v1(
  field_index: Int,
  previous: Option<ByteArray>,
  item: ByteArray,
) -> Bool {
  if !shape_is_legal(field_index, item) {
    False
  } else {
    when previous is {
      None -> True
      Some(prior) ->
        // Shape is checked before the wrapper is dropped and the key read.
        if !shape_is_legal(field_index, prior) {
          False
        } else {
          fixed_payload_follows_v1(
            field_index,
            bytearray.drop(prior, 2),
            bytearray.drop(item, 2),
          )
        }
    }
  }
}
