//// CanonicalDecode's total fixed-stride item predicate, shared with FIWI.
//// `item` and `previous` include their two-byte field-preimage wrappers.
//// The caller authenticates the field and derives these exact stride slices.
//// This module returns False for committed garbage; it never parses by expect.

use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray

pub const max_serialized_output_preimage_bytes: Int = 16384

/// Width half shared by CanonicalDecode and FIWI. Fixed-item callers still
/// run fixed_item_is_legal_v1 for the wrapper, content and adjacent order.
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

fn follows_previous(
  field_index: Int,
  previous: ByteArray,
  item: ByteArray,
) -> Bool {
  if field_index != 7 {
    // Inputs and required signers preserve authored order. Input uniqueness
    // and observer ordering are later semantic checks, not item-shape faults.
    True
  } else {
    // Shape was checked before slicing the verification key or hashing it.
    let previous_signer = blake2b_224(bytearray.slice(previous, 5, 36))
    let current_signer = blake2b_224(bytearray.slice(item, 5, 36))
    when bytearray.compare(previous_signer, current_signer) is {
      Less -> True
      Equal -> bytearray.compare(previous, item) == Less
      Greater -> False
    }
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
        if !shape_is_legal(field_index, prior) {
          False
        } else {
          follows_previous(field_index, prior, item)
        }
    }
  }
}
