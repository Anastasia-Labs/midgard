//// Bounded envelope walk. Re-authenticate at most 32 KiB once per step, then
//// read at most 32 wrappers without re-hashing a chunk for every wrapper.
//// No item semantics are used until the entire envelope is certified.

use aiken/primitive/bytearray
use midgard/canonical_cbor_scan_v1 as cbor
use midgard/fraud_proofs/mint_item_non_canonical/scan
use midgard/native_tx_field_access_v1.{decode_field_array_header}

pub type Cursor {
  next_index: Int,
  next_offset: Int,
  item_offset: Int,
  item_length: Int,
  previous_policy: ByteArray,
}

pub fn advance(
  field: ByteArray,
  selected_index: Int,
  checkpoint: Option<Cursor>,
) -> Pair<Cursor, Bool> {
  let total = bytearray.length(field)
  expect total <= 32768
  let Pair(header_length, count) = decode_field_array_header(field)
  expect selected_index >= 0 && selected_index < count
  let start = when checkpoint is {
    None ->
      Cursor {
        next_index: 0,
        next_offset: header_length,
        item_offset: 0,
        item_length: 0,
        previous_policy: #"",
      }
    Some(cursor) -> cursor
  }
  expect start.next_index < count
  let next = walk(field, selected_index, count, total, start, 32)
  Pair(next, next.next_index == count)
}

fn walk(
  field: ByteArray,
  selected_index: Int,
  count: Int,
  total: Int,
  cursor: Cursor,
  budget: Int,
) -> Cursor {
  expect Some(head) = cbor.head_at_v1(field, cursor.next_offset, 2)
  let next_offset = head.next_offset + head.value
  let next_index = cursor.next_index + 1
  expect next_offset <= total
  expect
    if next_index == count {
      next_offset == total
    } else {
      next_offset < total
    }
  let previous_policy = if cursor.next_index == selected_index - 1 {
    expect head.value >= 31
    expect Some(policy) = scan.policy_id(
      bytearray.slice(field, head.next_offset, head.next_offset + 30),
    )
    policy
  } else {
    cursor.previous_policy
  }
  let next = Cursor {
    next_index,
    next_offset,
    previous_policy,
    item_offset: if cursor.next_index == selected_index {
      head.next_offset
    } else {
      cursor.item_offset
    },
    item_length: if cursor.next_index == selected_index {
      head.value
    } else {
      cursor.item_length
    },
  }
  if next_index == count || budget == 1 {
    next
  } else {
    walk(field, selected_index, count, total, next, budget - 1)
  }
}
