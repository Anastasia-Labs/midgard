use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{decode_canonical_int_at, expect_byte}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
}
use midgard/native_tx_field_access_v1.{
  decode_field_array_header_at, max_transaction_aggregate_field_bytes,
}

pub const batch_size = 32

pub type Control {
  item_count: Int,
  item_index: Int,
  cursor: Int,
  item_end: Int,
  remaining_assets: Int,
  previous_asset_name: Option<ByteArray>,
  policy_id: ByteArray,
  selected_complete: Bool,
}

pub fn initial(bytes: ByteArray, policy_index: Int) -> Control {
  expect bytearray.length(bytes) <= max_transaction_aggregate_field_bytes
  let Pair(cursor, count) = decode_field_array_header_at(bytes, 0)
  expect policy_index >= 0 && policy_index < count
  Control {
    item_count: count,
    item_index: 0,
    cursor,
    item_end: 0,
    remaining_assets: 0,
    previous_asset_name: None,
    policy_id: #"",
    selected_complete: False,
  }
}

pub fn complete(control: Control, total_length: Int) -> Bool {
  and {
    control.item_index == control.item_count,
    control.cursor == total_length,
    control.remaining_assets == 0,
    control.selected_complete,
    bytearray.length(control.policy_id) == 28,
  }
}

/// Each operation consumes one field envelope or one selected-policy asset.
/// Count/end consistency and the complete selected asset grammar are required
/// before a policy claim can leave this machine.
pub fn advance(
  control: Control,
  bytes: ByteArray,
  policy_index: Int,
  remaining: Int,
) -> Control {
  if remaining == 0 || control.item_index == control.item_count {
    control
  } else if control.remaining_assets > 0 {
    let Pair(cursor, asset_name) = decode_canonical_bytes_at(
      bytes,
      control.cursor,
    )
    expect bytearray.length(asset_name) <= 32
    expect
      when control.previous_asset_name is {
        None -> True
        Some(previous) -> canonical_bytes_key_precedes(previous, asset_name)
      }
    let Pair(cursor, quantity) = decode_canonical_int_at(bytes, cursor)
    expect quantity != 0 && cursor <= control.item_end
    let left = control.remaining_assets - 1
    expect left != 0 || cursor == control.item_end
    advance(
      Control {
        ..control,
        cursor,
        remaining_assets: left,
        previous_asset_name: Some(asset_name),
        item_index: if left == 0 {
          control.item_index + 1
        } else {
          control.item_index
        },
        selected_complete: left == 0,
      },
      bytes,
      policy_index,
      remaining - 1,
    )
  } else {
    expect control.item_index < control.item_count
    let Pair(next, item) = decode_canonical_bytes_at(bytes, control.cursor)
    expect next <= bytearray.length(bytes)
    let next_control = if control.item_index == policy_index {
      expect !control.selected_complete
      let start = next - bytearray.length(item)
      let cursor = expect_byte(bytes, start, 130)
      let Pair(cursor, policy_id) = decode_canonical_bytes_at(bytes, cursor)
      expect bytearray.length(policy_id) == 28
      let Pair(cursor, asset_count) = decode_canonical_map_header_at(
        bytes,
        cursor,
      )
      expect asset_count > 0 && cursor < next
      Control {
        ..control,
        cursor,
        item_end: next,
        remaining_assets: asset_count,
        previous_asset_name: None,
        policy_id,
      }
    } else {
      Control { ..control, cursor: next, item_index: control.item_index + 1 }
    }
    advance(next_control, bytes, policy_index, remaining - 1)
  }
}
