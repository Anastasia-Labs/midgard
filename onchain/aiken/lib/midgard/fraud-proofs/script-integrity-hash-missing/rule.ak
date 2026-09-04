//// Canonical Phase-A `ScriptIntegrityHashMissing` predicate.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at,
}
use midgard/fraud_proofs/native_tx/types.{NativeCardanoScript}
use midgard/native_tx_field_access_v1.{
  FieldViewV1, field_item_at, field_item_count,
}

pub const zero_hash32: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

pub fn contains_non_native_script_items_v1(items: List<ByteArray>) -> Bool {
  when items is {
    [] -> False
    [item, ..rest] -> {
      let Pair(offset, script) = decode_midgard_versioned_script_at(item, 0)
      expect offset == bytearray.length(item)
      if script.language != NativeCardanoScript {
        True
      } else {
        contains_non_native_script_items_v1(rest)
      }
    }
  }
}

fn view_contains_non_native(view: FieldViewV1, cursor: Int, count: Int) -> Bool {
  if cursor == count {
    False
  } else {
    let item = field_item_at(view, cursor)
    let Pair(offset, script) = decode_midgard_versioned_script_at(item, 0)
    expect offset == bytearray.length(item)
    if script.language != NativeCardanoScript {
      True
    } else {
      view_contains_non_native(view, cursor + 1, count)
    }
  }
}

pub fn field_view_contains_non_native_script_v1(view: FieldViewV1) -> Bool {
  view_contains_non_native(view, 0, field_item_count(view))
}

pub fn fault_holds_v1(
  script_integrity_hash: ByteArray,
  contains_non_native_script: Bool,
  has_redeemers: Bool,
) -> Bool {
  expect bytearray.length(script_integrity_hash) == 32
  let requires_plutus_evaluation = or {
    contains_non_native_script,
    has_redeemers,
    script_integrity_hash != zero_hash32,
  }
  and {
    requires_plutus_evaluation,
    script_integrity_hash == zero_hash32,
  }
}
