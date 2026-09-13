use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{decode_canonical_int_at, expect_byte}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
}

/// The exact canonical policy-item prefix, without materializing its map.
pub fn policy_header(
  window: ByteArray,
  previous: Option<ByteArray>,
) -> Pair<ByteArray, Pair<Int, Int>> {
  let start = expect_byte(window, 0, 130)
  let Pair(offset, policy_id) = decode_canonical_bytes_at(window, start)
  expect bytearray.length(policy_id) == 28
  expect
    when previous is {
      None -> True
      Some(before) -> canonical_bytes_key_precedes(before, policy_id)
    }
  let Pair(cursor, count) = decode_canonical_map_header_at(window, offset)
  expect count > 0
  Pair(policy_id, Pair(cursor, count))
}

/// One sorted canonical asset-name/quantity pair. The caller authenticates
/// the window's absolute offset and proves exact policy-item exhaustion.
pub fn asset(
  window: ByteArray,
  previous: Option<ByteArray>,
) -> Pair<ByteArray, Pair<Int, Int>> {
  let Pair(offset, name) = decode_canonical_bytes_at(window, 0)
  expect bytearray.length(name) <= 32
  expect
    when previous is {
      None -> True
      Some(before) -> canonical_bytes_key_precedes(before, name)
    }
  let Pair(consumed, quantity) = decode_canonical_int_at(window, offset)
  expect quantity != 0
  Pair(name, Pair(quantity, consumed))
}
