//// Total §5.6 parser. No quantity accounting, authorization or asset-count
//// limit predicate lives here. All work and retained state are bounded.

use aiken/primitive/bytearray
use midgard/canonical_cbor_scan_v1 as cbor
use midgard/fraud_proofs/native_tx/preimages.{canonical_bytes_key_precedes}

pub const terminal: Int = 2

pub type Control {
  stage: Int,
  cursor: Int,
  remaining: Int,
  previous_policy: ByteArray,
  previous_asset: Option<ByteArray>,
}

pub fn initial(previous_policy: ByteArray) -> Control {
  Control {
    stage: 0,
    cursor: 0,
    remaining: 0,
    previous_policy,
    previous_asset: None,
  }
}

/// Header only: adjacent policy ordering requires authentic policy keys, not
/// an assertion that the predecessor's independent asset body is valid.
pub fn policy_id(item: ByteArray) -> Option<ByteArray> {
  if bytearray.take(item, 3) == #"82581c" && bytearray.length(item) >= 31 {
    Some(bytearray.slice(item, 3, 30))
  } else {
    None
  }
}

fn header(control: Control, total: Int, window: ByteArray) -> Option<Control> {
  when policy_id(window) is {
    None -> None
    Some(policy) ->
      when cbor.head_at_v1(window, 31, 5) is {
        None -> None
        Some(head) ->
          if and {
            // The native mint grammar uses map heads through uint32 only.
            head.value <= 4294967295,
            head.value > 0,
            head.next_offset < total,
            or {
              control.previous_policy == #"",
              canonical_bytes_key_precedes(control.previous_policy, policy),
            },
          } {
            Some(
              Control {
                ..control,
                stage: 1,
                cursor: head.next_offset,
                remaining: head.value,
              },
            )
          } else {
            None
          }
      }
  }
}

fn asset(
  control: Control,
  total: Int,
  window: ByteArray,
  offset: Int,
) -> Option<Control> {
  when cbor.head_at_v1(window, offset, 2) is {
    None -> None
    Some(name_head) ->
      if name_head.value > 32 {
        None
      } else {
        when cbor.bytes_at_v1(window, offset) is {
          None -> None
          Some(name) -> {
            let quantity_offset = name.next_offset
            let positive = cbor.head_at_v1(window, quantity_offset, 0)
            let quantity = when positive is {
              Some(head) ->
                if head.value == 0 {
                  None
                } else {
                  Some(head)
                }
              None -> cbor.head_at_v1(window, quantity_offset, 1)
            }
            when quantity is {
              None -> None
              Some(head) -> {
                let cursor = control.cursor + head.next_offset - offset
                let remaining = control.remaining - 1
                if and {
                  when control.previous_asset is {
                    None -> True
                    Some(previous) ->
                      canonical_bytes_key_precedes(previous, name.value)
                  },
                  if remaining == 0 {
                    cursor == total
                  } else {
                    cursor < total
                  },
                } {
                  Some(
                    Control {
                      ..control,
                      cursor,
                      remaining,
                      previous_asset: Some(name.value),
                      stage: if remaining == 0 {
                        terminal
                      } else {
                        1
                      },
                    },
                  )
                } else {
                  None
                }
              }
            }
          }
        }
      }
  }
}

/// A window contains the current 4095-byte chunk and its successor, so even a
/// maximum-width token straddling the boundary is fully authenticated. Thirty
/// two entries are the per-transaction work cap; stop before a third chunk.
pub fn advance(
  control: Control,
  total: Int,
  window: ByteArray,
  offset: Int,
) -> Option<Control> {
  if control.stage == 0 {
    header(control, total, window)
  } else {
    assets(control, total, window, offset, 32)
  }
}

fn assets(
  control: Control,
  total: Int,
  window: ByteArray,
  offset: Int,
  budget: Int,
) -> Option<Control> {
  when asset(control, total, window, offset) is {
    None -> None
    Some(next) -> {
      let next_offset = offset + next.cursor - control.cursor
      if or {
        next.stage == terminal,
        budget == 1,
        next_offset >= 4095,
      } {
        Some(next)
      } else {
        assets(next, total, window, next_offset, budget - 1)
      }
    }
  }
}
