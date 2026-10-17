//// Narrow semantic engine for `mintItemNonCanonical`.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/mint_item_non_canonical/field_scan
use midgard/fraud_proofs/mint_item_non_canonical/scan
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

pub const mint_field_index: Int = 5

pub const max_item_bytes: Int = 32768

pub const outcome_scanning: Int = 0

pub const outcome_canonical: Int = 1

pub const outcome_non_canonical: Int = 2

pub const scan_chunk_bytes: Int = 4095

pub const scan_window_bytes: Int = 8190

pub type BoundItemV1 {
  subject: substrate.VerdictSubjectV1,
  item_index: Int,
  field_cursor: Option<field_scan.Cursor>,
}

pub type MintScanStateV1 {
  subject: substrate.VerdictSubjectV1,
  item_index: Int,
  item_length: Int,
  item_hash: ByteArray,
  chunk_hashes: List<ByteArray>,
  control: scan.Control,
  outcome: Int,
}

pub fn bind_item_v1(
  subject: substrate.VerdictSubjectV1,
  item_index: Int,
) -> BoundItemV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect item_index >= 0
  // Malformed mint currently has no typed rejection arm. Never reinterpret
  // a different family's forced rejection as this statement.
  expect subject.direction == substrate.direction_wrongful_acceptance
  BoundItemV1 { subject, item_index, field_cursor: None }
}

pub fn initial_scan_v1(
  bound: BoundItemV1,
  item: ByteArray,
  previous_policy: ByteArray,
) -> MintScanStateV1 {
  let item_length = bytearray.length(item)
  expect item_length > 0 && item_length <= max_item_bytes
  MintScanStateV1 {
    subject: bound.subject,
    item_index: bound.item_index,
    item_length,
    item_hash: builtin.blake2b_256(item),
    chunk_hashes: chunk_hashes_v1(item, 0, item_length),
    control: scan.initial(previous_policy),
    outcome: outcome_scanning,
  }
}

fn chunk_hashes_v1(item: ByteArray, offset: Int, total: Int) -> List<ByteArray> {
  if offset >= total {
    []
  } else {
    let length = minimum(scan_chunk_bytes, total - offset)
    [
      builtin.blake2b_256(bytearray.slice(item, offset, offset + length - 1)),
      ..chunk_hashes_v1(item, offset + length, total)
    ]
  }
}

pub fn state_is_well_formed_v1(state: MintScanStateV1) -> Bool {
  and {
    substrate.subject_is_canonical_v1(state.subject),
    state.item_index >= 0,
    state.item_length > 0,
    state.item_length <= max_item_bytes,
    bytearray.length(state.item_hash) == 32,
    list.length(state.chunk_hashes) == (
      state.item_length + scan_chunk_bytes - 1
    ) / scan_chunk_bytes,
    list.all(state.chunk_hashes, fn(hash) { bytearray.length(hash) == 32 }),
    state.control.stage >= 0 && state.control.stage <= scan.terminal,
    state.control.cursor >= 0 && state.control.cursor <= state.item_length,
    state.control.remaining >= 0,
    bytearray.length(state.control.previous_policy) == 0 || bytearray.length(
      state.control.previous_policy,
    ) == 28,
    or {
      state.outcome == outcome_scanning,
      state.outcome == outcome_canonical,
      state.outcome == outcome_non_canonical,
    },
    if state.outcome == outcome_canonical {
      state.control.stage == scan.terminal && state.control.cursor == state.item_length && state.control.remaining == 0
    } else {
      True
    },
  }
}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

pub fn advance_scan_v1(
  state: MintScanStateV1,
  window: ByteArray,
) -> MintScanStateV1 {
  expect and {
      state_is_well_formed_v1(state),
      state.outcome == outcome_scanning,
    }
  let control = state.control
  let next = if control.cursor < state.item_length {
    let chunk_start = control.cursor - control.cursor % scan_chunk_bytes
    let chunk_index = control.cursor / scan_chunk_bytes
    let current_length = minimum(
      scan_chunk_bytes,
      state.item_length - chunk_start,
    )
    expect Some(current_hash) = list.at(state.chunk_hashes, chunk_index)
    expect
      builtin.blake2b_256(bytearray.slice(window, 0, current_length - 1)) == current_hash
    let next_length = minimum(
      scan_chunk_bytes,
      state.item_length - chunk_start - current_length,
    )
    let window_length = current_length + if next_length > 0 {
      next_length
    } else {
      0
    }
    expect bytearray.length(window) == window_length
    expect _next_hash = if window_length > current_length {
      expect Some(next_hash) = list.at(state.chunk_hashes, chunk_index + 1)
      expect
        builtin.blake2b_256(
          bytearray.slice(window, current_length, window_length - 1),
        ) == next_hash
      next_hash
    } else {
      current_hash
    }
    scan.advance(
      control,
      state.item_length,
      window,
      control.cursor - chunk_start,
    )
  } else {
    None
  }
  when next is {
    None -> MintScanStateV1 { ..state, outcome: outcome_non_canonical }
    Some(next_control) ->
      MintScanStateV1 {
        ..state,
        control: next_control,
        outcome: if next_control.stage == scan.terminal {
          outcome_canonical
        } else {
          outcome_scanning
        },
      }
  }
}

pub fn terminal_contradiction_v1(state: MintScanStateV1) -> Bool {
  expect and {
      state_is_well_formed_v1(state),
      state.outcome != outcome_scanning,
    }
  substrate.terminal_contradiction_v1(
    state.subject,
    state.outcome == outcome_non_canonical,
  )
}

pub fn encode_bound_item_v1(bound: BoundItemV1) -> ByteArray {
  cbor.serialise(bound)
}

pub fn encode_scan_state_v1(state: MintScanStateV1) -> ByteArray {
  cbor.serialise(state)
}

/// A malformed predecessor is proved at its own coordinate; it cannot supply
/// a made-up key for an ordering accusation at the next coordinate.
pub fn previous_policy_v1(item: ByteArray) -> ByteArray {
  expect Some(policy) = scan.policy_id(item)
  policy
}
