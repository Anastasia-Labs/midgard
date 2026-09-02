//// Complete, checkpointed negation of `DuplicateInput` for forced leaves.

use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{DuplicateInput}

const checkpoint_domain: ByteArray =
  "midgard/fraud-proofs/input-set-uniqueness/checkpoint-v1"

pub type BoundDuplicateInputV1 {
  subject: substrate.VerdictSubjectV1,
  first_field_index: Int,
  first_item_index: Int,
  second_field_index: Int,
  second_item_index: Int,
}

pub type UniqueScanStateV1 {
  bound: BoundDuplicateInputV1,
  spend_count: Int,
  reference_count: Int,
  cursor: Int,
  previous_item: ByteArray,
  next_expected_script_hash: ByteArray,
  checkpoint_hash: ByteArray,
}

pub fn bind_forced_duplicate_reason_v1(
  subject: substrate.VerdictSubjectV1,
) -> BoundDuplicateInputV1 {
  expect subject.direction == substrate.direction_wrongful_rejection
  expect subject.source_kind == substrate.source_kind_forced
  expect
      DuplicateInput {
        first_field_index,
        first_item_index,
        second_field_index,
        second_item_index,
      }
    = substrate.rejection_reason_of_v1(subject)
  BoundDuplicateInputV1 {
    subject,
    first_field_index,
    first_item_index,
    second_field_index,
    second_item_index,
  }
}

fn coordinate_is_in_range(
  field_index: Int,
  item_index: Int,
  spend_count: Int,
  reference_count: Int,
) -> Bool {
  and {
    item_index >= 0,
    if field_index == 0 {
      item_index < spend_count
    } else if field_index == 1 {
      item_index < reference_count
    } else {
      False
    },
  }
}

fn coordinate_precedes(
  first_field: Int,
  first_item: Int,
  second_field: Int,
  second_item: Int,
) -> Bool {
  or {
    first_field < second_field,
    and {
      first_field == second_field,
      first_item < second_item,
    },
  }
}

pub fn checkpoint_v1(
  bound: BoundDuplicateInputV1,
  spend_count: Int,
  reference_count: Int,
  cursor: Int,
  previous_item: ByteArray,
  next_expected_script_hash: ByteArray,
) -> ByteArray {
  checkpoint_domain
    |> bytearray.concat(substrate.encode_verdict_subject_v1(bound.subject))
    |> bytearray.concat(cbor.serialise(bound.first_field_index))
    |> bytearray.concat(cbor.serialise(bound.first_item_index))
    |> bytearray.concat(cbor.serialise(bound.second_field_index))
    |> bytearray.concat(cbor.serialise(bound.second_item_index))
    |> bytearray.concat(cbor.serialise(spend_count))
    |> bytearray.concat(cbor.serialise(reference_count))
    |> bytearray.concat(cbor.serialise(cursor))
    |> bytearray.concat(encode_definite_bytes(previous_item))
    |> bytearray.concat(encode_definite_bytes(next_expected_script_hash))
    |> blake2b_256
}

fn state_with(
  bound: BoundDuplicateInputV1,
  spend_count: Int,
  reference_count: Int,
  cursor: Int,
  previous_item: ByteArray,
  next_expected_script_hash: ByteArray,
) -> UniqueScanStateV1 {
  UniqueScanStateV1 {
    bound,
    spend_count,
    reference_count,
    cursor,
    previous_item,
    next_expected_script_hash,
    checkpoint_hash: checkpoint_v1(
      bound,
      spend_count,
      reference_count,
      cursor,
      previous_item,
      next_expected_script_hash,
    ),
  }
}

pub fn initial_scan_state_v1(
  bound: BoundDuplicateInputV1,
  spend_count: Int,
  reference_count: Int,
  scan_script_hash: ByteArray,
) -> UniqueScanStateV1 {
  expect and {
      spend_count >= 0,
      reference_count >= 0,
      bytearray.length(scan_script_hash) == 28,
      coordinate_is_in_range(
        bound.first_field_index,
        bound.first_item_index,
        spend_count,
        reference_count,
      ),
      coordinate_is_in_range(
        bound.second_field_index,
        bound.second_item_index,
        spend_count,
        reference_count,
      ),
      coordinate_precedes(
        bound.first_field_index,
        bound.first_item_index,
        bound.second_field_index,
        bound.second_item_index,
      ),
    }
  state_with(bound, spend_count, reference_count, 0, #"", scan_script_hash)
}

pub fn scan_state_is_authentic_v1(state: UniqueScanStateV1) -> Bool {
  let total = state.spend_count + state.reference_count
  and {
    state.spend_count >= 0,
    state.reference_count >= 0,
    state.cursor >= 0,
    state.cursor <= total,
    bytearray.length(state.next_expected_script_hash) == 28,
    if state.cursor == 0 {
      state.previous_item == #""
    } else {
      bytearray.length(state.previous_item) == 38
    },
    state.checkpoint_hash == checkpoint_v1(
      state.bound,
      state.spend_count,
      state.reference_count,
      state.cursor,
      state.previous_item,
      state.next_expected_script_hash,
    ),
  }
}

pub fn advance_unique_scan_v1(
  state: UniqueScanStateV1,
  item: ByteArray,
  next_script_hash: ByteArray,
) -> UniqueScanStateV1 {
  // The spending validator authenticates the input checkpoint once before it
  // opens the field. Repeating the same hash here would add no binding and
  // doubles the hottest operation across every one-item continuation.
  expect state.cursor < state.spend_count + state.reference_count
  expect bytearray.length(item) == 38
  expect or {
      state.cursor == 0,
      bytearray.compare(state.previous_item, item) == Less,
    }
  state_with(
    state.bound,
    state.spend_count,
    state.reference_count,
    state.cursor + 1,
    item,
    next_script_hash,
  )
}

pub fn advance_unique_batch_v1(
  state: UniqueScanStateV1,
  items: List<ByteArray>,
  next_script_hash: ByteArray,
) -> UniqueScanStateV1 {
  let count = list.length(items)
  expect count > 0
  expect state.cursor + count <= state.spend_count + state.reference_count
  let Pair(previous_item, consumed) = list.foldl(
    items,
    Pair(state.previous_item, 0),
    fn(item, accumulator) {
      let Pair(previous, offset) = accumulator
      expect bytearray.length(item) == 38
      expect or {
          state.cursor + offset == 0,
          bytearray.compare(previous, item) == Less,
        }
      Pair(item, offset + 1)
    },
  )
  state_with(
    state.bound,
    state.spend_count,
    state.reference_count,
    state.cursor + consumed,
    previous_item,
    next_script_hash,
  )
}

pub fn unique_scan_is_complete_v1(state: UniqueScanStateV1) -> Bool {
  and {
    scan_state_is_authentic_v1(state),
    state.cursor == state.spend_count + state.reference_count,
    substrate.terminal_contradiction_v1(state.bound.subject, False),
  }
}
