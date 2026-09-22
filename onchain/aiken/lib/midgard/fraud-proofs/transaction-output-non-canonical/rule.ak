//// Narrow semantic engine for `transactionOutputNonCanonical`.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_output_scan_v1
use midgard/rejection_reason_v1.{OutputNonCanonical}

pub const outputs_field_index: Int = 2

pub const max_output_bytes: Int = 16384

pub const outcome_scanning: Int = 0

pub const outcome_canonical: Int = 1

pub const outcome_non_canonical: Int = 2

pub const scan_chunk_bytes: Int = 4095

pub const scan_window_bytes: Int = 8190

pub type BoundOutputV1 {
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
}

pub type OutputScanStateV1 {
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
  item_length: Int,
  item_hash: ByteArray,
  chunk_hashes: List<ByteArray>,
  control: ledger_output_scan_v1.LedgerOutputScanControlV1,
  outcome: Int,
}

pub fn bind_output_v1(
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
) -> BoundOutputV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect output_index >= 0
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        OutputNonCanonical { output_index },
      ),
    )
  } else {
    None
  }
  BoundOutputV1 { subject, output_index }
}

pub fn initial_scan_v1(
  bound: BoundOutputV1,
  item: ByteArray,
) -> OutputScanStateV1 {
  let item_length = bytearray.length(item)
  expect item_length <= max_output_bytes
  OutputScanStateV1 {
    subject: bound.subject,
    output_index: bound.output_index,
    item_length,
    item_hash: builtin.blake2b_256(item),
    chunk_hashes: chunk_hashes_v1(item, 0, item_length),
    control: ledger_output_scan_v1.initial_control_v1(),
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

pub fn state_is_well_formed_v1(state: OutputScanStateV1) -> Bool {
  and {
    substrate.subject_is_canonical_v1(state.subject),
    state.output_index >= 0,
    state.item_length >= 0,
    state.item_length <= max_output_bytes,
    bytearray.length(state.item_hash) == 32,
    list.length(state.chunk_hashes) == (
      state.item_length + scan_chunk_bytes - 1
    ) / scan_chunk_bytes,
    list.all(state.chunk_hashes, fn(hash) { bytearray.length(hash) == 32 }),
    ledger_output_scan_v1.control_is_well_formed(state.control),
    or {
      state.outcome == outcome_scanning,
      state.outcome == outcome_canonical,
      state.outcome == outcome_non_canonical,
    },
    if state.outcome == outcome_canonical {
      ledger_output_scan_v1.terminal_is_exact_v1(
        state.control,
        state.item_length,
      )
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
  state: OutputScanStateV1,
  window: ByteArray,
) -> OutputScanStateV1 {
  expect and {
      state_is_well_formed_v1(state),
      state.outcome == outcome_scanning,
    }
  let control = state.control
  let next = if and {
    control.stage == ledger_output_scan_v1.stage_optional_field,
    control.cursor == state.item_length,
  } {
    ledger_output_scan_v1.finish_v1(control, state.item_length)
  } else if control.cursor < state.item_length {
    let chunk_start = control.cursor - control.cursor % scan_chunk_bytes
    let chunk_index = control.cursor / scan_chunk_bytes
    let current_length = minimum(
      scan_chunk_bytes,
      state.item_length - chunk_start,
    )
    expect Some(current_hash) = list.at(state.chunk_hashes, chunk_index)
    expect
      builtin.blake2b_256(bytearray.slice(window, 0, current_length - 1)) == current_hash
    let token_stage = control.stage <= ledger_output_scan_v1.stage_optional_field
    let next_length = minimum(
      scan_chunk_bytes,
      state.item_length - chunk_start - current_length,
    )
    let window_length = current_length + if and {
      token_stage,
      next_length > 0,
    } {
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
    ledger_output_scan_v1.step_v1(
      control,
      state.item_length,
      window,
      control.cursor - chunk_start,
    )
  } else {
    None
  }
  when next is {
    None -> OutputScanStateV1 { ..state, outcome: outcome_non_canonical }
    Some(next_control) ->
      OutputScanStateV1 {
        ..state,
        control: next_control,
        outcome: if ledger_output_scan_v1.terminal_is_exact_v1(
          next_control,
          state.item_length,
        ) {
          outcome_canonical
        } else {
          outcome_scanning
        },
      }
  }
}

pub fn terminal_contradiction_v1(state: OutputScanStateV1) -> Bool {
  expect and {
      state_is_well_formed_v1(state),
      state.outcome != outcome_scanning,
    }
  substrate.terminal_contradiction_v1(
    state.subject,
    state.outcome == outcome_non_canonical,
  )
}

pub fn encode_bound_output_v1(bound: BoundOutputV1) -> ByteArray {
  cbor.serialise(bound)
}

pub fn encode_scan_state_v1(state: OutputScanStateV1) -> ByteArray {
  cbor.serialise(state)
}
