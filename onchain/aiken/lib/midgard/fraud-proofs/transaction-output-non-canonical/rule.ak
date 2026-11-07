//// Narrow semantic engine for `transactionOutputNonCanonical`.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_stream_v1.{
  StreamCanonical, StreamFrameV1, StreamNonCanonical, StreamPaused,
} as stream
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

/// The inline datum of an output whose envelope is canonical, read as
/// canonical Data a bounded number of nodes per transaction. `cursor` is the
/// item offset of the next node; `stack_root` is the hash chain of the open
/// containers (`canonical_plutus_data_stream_v1.stack_root_v1`).
pub type DatumScanStateV1 {
  scan: OutputScanStateV1,
  cursor: Int,
  stack_root: ByteArray,
}

/// One datum step's result: read on, or hand the verdict to the terminal.
pub type DatumStepV1 {
  ContinueDatum { datum: DatumScanStateV1 }
  FinishDatum { scan: OutputScanStateV1 }
}

/// `subject` is the value `open_subject_v1` returned in this Step 01 run, so it
/// is canonical by construction (see `substrate.opened_rejection_reason_v1`).
pub fn bind_output_v1(
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
) -> BoundOutputV1 {
  expect output_index >= 0
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_opened_rejection_reason_v1(
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

/// The datum step's first state once the envelope scan reaches a canonical
/// terminal with an inline datum; `None` when the terminal is final.
pub fn open_datum_v1(scan: OutputScanStateV1) -> Option<DatumScanStateV1> {
  if and {
    scan.outcome == outcome_canonical,
    scan.control.datum_offset >= 0,
  } {
    Some(
      DatumScanStateV1 {
        scan,
        cursor: scan.control.datum_offset,
        stack_root: stream.empty_stack_root,
      },
    )
  } else {
    None
  }
}

/// Reads up to `node_budget` nodes of the datum over `window`: the item chunk
/// holding the cursor, followed by the next chunk while the datum continues
/// past it. `revealed` and `tail_root` open the stored container stack.
pub fn advance_datum_v1(
  datum: DatumScanStateV1,
  window: ByteArray,
  revealed: List<StreamFrameV1>,
  tail_root: ByteArray,
  node_budget: Int,
) -> DatumStepV1 {
  let DatumScanStateV1 { scan, cursor, stack_root } = datum
  expect stream.stack_root_v1(revealed, tail_root) == stack_root
  let datum_start = scan.control.datum_offset
  let datum_end = datum_start + scan.control.datum_length
  let chunk_index = minimum(cursor, scan.item_length - 1) / scan_chunk_bytes
  let base = chunk_index * scan_chunk_bytes
  let current_length = minimum(scan_chunk_bytes, scan.item_length - base)
  let next_length = if datum_end > base + current_length {
    minimum(scan_chunk_bytes, scan.item_length - base - current_length)
  } else {
    0
  }
  let window_length = current_length + next_length
  expect bytearray.length(window) == window_length
  expect Some(current_hash) = list.at(scan.chunk_hashes, chunk_index)
  expect
    builtin.blake2b_256(bytearray.slice(window, 0, current_length - 1)) == current_hash
  expect
    next_length == 0 || list.at(scan.chunk_hashes, chunk_index + 1) == Some(
      builtin.blake2b_256(
        bytearray.slice(window, current_length, window_length - 1),
      ),
    )
  let full = base + window_length >= datum_end
  let bytes = if full {
    bytearray.take(window, datum_end - base)
  } else {
    window
  }
  when
    stream.run_v1(
      bytes,
      full,
      datum_start - base,
      cursor - base,
      revealed,
      tail_root,
      node_budget,
    )
  is {
    StreamCanonical -> FinishDatum(scan)
    StreamNonCanonical ->
      FinishDatum(OutputScanStateV1 { ..scan, outcome: outcome_non_canonical })
    StreamPaused { offset, frames, .. } ->
      ContinueDatum(
        DatumScanStateV1 {
          scan,
          cursor: base + offset,
          stack_root: stream.stack_root_v1(frames, tail_root),
        },
      )
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
