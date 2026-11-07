//// Narrow total rule for `redeemerCanonicity` (`00000028`).

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/canonical_cbor_scan_v1
use midgard/canonical_plutus_data_stream_v1.{
  StreamCanonical, StreamFrameV1, StreamNonCanonical, StreamPaused,
} as stream
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{RedeemerMalformed}

pub const redeemer_field_index: Int = 8

pub const data_chunk_bytes: Int = 4095

pub type BoundRedeemerV1 {
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  redeemer_index: Int,
}

pub type TerminalStateV1 {
  bound: BoundRedeemerV1,
  canonical: Bool,
}

/// `subject` is the value `open_subject_v1` returned in this Step 01 run, so it
/// is canonical by construction (see `substrate.opened_rejection_reason_v1`).
pub fn bind_redeemer_v1(
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  redeemer_index: Int,
) -> BoundRedeemerV1 {
  expect bytearray.length(witness_set_hash) == 32
  expect redeemer_index >= 0
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_opened_rejection_reason_v1(
        subject,
        RedeemerMalformed { redeemer_index },
      ),
    )
  } else {
    None
  }
  BoundRedeemerV1 { subject, witness_set_hash, redeemer_index }
}

fn supported_purpose(tag: Int) -> Bool {
  tag == 0 || tag == 1 || tag == 3 || tag == 6
}

fn head_then(
  item: ByteArray,
  offset: Int,
  major: Int,
  keep: fn(canonical_cbor_scan_v1.CborHeadV1) -> Bool,
  next: fn(canonical_cbor_scan_v1.CborHeadV1) -> Option<Pair<Int, Int>>,
) -> Option<Pair<Int, Int>> {
  when canonical_cbor_scan_v1.head_at_v1(item, offset, major) is {
    Some(head) ->
      if keep(head) {
        next(head)
      } else {
        None
      }
    None -> None
  }
}

fn any_head(_head: canonical_cbor_scan_v1.CborHeadV1) -> Bool {
  True
}

/// The redeemer's data span `(start, length)` within a field-8 item whose
/// envelope `[purpose, index, bytes(data), [mem, steps]]` is canonical and
/// ends exactly at the item's end; `None` for any other item, so malformed
/// adversarial bytes are a verdict rather than a failure. The data bytes
/// themselves are read by the data step.
pub fn data_span_v1(item: ByteArray) -> Option<Pair<Int, Int>> {
  let outer <- head_then(item, 0, 4, fn(head) { head.value == 4 })
  let purpose <- head_then(
    item,
    outer.next_offset,
    0,
    fn(head) { supported_purpose(head.value) },
  )
  let pointer <- head_then(item, purpose.next_offset, 0, any_head)
  let data <- head_then(item, pointer.next_offset, 2, any_head)
  let ex_units <- head_then(
    item,
    data.next_offset + data.value,
    4,
    fn(head) { head.value == 2 },
  )
  let memory <- head_then(item, ex_units.next_offset, 0, any_head)
  let _steps <- head_then(
    item,
    memory.next_offset,
    0,
    fn(head) { head.next_offset == bytearray.length(item) },
  )
  Some(Pair(data.next_offset, data.value))
}

/// The redeemer's data, read as canonical Data a bounded number of nodes per
/// transaction. `chunk_hashes` authenticate the span's `data_chunk_bytes`
/// chunks; `cursor` is the span offset of the next node; `stack_root` is the
/// hash chain of the open containers
/// (`canonical_plutus_data_stream_v1.stack_root_v1`).
pub type DataScanStateV1 {
  bound: BoundRedeemerV1,
  data_length: Int,
  chunk_hashes: List<ByteArray>,
  cursor: Int,
  stack_root: ByteArray,
}

/// Step 02's result: read the data on, or hand the verdict to the terminal.
pub type DataStepV1 {
  ContinueData { scan: DataScanStateV1 }
  FinishData { terminal: TerminalStateV1 }
}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn chunk_hashes_v1(span: ByteArray, offset: Int, total: Int) -> List<ByteArray> {
  if offset >= total {
    []
  } else {
    let length = minimum(data_chunk_bytes, total - offset)
    [
      builtin.blake2b_256(bytearray.slice(span, offset, offset + length - 1)),
      ..chunk_hashes_v1(span, offset + length, total)
    ]
  }
}

/// Opens the authenticated item: a malformed envelope or an empty data span is
/// final; otherwise the data step starts at the span's first byte.
pub fn open_item_v1(bound: BoundRedeemerV1, item: ByteArray) -> DataStepV1 {
  when data_span_v1(item) is {
    Some(Pair(start, length)) ->
      if length > 0 {
        ContinueData(
          DataScanStateV1 {
            bound,
            data_length: length,
            chunk_hashes: chunk_hashes_v1(
              builtin.slice_bytearray(start, length, item),
              0,
              length,
            ),
            cursor: 0,
            stack_root: stream.empty_stack_root,
          },
        )
      } else {
        FinishData(TerminalStateV1 { bound, canonical: False })
      }
    None -> FinishData(TerminalStateV1 { bound, canonical: False })
  }
}

/// Reads up to `node_budget` nodes of the data over `window`: the span chunk
/// holding the cursor, followed by the next chunk while the span continues
/// past it. `revealed` and `tail_root` open the stored container stack.
pub fn advance_data_v1(
  scan: DataScanStateV1,
  window: ByteArray,
  revealed: List<StreamFrameV1>,
  tail_root: ByteArray,
  node_budget: Int,
) -> DataStepV1 {
  let DataScanStateV1 { bound, data_length, chunk_hashes, cursor, stack_root } = scan
  expect stream.stack_root_v1(revealed, tail_root) == stack_root
  let chunk_index = minimum(cursor, data_length - 1) / data_chunk_bytes
  let base = chunk_index * data_chunk_bytes
  let current_length = minimum(data_chunk_bytes, data_length - base)
  let rest = data_length - base - current_length
  let next_length = minimum(data_chunk_bytes, rest)
  let window_length = current_length + next_length
  expect bytearray.length(window) == window_length
  expect [current_hash, ..later] = list.drop(chunk_hashes, chunk_index)
  expect
    builtin.blake2b_256(bytearray.slice(window, 0, current_length - 1)) == current_hash
  expect
    next_length == 0 || list.head(later) == Some(
      builtin.blake2b_256(
        bytearray.slice(window, current_length, window_length - 1),
      ),
    )
  when
    stream.run_v1(
      window,
      next_length == rest,
      -base,
      cursor - base,
      revealed,
      tail_root,
      node_budget,
    )
  is {
    StreamCanonical -> FinishData(TerminalStateV1 { bound, canonical: True })
    StreamNonCanonical ->
      FinishData(TerminalStateV1 { bound, canonical: False })
    StreamPaused { offset, frames, .. } ->
      ContinueData(
        DataScanStateV1 {
          ..scan,
          cursor: base + offset,
          stack_root: stream.stack_root_v1(frames, tail_root),
        },
      )
  }
}

pub fn terminal_contradiction_v1(state: TerminalStateV1) -> Bool {
  substrate.terminal_contradiction_v1(state.bound.subject, !state.canonical)
}

pub fn encode_bound_v1(bound: BoundRedeemerV1) -> ByteArray {
  cbor.serialise(bound)
}

pub fn encode_terminal_v1(state: TerminalStateV1) -> ByteArray {
  cbor.serialise(state)
}
