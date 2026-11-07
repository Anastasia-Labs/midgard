//// The data step of `redeemerCanonicity`: a redeemer whose envelope is
//// canonical is canonical only when its data is canonical Data. Each test
//// drives the step the way a prover does, one transaction per call, and checks
//// the terminal polarity in both directions.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_stream_v1.{StreamFrameV1,
  StreamPaused} as stream
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/redeemer_canonicity/rule.{
  ContinueData, DataScanStateV1, DataStepV1, FinishData, TerminalStateV1,
}
use midgard/rejection_reason_v1.{RedeemerMalformed}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const witness_set_hash =
  #"202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f"

const max_redeemer_data_bytes: Int = 32743

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn forced_accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    ..accepted(),
    source_kind: substrate.source_kind_forced,
    source_key: #"01",
  }
}

fn rejected() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(RedeemerMalformed { redeemer_index: 0 }),
  }
}

fn repeat(bytes: ByteArray, count: Int) -> ByteArray {
  if count <= 0 {
    #""
  } else {
    bytearray.concat(bytes, repeat(bytes, count - 1))
  }
}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn definite_bytes(bytes: ByteArray) -> ByteArray {
  let length = bytearray.length(bytes)
  let head = if length < 24 {
    bytearray.push(#"", 0x40 + length)
  } else if length < 256 {
    bytearray.push(bytearray.push(#"", length), 0x58)
  } else {
    bytearray.push(
      bytearray.push(bytearray.push(#"", length % 256), length / 256),
      0x59,
    )
  }
  bytearray.concat(head, bytes)
}

/// A spend redeemer item carrying `data`, with a canonical envelope.
fn item_with(data: ByteArray) -> ByteArray {
  bytearray.concat(
    #"840000",
    bytearray.concat(definite_bytes(data), #"821a000f42401a3b9aca00"),
  )
}

/// The window a prover supplies: the data chunk holding the cursor, and the
/// next chunk while the data continues past it.
fn window_of(scan: DataScanStateV1, data: ByteArray) -> ByteArray {
  let length = bytearray.length(data)
  let base = minimum(scan.cursor, length - 1) / rule.data_chunk_bytes * rule.data_chunk_bytes
  let window_end = minimum(base + 2 * rule.data_chunk_bytes, length)
  bytearray.slice(data, base, window_end - 1)
}

fn base_of(scan: DataScanStateV1) -> Int {
  minimum(scan.cursor, scan.data_length - 1) / rule.data_chunk_bytes * rule.data_chunk_bytes
}

/// A prover's run: every call reveals at most `reveal` open containers and
/// reads at most `budget` nodes. Returns the terminal handed to step 03 and the
/// number of data-step transactions.
fn drive(
  step: DataStepV1,
  data: ByteArray,
  stack: List<StreamFrameV1>,
  reveal: Int,
  budget: Int,
  calls: Int,
) -> (TerminalStateV1, Int) {
  when step is {
    FinishData { terminal } -> (terminal, calls)
    ContinueData { scan } -> {
      let window = window_of(scan, data)
      let base = base_of(scan)
      let revealed = list.take(stack, reveal)
      let hidden = list.drop(stack, reveal)
      let tail_root = stream.stack_root_v1(hidden, stream.empty_stack_root)
      let next = rule.advance_data_v1(scan, window, revealed, tail_root, budget)
      when next is {
        FinishData { terminal } -> (terminal, calls + 1)
        ContinueData { .. } -> {
          expect StreamPaused { frames, nodes, .. } = stream.run_v1(
            window,
            base + bytearray.length(window) == scan.data_length,
            -base,
            scan.cursor - base,
            revealed,
            tail_root,
            budget,
          )
          expect nodes > 0 || revealed == []
          drive(
            next,
            data,
            list.concat(frames, hidden),
            reveal,
            budget,
            calls + 1,
          )
        }
      }
    }
  }
}

fn terminal(
  subject: substrate.VerdictSubjectV1,
  data: ByteArray,
  reveal: Int,
  budget: Int,
) -> (TerminalStateV1, Int) {
  let bound = rule.bind_redeemer_v1(subject, witness_set_hash, 0)
  drive(rule.open_item_v1(bound, item_with(data)), data, [], reveal, budget, 0)
}

/// The verdict at the terminal, whatever the prover's per-call budget and
/// reveal, and that it convicts exactly the direction that contradicts it.
fn verdict(data: ByteArray, canonical: Bool) -> Bool {
  let (whole, _) = terminal(accepted(), data, 1_000_000, 1_000_000)
  let (stepped, _) = terminal(rejected(), data, 1, 1)
  let (forced, _) = terminal(forced_accepted(), data, 4, 3)
  and {
    whole.canonical == canonical,
    stepped.canonical == canonical,
    forced.canonical == canonical,
    rule.terminal_contradiction_v1(whole) == !canonical,
    rule.terminal_contradiction_v1(forced) == !canonical,
    rule.terminal_contradiction_v1(stepped) == canonical,
  }
}

// ## Verdicts

test canonical_redeemer_is_canonical() {
  verdict(#"d8799f0102ff", True)
}

test non_minimal_integer_is_non_canonical() {
  verdict(#"1800", False)
}

test trailing_bytes_after_the_data_are_non_canonical() {
  verdict(#"0000", False)
}

test indefinite_map_at_depth_five_is_non_canonical() {
  verdict(#"9f9f9f9fbf0000ffffffffff", False)
}

test definite_non_empty_list_at_depth_five_is_non_canonical() {
  verdict(#"9f9f9f9f8100ffffffff", False)
}

test canonical_definite_maps_at_depth_five() {
  verdict(#"a100a100a100a100a10000", True)
}

test unterminated_list_is_non_canonical() {
  verdict(#"9f9f00ff", False)
}

// ## Long and deep data

/// A canonical list of 64-byte byte strings: nodes straddle every chunk
/// boundary of the span.
fn long_byte_strings(count: Int) -> ByteArray {
  bytearray.concat(
    #"9f",
    bytearray.concat(
      repeat(bytearray.concat(#"5840", repeat(#"ab", 64)), count),
      #"ff",
    ),
  )
}

test flaw_past_thirty_thousand_bytes_is_non_canonical() {
  let prefix = bytearray.take(long_byte_strings(460), 1 + 66 * 460)
  let data = bytearray.concat(prefix, #"1800ff")
  and {
    bytearray.length(prefix) > 30000,
    verdict(data, False),
  }
}

/// The largest redeemer data, canonical: a list of small integers.
fn largest_flat_data() -> ByteArray {
  bytearray.concat(
    #"9f",
    bytearray.concat(repeat(#"00", max_redeemer_data_bytes - 2), #"ff"),
  )
}

test largest_redeemer_is_canonical() {
  let data = largest_flat_data()
  let (forced, calls) = terminal(rejected(), data, 1_000_000, 4000)
  and {
    bytearray.length(data) == max_redeemer_data_bytes,
    forced.canonical,
    calls > 1,
    rule.terminal_contradiction_v1(forced),
  }
}

test largest_redeemer_false_acceptance_claim_refuses() fail {
  let (whole, _) = terminal(accepted(), largest_flat_data(), 1_000_000, 4000)
  rule.terminal_contradiction_v1(whole)
}

/// Single-pair maps nested to the span's end: the densest container stack.
fn largest_nested_maps() -> ByteArray {
  let pairs = ( max_redeemer_data_bytes - 1 ) / 2
  bytearray.concat(repeat(#"a1a0", pairs), #"a0")
}

test largest_nested_maps_are_canonical() {
  let data = largest_nested_maps()
  let (forced, _) = terminal(rejected(), data, 1_000_000, 20000)
  and {
    bytearray.length(data) == max_redeemer_data_bytes,
    forced.canonical,
    rule.terminal_contradiction_v1(forced),
  }
}

fn deep_lists(levels: Int) -> ByteArray {
  bytearray.concat(
    repeat(#"9f", levels),
    bytearray.concat(#"00", repeat(#"ff", levels)),
  )
}

test four_thousand_levels_are_canonical() {
  let (forced, _) = terminal(rejected(), deep_lists(4000), 600, 1500)
  and {
    forced.canonical,
    rule.terminal_contradiction_v1(forced),
  }
}

test four_thousand_levels_false_acceptance_claim_refuses() fail {
  let (whole, _) = terminal(accepted(), deep_lists(4000), 600, 1500)
  rule.terminal_contradiction_v1(whole)
}

test four_thousand_levels_missing_one_close_is_non_canonical() {
  let data = bytearray.concat(
    repeat(#"9f", 4000),
    bytearray.concat(#"00", repeat(#"ff", 3999)),
  )
  let (whole, _) = terminal(accepted(), data, 600, 1500)
  let (forced, _) = terminal(rejected(), data, 600, 1500)
  and {
    !whole.canonical,
    !forced.canonical,
    rule.terminal_contradiction_v1(whole),
    !rule.terminal_contradiction_v1(forced),
  }
}

// ## Refusals at the exact check

fn scan_of(data: ByteArray) -> DataScanStateV1 {
  let bound = rule.bind_redeemer_v1(accepted(), witness_set_hash, 0)
  expect ContinueData { scan } = rule.open_item_v1(bound, item_with(data))
  scan
}

/// The data step one node in: one list open, with its frame stored.
fn one_list_open() -> (DataScanStateV1, ByteArray) {
  let data = #"9f9f00ffff"
  let scan = scan_of(data)
  expect ContinueData { scan: next } = rule.advance_data_v1(
    scan,
    data,
    [],
    stream.empty_stack_root,
    1,
  )
  (next, data)
}

test stored_stack_opens_with_its_frames() {
  let (scan, data) = one_list_open()
  when
    rule.advance_data_v1(
      scan,
      data,
      [stream.ListFrame { started: False }],
      stream.empty_stack_root,
      1,
    )
  is {
    ContinueData { scan: next } -> next.cursor == scan.cursor + 1
    FinishData { .. } -> False
  }
}

test forged_open_container_refuses() fail {
  let (scan, data) = one_list_open()
  rule.advance_data_v1(
    scan,
    data,
    [stream.ListFrame { started: True }],
    stream.empty_stack_root,
    1,
  ) != ContinueData(scan)
}

test hidden_stack_claimed_empty_refuses() fail {
  let (scan, data) = one_list_open()
  rule.advance_data_v1(scan, data, [], stream.empty_stack_root, 1) != ContinueData(
    scan,
  )
}

test substituted_window_refuses() fail {
  let scan = scan_of(#"d8799f0102ff")
  rule.advance_data_v1(scan, #"d8799f0103ff", [], stream.empty_stack_root, 10) != ContinueData(
    scan,
  )
}

test window_missing_its_next_chunk_refuses() fail {
  let data = long_byte_strings(100)
  let scan = scan_of(data)
  rule.advance_data_v1(
    scan,
    bytearray.take(window_of(scan, data), rule.data_chunk_bytes),
    [],
    stream.empty_stack_root,
    1,
  ) != ContinueData(scan)
}

test window_carrying_extra_bytes_refuses() fail {
  let data = #"d8799f0102ff"
  let scan = scan_of(data)
  rule.advance_data_v1(
    scan,
    bytearray.concat(data, #"00"),
    [],
    stream.empty_stack_root,
    10,
  ) != ContinueData(scan)
}

/// The window crosses into the next chunk: the cursor chunk is authentic and
/// the bytes past the boundary are not the span's.
test window_with_substituted_next_chunk_refuses() fail {
  let data = long_byte_strings(100)
  let scan = scan_of(data)
  let window = window_of(scan, data)
  let first_next = bytearray.at(window, rule.data_chunk_bytes)
  let substituted = bytearray.concat(
    bytearray.take(window, rule.data_chunk_bytes),
    bytearray.push(
      bytearray.drop(window, rule.data_chunk_bytes + 1),
      ( first_next + 1 ) % 256,
    ),
  )
  rule.advance_data_v1(scan, substituted, [], stream.empty_stack_root, 1) != ContinueData(
    scan,
  )
}
