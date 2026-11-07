//// The datum step of `transactionOutputNonCanonical`: an output whose envelope
//// is canonical is canonical only when its inline datum is canonical Data.
//// Each test drives the scanning steps the way a prover does, one transaction
//// per call, and checks the terminal polarity in both directions.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_stream_v1.{StreamFrameV1,
  StreamPaused} as stream
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  ContinueDatum, DatumScanStateV1, FinishDatum, OutputScanStateV1,
}
use midgard/rejection_reason_v1.{OutputNonCanonical}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

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

fn rejected() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"d8799f581c000000000000000000000000000000000000000000000000000000001affffffff",
    rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
  }
}

/// A canonical output envelope carrying `datum` as its inline datum.
fn output_with(datum: ByteArray) -> ByteArray {
  encode_midgard_tx_output(
    MidgardTxOutput {
      ..fixture.sample_native_output(),
      datum_cbor: Some(datum),
    },
  )
}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn repeat(bytes: ByteArray, count: Int) -> ByteArray {
  if count <= 0 {
    #""
  } else {
    bytearray.concat(bytes, repeat(bytes, count - 1))
  }
}

/// The window the output phase reads: the chunk holding the cursor, and the
/// next chunk while the envelope's tokens are being read.
fn output_window(scan: OutputScanStateV1, output: ByteArray) -> ByteArray {
  let start = scan.control.cursor - scan.control.cursor % rule.scan_chunk_bytes
  let maximum = if scan.control.stage <= 4 {
    rule.scan_window_bytes
  } else {
    rule.scan_chunk_bytes
  }
  let length = minimum(bytearray.length(output) - start, maximum)
  bytearray.slice(output, start, start + length - 1)
}

/// The datum phase's window and the stream reader's view of it.
type DatumViewV1 {
  window: ByteArray,
  bytes: ByteArray,
  full: Bool,
  base: Int,
}

fn datum_view(datum: DatumScanStateV1, output: ByteArray) -> DatumViewV1 {
  let scan = datum.scan
  let item_length = bytearray.length(output)
  let datum_end = scan.control.datum_offset + scan.control.datum_length
  let base = minimum(datum.cursor, item_length - 1) / rule.scan_chunk_bytes * rule.scan_chunk_bytes
  let current_end = minimum(base + rule.scan_chunk_bytes, item_length)
  let window_end = if datum_end > current_end {
    minimum(current_end + rule.scan_chunk_bytes, item_length)
  } else {
    current_end
  }
  let window = bytearray.slice(output, base, window_end - 1)
  let full = window_end >= datum_end
  DatumViewV1 {
    window,
    bytes: if full {
      bytearray.take(window, datum_end - base)
    } else {
      window
    },
    full,
    base,
  }
}

/// The step a prover's thread sits on.
type PhaseV1 {
  OutputPhase { scan: OutputScanStateV1 }
  DatumPhase { datum: DatumScanStateV1 }
}

/// One output-scanning transaction, routed the way step 03 routes it.
fn output_step(scan: OutputScanStateV1, output: ByteArray) -> PhaseV1 {
  let next = rule.advance_scan_v1(scan, output_window(scan, output))
  if next.outcome == rule.outcome_scanning {
    OutputPhase(next)
  } else {
    when rule.open_datum_v1(next) is {
      Some(datum) -> DatumPhase(datum)
      None -> OutputPhase(next)
    }
  }
}

/// A prover's run: every datum call reveals at most `reveal` open containers
/// and reads at most `budget` nodes. Returns the terminal handed to step 04 and
/// the number of scanning transactions.
fn drive(
  phase: PhaseV1,
  output: ByteArray,
  stack: List<StreamFrameV1>,
  reveal: Int,
  budget: Int,
  calls: Int,
) -> (OutputScanStateV1, Int) {
  when phase is {
    OutputPhase { scan } ->
      if scan.outcome != rule.outcome_scanning {
        (scan, calls)
      } else {
        drive(output_step(scan, output), output, [], reveal, budget, calls + 1)
      }
    DatumPhase { datum } -> {
      let view = datum_view(datum, output)
      let revealed = list.take(stack, reveal)
      let hidden = list.drop(stack, reveal)
      let tail_root = stream.stack_root_v1(hidden, stream.empty_stack_root)
      when
        rule.advance_datum_v1(datum, view.window, revealed, tail_root, budget)
      is {
        FinishDatum(terminal) -> (terminal, calls + 1)
        ContinueDatum(next) -> {
          expect StreamPaused { frames, nodes, .. } = stream.run_v1(
            view.bytes,
            view.full,
            datum.scan.control.datum_offset - view.base,
            datum.cursor - view.base,
            revealed,
            tail_root,
            budget,
          )
          expect nodes > 0
          drive(
            DatumPhase(next),
            output,
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

fn start(subject: substrate.VerdictSubjectV1, output: ByteArray) -> PhaseV1 {
  OutputPhase(rule.initial_scan_v1(rule.bind_output_v1(subject, 0), output))
}

fn terminal(
  subject: substrate.VerdictSubjectV1,
  output: ByteArray,
  reveal: Int,
  budget: Int,
) -> OutputScanStateV1 {
  drive(start(subject, output), output, [], reveal, budget, 0).1st
}

/// The verdict at the terminal, and that it convicts exactly the direction
/// that contradicts it, whatever the prover's per-call budget and reveal.
fn verdict(datum: ByteArray, canonical: Bool) -> Bool {
  let output = output_with(datum)
  let expected = if canonical {
    rule.outcome_canonical
  } else {
    rule.outcome_non_canonical
  }
  let whole = terminal(accepted(), output, 1_000_000, 1_000_000)
  let stepped = terminal(rejected(), output, 1, 1)
  and {
    whole.outcome == expected,
    stepped.outcome == expected,
    rule.state_is_well_formed_v1(whole),
    rule.state_is_well_formed_v1(stepped),
  }
}

// ## Verdicts

test canonical_datum_is_canonical() {
  verdict(#"d8799f0102ff", True)
}

test non_canonical_datum_is_non_canonical() {
  verdict(#"9fff", False)
}

test trailing_bytes_after_the_datum_are_non_canonical() {
  verdict(#"0000", False)
}

test non_canonical_datum_at_depth_five() {
  verdict(#"9f9f9f9f9fffffffffff", False)
}

test unchunked_long_bytes_datum_is_non_canonical() {
  verdict(bytearray.concat(#"5864", repeat(#"11", 100)), False)
}

test canonical_definite_maps_at_depth_five() {
  verdict(#"a100a100a100a100a10000", True)
}

test output_without_a_datum_finishes_in_the_output_phase() {
  let output = encode_midgard_tx_output(fixture.sample_native_output())
  let (terminal, _calls) = drive(start(rejected(), output), output, [], 1, 1, 0)
  and {
    terminal.outcome == rule.outcome_canonical,
    terminal.control.datum_offset == -1,
    rule.terminal_contradiction_v1(terminal),
  }
}

// ## Terminal polarity in both directions

test forced_order_with_non_canonical_datum_rejection_stands() fail {
  rule.terminal_contradiction_v1(
    terminal(rejected(), output_with(#"d8799f00"), 1, 1),
  )
}

test forced_order_with_canonical_datum_false_rejection_convicts() {
  rule.terminal_contradiction_v1(
    terminal(rejected(), output_with(#"a100a100a100a100a10000"), 1, 1),
  )
}

test accepted_non_canonical_datum_convicts() {
  rule.terminal_contradiction_v1(
    terminal(accepted(), output_with(#"9f9f9f9f9fffffffffff"), 1, 1),
  )
}

test accepted_canonical_datum_refuses() fail {
  rule.terminal_contradiction_v1(
    terminal(accepted(), output_with(#"d8799f0102ff"), 1, 1),
  )
}

// ## Stage boundaries

/// A canonical list of 64-byte byte strings: nodes straddle every chunk
/// boundary of the item.
fn long_byte_strings(count: Int) -> ByteArray {
  bytearray.concat(
    #"9f",
    bytearray.concat(
      repeat(bytearray.concat(#"5840", repeat(#"ab", 64)), count),
      #"ff",
    ),
  )
}

test datum_crossing_chunk_boundaries_is_canonical() {
  let datum = long_byte_strings(200)
  let output = output_with(datum)
  and {
    bytearray.length(output) > 3 * rule.scan_chunk_bytes,
    verdict(datum, True),
  }
}

test fault_past_a_chunk_boundary_is_non_canonical() {
  let datum = bytearray.concat(
    bytearray.take(long_byte_strings(100), 1 + 66 * 100),
    #"5864",
  )
  verdict(bytearray.concat(datum, repeat(#"cd", 100)), False)
}

/// Twelve nested lists, read one node per call and revealing one container
/// per call: the open stack spans many transactions.
test container_stack_spanning_transactions() {
  let open = repeat(#"9f", 12)
  let close = repeat(#"ff", 12)
  and {
    verdict(bytearray.concat(open, bytearray.concat(#"00", close)), True),
    verdict(
      bytearray.concat(open, bytearray.concat(#"00", repeat(#"ff", 11))),
      False,
    ),
  }
}

/// A canonical datum whose end is the end of the item, at a chunk boundary,
/// closing nested maps after its last byte has been read.
fn exact_terminal_datum(length: Int) -> ByteArray {
  // a2 00 9f 00..00 ff 01 a1 00 a1 00 00
  let fixed = 1 + 1 + 1 + 1 + 1 + 5
  bytearray.concat(
    #"a2009f",
    bytearray.concat(repeat(#"00", length - fixed), #"ff01a100a10000"),
  )
}

test exact_terminal_datum_at_the_end_of_the_output() {
  let probe = output_with(exact_terminal_datum(300))
  let envelope = bytearray.length(probe) - 300
  let datum = exact_terminal_datum(2 * rule.scan_chunk_bytes - envelope)
  let output = output_with(datum)
  let last = terminal(rejected(), output, 1, 1)
  and {
    bytearray.length(output) == 2 * rule.scan_chunk_bytes,
    last.control.datum_offset + last.control.datum_length == bytearray.length(
      output,
    ),
    verdict(datum, True),
    rule.terminal_contradiction_v1(last),
  }
}

test largest_forced_datum_is_scanned() {
  let probe = output_with(repeat(#"00", 300))
  let envelope = bytearray.length(probe) - 300
  let items = rule.max_output_bytes - envelope - 2
  let datum = bytearray.concat(
    #"9f",
    bytearray.concat(repeat(#"00", items), #"ff"),
  )
  let output = output_with(datum)
  and {
    bytearray.length(output) == rule.max_output_bytes,
    terminal(rejected(), output, 1_000_000, 1_000_000).outcome == rule.outcome_canonical,
  }
}

// ## Refusals at the exact check

fn datum_phase_of(datum: ByteArray) -> (DatumScanStateV1, ByteArray) {
  let output = output_with(datum)
  (open_datum(start(accepted(), output), output), output)
}

/// Runs the output phase until it opens the datum step.
fn open_datum(phase: PhaseV1, output: ByteArray) -> DatumScanStateV1 {
  expect OutputPhase { scan } = phase
  when output_step(scan, output) is {
    DatumPhase { datum } -> datum
    next -> open_datum(next, output)
  }
}

/// The datum step one node in: one list open, with its frame stored.
fn one_list_open() -> (DatumScanStateV1, ByteArray) {
  let (datum, output) = datum_phase_of(#"9f9f00ffff")
  let view = datum_view(datum, output)
  expect ContinueDatum(next) = rule.advance_datum_v1(
    datum,
    view.window,
    [],
    stream.empty_stack_root,
    1,
  )
  (next, output)
}

test stored_stack_opens_with_its_frames() {
  let (datum, output) = one_list_open()
  let view = datum_view(datum, output)
  when
    rule.advance_datum_v1(
      datum,
      view.window,
      [stream.ListFrame { started: False }],
      stream.empty_stack_root,
      1,
    )
  is {
    ContinueDatum(next) -> next.cursor == datum.cursor + 1
    FinishDatum(_) -> False
  }
}

test forged_open_container_refuses() fail {
  let (datum, output) = one_list_open()
  let view = datum_view(datum, output)
  rule.advance_datum_v1(
    datum,
    view.window,
    [stream.ListFrame { started: True }],
    stream.empty_stack_root,
    1,
  ) != FinishDatum(datum.scan)
}

test hidden_stack_claimed_empty_refuses() fail {
  let (datum, output) = one_list_open()
  let view = datum_view(datum, output)
  rule.advance_datum_v1(datum, view.window, [], stream.empty_stack_root, 1) != FinishDatum(
    datum.scan,
  )
}

test substituted_datum_window_refuses() fail {
  let (datum, _output) = datum_phase_of(#"d8799f0102ff")
  let view = datum_view(datum, output_with(#"d8799f0103ff"))
  rule.advance_datum_v1(datum, view.window, [], stream.empty_stack_root, 10) != FinishDatum(
    datum.scan,
  )
}

test datum_window_missing_its_next_chunk_refuses() fail {
  let (datum, output) = datum_phase_of(long_byte_strings(100))
  let view = datum_view(datum, output)
  rule.advance_datum_v1(
    datum,
    bytearray.take(view.window, rule.scan_chunk_bytes),
    [],
    stream.empty_stack_root,
    1,
  ) != FinishDatum(datum.scan)
}

test envelope_without_datum_opens_no_datum_step() {
  let output = encode_midgard_tx_output(fixture.sample_native_output())
  let terminal = terminal(rejected(), output, 1, 1)
  rule.open_datum_v1(terminal) == None
}

test non_canonical_envelope_opens_no_datum_step() {
  let scan = OutputScanStateV1 {
    ..terminal(rejected(), output_with(#"00"), 1, 1),
    outcome: rule.outcome_non_canonical,
  }
  rule.open_datum_v1(scan) == None
}
