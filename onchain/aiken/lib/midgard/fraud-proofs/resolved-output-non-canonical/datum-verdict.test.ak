//// The datum step of `resolvedOutputNonCanonical`: a resolved output whose
//// envelope is canonical is canonical only when its inline datum is canonical
//// Data. Each test drives the reconstruction and the datum step the way a
//// prover does, one transaction per call, with chunk proofs against the
//// descriptor's item commitment, and checks the terminal polarity in both
//// directions.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/canonical_plutus_data_stream_v1.{StreamFrameV1,
  StreamPaused} as stream
use midgard/cek_data_v1
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{
  CanonicalVerdictV1, ContinueDatum, DatumStepV1, DatumVerdictStateV1,
  FinishDatum,
}
use midgard/ledger_output_commitment_v1.{LedgerOutputCommitmentV1}
use midgard/ledger_output_scan_v1.{LedgerOutputScanControlV1}
use midgard/rejection_reason_v1.{InputSpentOutputNonCanonical}
use midgard/validation_merkle_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h32 = #"2222222222222222222222222222222222222222222222222222222222222222"

/// `{0: address(29), 1: [5000000, {}]}` without its map header.
const envelope_fields =
  #"00581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

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
    source_key: #"01",
    rejection_reason: Some(
      InputSpentOutputNonCanonical { source_kind: 0, input_index: 0 },
    ),
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

/// A canonical ledger output envelope carrying `datum` as its inline datum.
fn output_with(datum: ByteArray) -> ByteArray {
  bytearray.concat(
    #"a3",
    bytearray.concat(
      envelope_fields,
      bytearray.concat(#"02", definite_bytes(datum)),
    ),
  )
}

fn output_without_datum() -> ByteArray {
  bytearray.concat(#"a2", envelope_fields)
}

/// The descriptor a prior block committed for `output` at index 0. Only the
/// item commitment, output index and total length reach the scan; the
/// remaining facts are well-formed placeholders.
fn descriptor(output: ByteArray) -> LedgerOutputCommitmentV1 {
  LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 0,
    total_length: bytearray.length(output),
    item_commitment: ledger_output_commitment_v1.output_item_commitment(
      0,
      output,
    ),
    address: #"6011111111111111111111111111111111111111111111111111111111",
    lovelace: 5000000,
    asset_count: 0,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
    cardano_value_size: 5,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: cek_data_v1.DataSummaryV1 {
      root: h32,
      cbor_length: 101,
      memory: 202,
    },
    midgard_tx_out: cek_data_v1.DataSummaryV1 {
      root: h32,
      cbor_length: 103,
      memory: 204,
    },
    cardano_spend_datum: cek_data_v1.DataSummaryV1 {
      root: h32,
      cbor_length: 3,
      memory: 4,
    },
  }
}

// ## Chunk proofs

fn chunk_of(output: ByteArray, chunk_index: Int) -> ByteArray {
  let total = bytearray.length(output)
  let start = chunk_index * bounded_item_v1.chunk_bytes
  let length = minimum(bounded_item_v1.chunk_bytes, total - start)
  builtin.slice_bytearray(start, length, output)
}

fn leaf(output: ByteArray, chunk_index: Int) -> ByteArray {
  bounded_item_v1.hash_chunk(
    ledger_output_commitment_v1.output_field_index,
    0,
    chunk_index,
    chunk_of(output, chunk_index),
  )
}

fn subtree_root(output: ByteArray, start: Int, size: Int) -> ByteArray {
  if size == 1 {
    leaf(output, start)
  } else {
    let half = size / 2
    validation_merkle_v1.hash_branch(
      subtree_root(output, start, half),
      subtree_root(output, start + half, half),
    )
  }
}

/// The bottom-up sibling path of leaf `local` in the perfect subtree of
/// `size` leaves starting at `start`.
fn path(output: ByteArray, start: Int, size: Int, local: Int) -> List<ByteArray> {
  if size == 1 {
    []
  } else {
    let half = size / 2
    if local < half {
      list.concat(
        path(output, start, half, local),
        [subtree_root(output, start + half, half)],
      )
    } else {
      list.concat(
        path(output, start + half, half, local - half),
        [subtree_root(output, start, half)],
      )
    }
  }
}

fn largest_power_at_most(value: Int, power: Int) -> Int {
  if power * 2 > value {
    power
  } else {
    largest_power_at_most(value, power * 2)
  }
}

/// The siblings of `chunk_index` within its frontier peak: peaks cover the
/// chunk count's binary digits from the highest down.
fn siblings(
  output: ByteArray,
  count: Int,
  chunk_index: Int,
  offset: Int,
) -> List<ByteArray> {
  let size = largest_power_at_most(count - offset, 1)
  if chunk_index < offset + size {
    path(output, offset, size, chunk_index - offset)
  } else {
    siblings(output, count, chunk_index, offset + size)
  }
}

fn proof_at(output: ByteArray, chunk_index: Int) -> ChunkProofV1 {
  let total = bytearray.length(output)
  let count = bounded_item_v1.chunk_count(total)
  let built = validation_merkle_v1.build_frontier(
    list.map(list.range(0, count - 1), fn(index) { leaf(output, index) }),
  )
  ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: ledger_output_commitment_v1.output_field_index,
    item_index: 0,
    total_length: total,
    chunk_index,
    chunk: chunk_of(output, chunk_index),
    frontier: built.peaks,
    siblings: siblings(output, count, chunk_index, 0),
  }
}

// ## The envelope phase

fn next_proof_of(output: ByteArray, chunk_index: Int) -> Option<ChunkProofV1> {
  if chunk_index + 1 < bounded_item_v1.chunk_count(bytearray.length(output)) {
    Some(proof_at(output, chunk_index + 1))
  } else {
    None
  }
}

/// Reconstructs the envelope the way step 04 does and returns what step 04
/// hands on: the datum step, or the verdict.
fn envelope(
  subject: substrate.VerdictSubjectV1,
  output: ByteArray,
  control: LedgerOutputScanControlV1,
) -> DatumStepV1 {
  let descriptor = descriptor(output)
  when ledger_output_scan_v1.finish_v1(control, descriptor.total_length) is {
    Some(_) ->
      rule.open_datum_v1(
        subject,
        descriptor,
        rule.finalize_canonical_v1(descriptor, control),
      )
    None -> {
      let chunk_index = control.cursor / bounded_item_v1.chunk_bytes
      when
        rule.advance_reconstruction_v1(
          descriptor,
          control,
          proof_at(output, chunk_index),
          next_proof_of(output, chunk_index),
        )
      is {
        Some(next) -> envelope(subject, output, next)
        None ->
          FinishDatum(
            CanonicalVerdictV1 { subject, output_is_non_canonical: True },
          )
      }
    }
  }
}

// ## The datum phase

fn chunk_index_of(state: DatumVerdictStateV1, output: ByteArray) -> Int {
  minimum(state.cursor, bytearray.length(output) - 1) / bounded_item_v1.chunk_bytes
}

/// The proofs a prover supplies: the chunk holding the cursor, and the next
/// chunk while the datum continues past it.
fn datum_proofs(
  state: DatumVerdictStateV1,
  output: ByteArray,
) -> (ChunkProofV1, Option<ChunkProofV1>) {
  let chunk_index = chunk_index_of(state, output)
  let current_end = chunk_index * bounded_item_v1.chunk_bytes + bytearray.length(
    chunk_of(output, chunk_index),
  )
  (
    proof_at(output, chunk_index),
    if state.datum_end > current_end {
      Some(proof_at(output, chunk_index + 1))
    } else {
      None
    },
  )
}

/// A prover's run: every call reveals at most `reveal` open containers and
/// reads at most `budget` nodes. Returns the verdict handed to step 05.
fn drive(
  step: DatumStepV1,
  output: ByteArray,
  stack: List<StreamFrameV1>,
  reveal: Int,
  budget: Int,
) -> CanonicalVerdictV1 {
  when step is {
    FinishDatum { verdict } -> verdict
    ContinueDatum { state } -> {
      let (current, next) = datum_proofs(state, output)
      let revealed = list.take(stack, reveal)
      let hidden = list.drop(stack, reveal)
      let tail_root = stream.stack_root_v1(hidden, stream.empty_stack_root)
      let after = rule.advance_datum_v1(
        state,
        current,
        next,
        revealed,
        tail_root,
        budget,
      )
      when after is {
        FinishDatum { verdict } -> verdict
        ContinueDatum { .. } -> {
          let base = chunk_index_of(state, output) * bounded_item_v1.chunk_bytes
          let window = when next is {
            None -> current.chunk
            Some(proof) -> bytearray.concat(current.chunk, proof.chunk)
          }
          let full = base + bytearray.length(window) >= state.datum_end
          expect StreamPaused { frames, .. } = stream.run_v1(
            if full {
              bytearray.take(window, state.datum_end - base)
            } else {
              window
            },
            full,
            state.datum_start - base,
            state.cursor - base,
            revealed,
            tail_root,
            budget,
          )
          drive(after, output, list.concat(frames, hidden), reveal, budget)
        }
      }
    }
  }
}

fn run(
  subject: substrate.VerdictSubjectV1,
  output: ByteArray,
  reveal: Int,
  budget: Int,
) -> CanonicalVerdictV1 {
  drive(
    envelope(subject, output, ledger_output_scan_v1.initial_control_v1()),
    output,
    [],
    reveal,
    budget,
  )
}

/// The verdict at the terminal, whatever the prover's per-call budget and
/// reveal, and that it convicts exactly the direction that contradicts it.
fn verdict(datum: ByteArray, canonical: Bool) -> Bool {
  let output = output_with(datum)
  let whole = run(accepted(), output, 1_000_000, 1_000_000)
  let stepped = run(rejected(), output, 1, 1)
  and {
    whole.output_is_non_canonical == !canonical,
    stepped.output_is_non_canonical == !canonical,
    rule.terminal_contradiction_v1(whole) == !canonical,
    rule.terminal_contradiction_v1(stepped) == canonical,
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

test indefinite_map_at_depth_five_is_non_canonical() {
  verdict(#"9f9f9f9fbf0000ffffffffff", False)
}

test definite_non_empty_list_at_depth_five_is_non_canonical() {
  verdict(#"9f9f9f9f8100ffffffff", False)
}

test canonical_definite_maps_at_depth_five() {
  verdict(#"a100a100a100a100a10000", True)
}

test output_without_a_datum_is_canonical_without_a_datum_step() {
  let output = output_without_datum()
  envelope(rejected(), output, ledger_output_scan_v1.initial_control_v1()) == FinishDatum(
    CanonicalVerdictV1 { subject: rejected(), output_is_non_canonical: False },
  )
}

test non_canonical_envelope_opens_no_datum_step() {
  let output = bytearray.concat(output_with(#"00"), #"00")
  envelope(accepted(), output, ledger_output_scan_v1.initial_control_v1()) == FinishDatum(
    CanonicalVerdictV1 { subject: accepted(), output_is_non_canonical: True },
  )
}

// ## Chunk boundaries

/// A canonical list of 64-byte byte strings: nodes straddle every chunk
/// boundary of the output.
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
  and {
    bytearray.length(output_with(datum)) > 3 * bounded_item_v1.chunk_bytes,
    verdict(datum, True),
  }
}

test fault_past_a_chunk_boundary_is_non_canonical() {
  let datum = bytearray.concat(
    bytearray.take(long_byte_strings(100), 1 + 66 * 100),
    #"1800ff",
  )
  verdict(datum, False)
}

test container_stack_spanning_transactions() {
  let open = repeat(#"9f", 12)
  and {
    verdict(
      bytearray.concat(open, bytearray.concat(#"00", repeat(#"ff", 12))),
      True,
    ),
    verdict(
      bytearray.concat(open, bytearray.concat(#"00", repeat(#"ff", 11))),
      False,
    ),
  }
}

// ## Refusals at the exact check

fn datum_state_of(datum: ByteArray) -> (DatumVerdictStateV1, ByteArray) {
  let output = output_with(datum)
  expect ContinueDatum { state } = envelope(
    accepted(),
    output,
    ledger_output_scan_v1.initial_control_v1(),
  )
  (state, output)
}

/// The datum step one node in: one list open, with its frame stored.
fn one_list_open() -> (DatumVerdictStateV1, ByteArray) {
  let (state, output) = datum_state_of(#"9f9f00ffff")
  let (current, next) = datum_proofs(state, output)
  expect ContinueDatum { state: after } = rule.advance_datum_v1(
    state,
    current,
    next,
    [],
    stream.empty_stack_root,
    1,
  )
  (after, output)
}

test stored_stack_opens_with_its_frames() {
  let (state, output) = one_list_open()
  let (current, next) = datum_proofs(state, output)
  when
    rule.advance_datum_v1(
      state,
      current,
      next,
      [stream.ListFrame { started: False }],
      stream.empty_stack_root,
      1,
    )
  is {
    ContinueDatum { state: after } -> after.cursor == state.cursor + 1
    FinishDatum { .. } -> False
  }
}

test forged_open_container_refuses() fail {
  let (state, output) = one_list_open()
  let (current, next) = datum_proofs(state, output)
  rule.advance_datum_v1(
    state,
    current,
    next,
    [stream.ListFrame { started: True }],
    stream.empty_stack_root,
    1,
  ) != ContinueDatum(state)
}

test hidden_stack_claimed_empty_refuses() fail {
  let (state, output) = one_list_open()
  let (current, next) = datum_proofs(state, output)
  rule.advance_datum_v1(state, current, next, [], stream.empty_stack_root, 1) != ContinueDatum(
    state,
  )
}

test substituted_datum_chunk_refuses() fail {
  let (state, _output) = datum_state_of(#"d8799f0102ff")
  let forged = output_with(#"d8799f0103ff")
  rule.advance_datum_v1(
    state,
    proof_at(forged, 0),
    None,
    [],
    stream.empty_stack_root,
    10,
  ) != ContinueDatum(state)
}

test chunk_other_than_the_cursor_chunk_refuses() fail {
  let datum = long_byte_strings(100)
  let (state, output) = datum_state_of(datum)
  rule.advance_datum_v1(
    state,
    proof_at(output, 1),
    Some(proof_at(output, 2)),
    [],
    stream.empty_stack_root,
    1,
  ) != ContinueDatum(state)
}

test datum_missing_its_next_chunk_refuses() fail {
  let (state, output) = datum_state_of(long_byte_strings(100))
  rule.advance_datum_v1(
    state,
    proof_at(output, 0),
    None,
    [],
    stream.empty_stack_root,
    1,
  ) != ContinueDatum(state)
}

test next_chunk_past_the_datum_refuses() fail {
  // The datum ends in the first chunk; trailing padding fills a second one.
  let datum = #"d8799f0102ff"
  let output = output_with(datum)
  let (state, _) = datum_state_of(datum)
  let padded = bytearray.concat(output, repeat(#"00", 5000))
  let padded_state = DatumVerdictStateV1 {
    ..state,
    item_commitment: ledger_output_commitment_v1.output_item_commitment(
      0,
      padded,
    ),
  }
  rule.advance_datum_v1(
    padded_state,
    proof_at(padded, 0),
    Some(proof_at(padded, 1)),
    [],
    stream.empty_stack_root,
    10,
  ) != ContinueDatum(padded_state)
}

test next_chunk_that_is_not_the_successor_refuses() fail {
  let (state, output) = datum_state_of(long_byte_strings(200))
  rule.advance_datum_v1(
    state,
    proof_at(output, 0),
    Some(proof_at(output, 2)),
    [],
    stream.empty_stack_root,
    1,
  ) != ContinueDatum(state)
}
