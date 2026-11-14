use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1} as dd_types
use midgard/fraud_proofs/execution_native_script_invalid/evaluate.{
  Finished, PageV1, Paged, PagedV1, SignerAbsent, SignerPresent, SignerQueryV1,
  SignerWitnessV1,
}
use midgard/fraud_proofs/execution_native_script_invalid/signers.{
  signer_leaf_v1,
}
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  EvalV1, Evaluated, FinalV1,
}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/native_script_scan_v1.{NativeScriptFrameV1, hash_frame_v1}
use midgard/validation_merkle_v1

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

const k_lo = #"01010101010101010101010101010101010101010101010101010101"

const k_a = #"11111111111111111111111111111111111111111111111111111111"

const k_x = #"1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a"

const k_b = #"22222222222222222222222222222222222222222222222222222222"

const k_c = #"33333333333333333333333333333333333333333333333333333333"

const k_hi = #"44444444444444444444444444444444444444444444444444444444"

// ## Native script encodings

fn sig(key: ByteArray) -> ByteArray {
  bytearray.concat(#"8200581c", key)
}

fn after(slot: Int) -> ByteArray {
  bytearray.concat(#"8204", cbor.serialise(slot))
}

fn before(slot: Int) -> ByteArray {
  bytearray.concat(#"8205", cbor.serialise(slot))
}

fn children(items: List<ByteArray>) -> ByteArray {
  list.foldl(
    items,
    encode_definite_array_header(list.length(items)),
    fn(item, acc) { bytearray.concat(acc, item) },
  )
}

fn all(items: List<ByteArray>) -> ByteArray {
  bytearray.concat(#"8201", children(items))
}

fn any(items: List<ByteArray>) -> ByteArray {
  bytearray.concat(#"8202", children(items))
}

fn at_least(required: Int, items: List<ByteArray>) -> ByteArray {
  #"8303"
    |> bytearray.concat(cbor.serialise(required))
    |> bytearray.concat(children(items))
}

/// The committed versioned-script item `[0, bytes(payload)]`.
fn item(payload: ByteArray) -> ByteArray {
  bytearray.concat(#"8200", encode_definite_bytes(payload))
}

// ## Chunk proofs of a one-chunk item

fn commitment_of(bytes: ByteArray) -> ByteArray {
  bounded_item_v1.from_bytes(6, 0, bytes)
}

fn chunk_of(bytes: ByteArray) -> ChunkProofV1 {
  ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 6,
    item_index: 0,
    total_length: bytearray.length(bytes),
    chunk_index: 0,
    chunk: bytes,
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: bounded_item_v1.hash_chunk(6, 0, 0, bytes),
      },
    ],
    siblings: [],
  }
}

// ## Signer frontier [k_a, k_b, k_c]

fn frontier() -> validation_merkle_v1.BuiltFrontier {
  validation_merkle_v1.build_frontier(
    [signer_leaf_v1(k_a), signer_leaf_v1(k_b), signer_leaf_v1(k_c)],
  )
}

fn witness_a() -> SignerWitnessV1 {
  SignerWitnessV1 { key_hash: k_a, siblings: [signer_leaf_v1(k_b)] }
}

fn witness_b() -> SignerWitnessV1 {
  SignerWitnessV1 { key_hash: k_b, siblings: [signer_leaf_v1(k_a)] }
}

fn witness_c() -> SignerWitnessV1 {
  SignerWitnessV1 { key_hash: k_c, siblings: [] }
}

fn coordinate() -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind: 0, index: 1 }
}

fn bound() -> BoundV1 {
  BoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: root,
      source_key: #"",
      rejection_reason: None,
    },
    compact_cbor: #"",
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: root,
  }
}

fn start(bytes: ByteArray, interval_start: Int, interval_end: Int) -> EvalV1 {
  let built = frontier()
  EvalV1 {
    bound: bound(),
    coordinate: coordinate(),
    item_commitment: commitment_of(bytes),
    validity_interval_start: interval_start,
    validity_interval_end: interval_end,
    signer_count: built.count,
    signer_peaks: built.peaks,
    total_length: 0,
    cursor: 0,
    stack_root: #"",
    stack_depth: 0,
    pending: None,
  }
}

fn page(
  chunks: List<ChunkProofV1>,
  frames: List<NativeScriptFrameV1>,
  queries: List<SignerQueryV1>,
  budget: Int,
) -> PageV1 {
  PageV1 {
    input_index: 0,
    output_index: 0,
    chunks,
    frames,
    signers: queries,
    budget,
  }
}

fn finished(result: PagedV1, satisfied: Bool) -> Bool {
  result == Finished {
    state: FinalV1 {
      bound: bound(),
      fact: Evaluated { coordinate: coordinate(), satisfied },
    },
  }
}

/// One page over the whole of a one-chunk source.
fn evaluate_once(
  payload: ByteArray,
  queries: List<SignerQueryV1>,
  interval_start: Int,
  interval_end: Int,
) -> PagedV1 {
  let bytes = item(payload)
  evaluate.page_v1(
    start(bytes, interval_start, interval_end),
    page([chunk_of(bytes)], [], queries, 64),
  )
}

fn holds(payload: ByteArray, queries: List<SignerQueryV1>) -> Bool {
  finished(evaluate_once(payload, queries, -1, -1), True)
}

fn fails(payload: ByteArray, queries: List<SignerQueryV1>) -> Bool {
  finished(evaluate_once(payload, queries, -1, -1), False)
}

// ## Signature leaves against the signer frontier

test signer_present_at_each_frontier_position() {
  and {
    holds(
      sig(k_a),
      [SignerPresent { position: 0, siblings: [signer_leaf_v1(k_b)] }],
    ),
    holds(
      sig(k_b),
      [SignerPresent { position: 1, siblings: [signer_leaf_v1(k_a)] }],
    ),
    holds(sig(k_c), [SignerPresent { position: 2, siblings: [] }]),
  }
}

test signer_absent_below_between_and_above() {
  and {
    fails(
      sig(k_lo),
      [SignerAbsent { position: 0, below: None, above: Some(witness_a()) }],
    ),
    fails(
      sig(k_x),
      [
        SignerAbsent {
          position: 1,
          below: Some(witness_a()),
          above: Some(witness_b()),
        },
      ],
    ),
    fails(
      sig(k_hi),
      [SignerAbsent { position: 3, below: Some(witness_c()), above: None }],
    ),
  }
}

test signer_absent_from_an_empty_signer_set() {
  let bytes = item(sig(k_a))
  let state = EvalV1 {
    ..start(bytes, -1, -1),
    signer_count: 0,
    signer_peaks: [],
  }
  finished(
    evaluate.page_v1(
      state,
      page(
        [chunk_of(bytes)],
        [],
        [SignerAbsent { position: 0, below: None, above: None }],
        8,
      ),
    ),
    False,
  )
}

test absent_signer_claimed_present_is_refused() fail {
  holds(
    sig(k_x),
    [SignerPresent { position: 1, siblings: [signer_leaf_v1(k_a)] }],
  )
}

test present_signer_claimed_absent_is_refused() fail {
  fails(
    sig(k_b),
    [
      SignerAbsent {
        position: 1,
        below: Some(witness_a()),
        above: Some(witness_b()),
      },
    ],
  )
}

test absence_skipping_a_present_signer_is_refused() fail {
  // k_b sits at position 1; a gap from k_a to k_c hides it.
  fails(
    sig(k_b),
    [
      SignerAbsent {
        position: 1,
        below: Some(witness_a()),
        above: Some(witness_c()),
      },
    ],
  )
}

test absence_with_an_unproved_lower_neighbour_is_refused() fail {
  fails(
    sig(k_x),
    [
      SignerAbsent {
        position: 1,
        below: Some(
          SignerWitnessV1 { key_hash: k_lo, siblings: [signer_leaf_v1(k_b)] },
        ),
        above: Some(witness_b()),
      },
    ],
  )
}

test absence_with_an_unproved_upper_neighbour_is_refused() fail {
  fails(
    sig(k_x),
    [
      SignerAbsent {
        position: 1,
        below: Some(witness_a()),
        above: Some(
          SignerWitnessV1 { key_hash: k_hi, siblings: [signer_leaf_v1(k_a)] },
        ),
      },
    ],
  )
}

test absence_below_the_first_signer_needs_position_zero() fail {
  fails(
    sig(k_x),
    [SignerAbsent { position: 1, below: None, above: Some(witness_b()) }],
  )
}

test absence_above_the_last_signer_needs_the_frontier_end() fail {
  fails(
    sig(k_x),
    [SignerAbsent { position: 1, below: Some(witness_a()), above: None }],
  )
}

test absence_with_an_upper_neighbour_below_the_key_is_refused() fail {
  // k_a is a real signer at position 0, but it orders below k_x.
  fails(
    sig(k_x),
    [SignerAbsent { position: 0, below: None, above: Some(witness_a()) }],
  )
}

test absence_with_a_lower_neighbour_equal_to_the_key_is_refused() fail {
  // k_c is the last signer; naming it as its own lower neighbour hides it.
  fails(
    sig(k_c),
    [SignerAbsent { position: 3, below: Some(witness_c()), above: None }],
  )
}

test absence_with_a_lower_neighbour_above_the_key_is_refused() fail {
  // k_c is a real signer at position 2, but it orders above k_b.
  fails(
    sig(k_b),
    [SignerAbsent { position: 3, below: Some(witness_c()), above: None }],
  )
}

// ## Timelock leaves against the validity interval

fn interval(payload: ByteArray, interval_start: Int, interval_end: Int) -> Bool {
  when evaluate_once(payload, [], interval_start, interval_end) is {
    Finished{state: FinalV1 { fact: Evaluated { satisfied, .. }, .. }} ->
      satisfied
    _ -> fail
  }
}

test after_holds_from_the_slot_on() {
  and {
    interval(after(100), 100, -1),
    interval(after(100), 101, -1),
    !interval(after(100), 99, -1),
    !interval(after(100), -1, 500),
    interval(after(0), 0, -1),
  }
}

test before_holds_up_to_the_slot() {
  and {
    interval(before(100), -1, 100),
    interval(before(100), -1, 99),
    !interval(before(100), -1, 101),
    !interval(before(100), 0, -1),
  }
}

// ## Containers

test containers_follow_all_any_and_at_least() {
  and {
    fails(
      all([sig(k_a), after(5)]),
      [SignerPresent { position: 0, siblings: [signer_leaf_v1(k_b)] }],
    ),
    interval(all([after(5), before(10)]), 5, 10),
    !interval(all([after(5), before(10)]), 5, 11),
    interval(any([after(50), before(10)]), 5, 10),
    !interval(any([]), 5, 10),
    interval(all([]), 5, 10),
    interval(at_least(0, []), 5, 10),
    interval(at_least(2, [after(50), after(1), before(10)]), 5, 10),
    !interval(at_least(3, [after(50), after(1), before(10)]), 5, 10),
  }
}

test nested_containers_consume_signer_answers_in_leaf_order() {
  finished(
    evaluate_once(
      all([any([sig(k_x), sig(k_c)]), at_least(1, [sig(k_lo), after(3)])]),
      [
        SignerAbsent {
          position: 1,
          below: Some(witness_a()),
          above: Some(witness_b()),
        },
        SignerPresent { position: 2, siblings: [] },
        SignerAbsent { position: 0, below: None, above: Some(witness_a()) },
      ],
      4,
      -1,
    ),
    True,
  )
}

// ## Paging across steps

fn top_frame(remaining: Int, valid_count: Int) -> NativeScriptFrameV1 {
  NativeScriptFrameV1 {
    tail: #"",
    kind: 1,
    child_count: 2,
    remaining,
    valid_count,
    required: 0,
  }
}

/// `all [sig k_a, after 5]` over four pages of one or two steps each: the
/// header and the container, the signature, the frame update, then the
/// timelock and the root.
fn paged(
  frame_after_signature: NativeScriptFrameV1,
  frame_at_root: NativeScriptFrameV1,
) -> PagedV1 {
  let bytes = item(all([sig(k_a), after(5)]))
  let proof = chunk_of(bytes)
  expect Paged { state: one } = evaluate.page_v1(
    start(bytes, 5, -1),
    page([proof], [], [], 1),
  )
  expect Paged { state: two } = evaluate.page_v1(
    one,
    page(
      [proof],
      [],
      [SignerPresent { position: 0, siblings: [signer_leaf_v1(k_b)] }],
      1,
    ),
  )
  expect Paged { state: three } = evaluate.page_v1(
    two,
    page([], [frame_after_signature], [], 1),
  )
  evaluate.page_v1(three, page([proof], [frame_at_root], [], 2))
}

test a_source_evaluates_across_pages_through_the_frame_stack() {
  finished(paged(top_frame(2, 0), top_frame(1, 1)), True)
}

test paged_state_commits_the_open_frame() {
  let bytes = item(all([sig(k_a), after(5)]))
  expect Paged { state } = evaluate.page_v1(
    start(bytes, 5, -1),
    page([chunk_of(bytes)], [], [], 1),
  )
  and {
    state.stack_depth == 1,
    state.stack_root == hash_frame_v1(top_frame(2, 0)),
    state.pending == None,
    state.total_length == bytearray.length(bytes),
  }
}

test a_carried_frame_the_root_does_not_commit_is_refused() fail {
  // The frame claims the signature already counted twice.
  finished(paged(top_frame(2, 1), top_frame(1, 1)), True)
}

test a_carried_frame_with_a_forged_count_is_refused() fail {
  finished(paged(top_frame(2, 0), top_frame(1, 0)), False)
}

// ## The source item

test a_tampered_chunk_is_refused() fail {
  let bytes = item(sig(k_x))
  let forged = item(sig(k_a))
  evaluate.page_v1(
    start(bytes, -1, -1),
    page(
      [ChunkProofV1 { ..chunk_of(bytes), chunk: forged }],
      [],
      [SignerPresent { position: 0, siblings: [signer_leaf_v1(k_b)] }],
      8,
    ),
  ) == Finished {
    state: FinalV1 {
      bound: bound(),
      fact: Evaluated { coordinate: coordinate(), satisfied: True },
    },
  }
}

test a_chunk_of_another_item_is_refused() fail {
  let bytes = item(sig(k_x))
  let other = item(sig(k_a))
  evaluate.page_v1(
    start(bytes, -1, -1),
    page(
      [chunk_of(other)],
      [],
      [SignerPresent { position: 0, siblings: [signer_leaf_v1(k_b)] }],
      8,
    ),
  ) == Finished {
    state: FinalV1 {
      bound: bound(),
      fact: Evaluated { coordinate: coordinate(), satisfied: True },
    },
  }
}

test bytes_after_the_root_script_are_refused() fail {
  let bytes = item(bytearray.concat(after(1), after(2)))
  finished(
    evaluate.page_v1(start(bytes, 5, -1), page([chunk_of(bytes)], [], [], 8)),
    True,
  )
}

test a_source_of_another_language_is_refused() fail {
  let bytes = bytearray.concat(#"8203", encode_definite_bytes(after(1)))
  finished(
    evaluate.page_v1(start(bytes, 5, -1), page([chunk_of(bytes)], [], [], 8)),
    True,
  )
}

test the_first_page_must_start_at_the_first_chunk() fail {
  let bytes = item(after(1))
  evaluate.page_v1(start(bytes, 5, -1), page([], [], [], 8)) == Paged {
    state: start(bytes, 5, -1),
  }
}

test a_signature_leaf_needs_an_answer() fail {
  holds(sig(k_a), [])
}

test a_zero_budget_page_is_refused() fail {
  let bytes = item(after(1))
  evaluate.page_v1(start(bytes, 5, -1), page([chunk_of(bytes)], [], [], 0)) == Paged {
    state: start(bytes, 5, -1),
  }
}

// ## A two-chunk source

/// `any` over enough `after` leaves to span two chunks; the last leaf decides.
fn wide(last: ByteArray) -> ByteArray {
  let filler = fixture.repeated_bytes_v1(after(1_000_000), 760)
  item(
    #"8202"
      |> bytearray.concat(encode_definite_array_header(761))
      |> bytearray.concat(filler)
      |> bytearray.concat(last),
  )
}

fn two_chunks(bytes: ByteArray) -> (ChunkProofV1, ChunkProofV1) {
  let total = bytearray.length(bytes)
  let first = bytearray.slice(bytes, 0, bounded_item_v1.chunk_bytes - 1)
  let second = bytearray.drop(bytes, bounded_item_v1.chunk_bytes)
  let h0 = bounded_item_v1.hash_chunk(6, 0, 0, first)
  let h1 = bounded_item_v1.hash_chunk(6, 0, 1, second)
  let built = validation_merkle_v1.build_frontier([h0, h1])
  (
    ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 6,
      item_index: 0,
      total_length: total,
      chunk_index: 0,
      chunk: first,
      frontier: built.peaks,
      siblings: [h1],
    },
    ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 6,
      item_index: 0,
      total_length: total,
      chunk_index: 1,
      chunk: second,
      frontier: built.peaks,
      siblings: [h0],
    },
  )
}

test a_two_chunk_source_evaluates_in_one_window() {
  let bytes = wide(after(3))
  let (c0, c1) = two_chunks(bytes)
  and {
    bytearray.length(bytes) > bounded_item_v1.chunk_bytes,
    finished(
      evaluate.page_v1(start(bytes, 5, -1), page([c0, c1], [], [], 2000)),
      True,
    ),
  }
}

test a_repeated_chunk_is_refused() fail {
  let bytes = wide(after(3))
  let (c0, _) = two_chunks(bytes)
  finished(
    evaluate.page_v1(start(bytes, 5, -1), page([c0, c0], [], [], 2000)),
    True,
  )
}

// ## A spent-output source: the item committed under its output's domain

test a_spent_output_source_evaluates_under_its_output_domain() {
  let bytes = item(all([sig(k_a), after(5)]))
  let chunk = ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 2,
    item_index: 5,
    total_length: bytearray.length(bytes),
    chunk_index: 0,
    chunk: bytes,
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: bounded_item_v1.hash_chunk(2, 5, 0, bytes),
      },
    ],
    siblings: [],
  }
  finished(
    evaluate.page_v1(
      EvalV1 {
        ..start(bytes, 6, -1),
        item_commitment: bounded_item_v1.from_bytes(2, 5, bytes),
      },
      page(
        [chunk],
        [],
        [SignerPresent { position: 0, siblings: [signer_leaf_v1(k_b)] }],
        64,
      ),
    ),
    True,
  )
}
