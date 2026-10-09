use aiken/interval.{Finite, Interval, IntervalBound, between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use env
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/ledger_state
use midgard/state_queue

const non_empty_root =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h28 = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

fn empty_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: env.empty_merkle_tree_root,
    utxos_root: env.empty_merkle_tree_root,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 1_000,
    end_time: 2_000,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28,
    operator_vkey: h28,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn header_hash(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  utils.serialise_and_hash_28(header_data)
}

test state_queue_header_decoder_accepts_exactly_protocol_v1() {
  let header = empty_header()
  state_queue.decode_header_view(header) == header
}

test state_queue_header_decoder_rejects_the_genesis_protocol_sentinel() fail {
  let header = ledger_state.HeaderV1 {
    ..empty_header(),
    protocol_version: ledger_state.genesis_protocol_version,
  }
  state_queue.decode_header_view(header) == header
}

test state_queue_header_decoder_rejects_an_unknown_future_protocol() fail {
  let header = ledger_state.HeaderV1 {
    ..empty_header(),
    protocol_version: ledger_state.protocol_version_v1 + 1,
  }
  state_queue.decode_header_view(header) == header
}

test committed_header_identity_binds_each_admission_count_and_root() {
  let header = empty_header()
  let expected_hash = header_hash(header)
  and {
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, withdrawal_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, forced_transaction_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, l2_transaction_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, deposit_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, total_event_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, transition_step_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, validation_trace_count: 1 },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, withdrawals_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 {
        ..header,
        forced_transactions_root: non_empty_root,
      },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, transactions_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, deposits_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, transition_trace_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, event_to_step_root: non_empty_root },
    ),
    expected_hash != header_hash(
      ledger_state.HeaderV1 { ..header, validation_traces_root: non_empty_root },
    ),
  }
}

// ---------------------------------------------------------------------------
// Q60 / D-S12 — the commit `end_time` bound.
//
// `state_queue.commit_bound_header_time_is_valid` anchors a committed header's
// `end_time` to the *inclusive upper bound* of the commit transaction's short
// validity interval. Acceptance is therefore equality with a single point, not
// membership in a range, and the accepted point moves only when the commit
// transaction's own window moves.
//
// Every test below is named `state_queue_commit_end_time_*` on purpose. Aiken's
// `-m` selector is a glob over *test names*, not module names, so the
// `state_queue` selector this family is gated by collects a test only when
// `state_queue` appears in the test's own name. The five originally
// `commit_header_time_*`-named tests were invisible to that gate.
// ---------------------------------------------------------------------------

test state_queue_commit_end_time_accepts_the_normalized_commit_upper_bound() {
  state_queue.commit_bound_header_time_is_valid(
    1_000,
    2_000,
    Interval {
      lower_bound: IntervalBound {
        bound_type: Finite(1_900),
        is_inclusive: True,
      },
      upper_bound: IntervalBound {
        bound_type: Finite(2_001),
        is_inclusive: False,
      },
    },
  )
}

test state_queue_commit_end_time_rejects_a_backdated_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_000,
    1_500,
    between(1_900, 2_000),
  )
}

test state_queue_commit_end_time_rejects_an_overlong_validity_range() fail {
  state_queue.commit_bound_header_time_is_valid(
    1,
    env.max_validity_range_length + 1,
    between(0, env.max_validity_range_length + 1),
  )
}

test state_queue_commit_end_time_rejects_a_future_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_000,
    2_500,
    between(1_900, 2_000),
  )
}

test state_queue_commit_end_time_rejects_a_non_advancing_event_interval() {
  !state_queue.commit_bound_header_time_is_valid(
    2_000,
    2_000,
    between(1_900, 2_000),
  )
}

// ---------------------------------------------------------------------------
// Q60 adjacent-bound fixtures.
//
// All five boundary cases share one absolute commit window so the accepted
// value and its neighbours are directly comparable:
//
//   inclusive lower bound  1_700_000_000_000
//   inclusive upper bound  1_700_000_480_000  (lower + 480_000)
//
// 1_700_000_480_000 is therefore both the only accepted `end_time` for this
// window and the largest `end_time` any commit opening at 1_700_000_000_000
// can carry, because a longer interval is not a short validity range at all.
//
// Four of the five probe the bound from at or above it. That is not enough on
// its own: a rule reading `lower <= end_time <= upper` would agree with every
// one of them, so a family built only from those four cannot tell the anchor
// apart from interval membership. Boundary 5 probes from strictly *inside* the
// window, and is the fixture that separates the two.
// ---------------------------------------------------------------------------

/// The absolute fixtures in this family are the adjacent bounds of the accepted
/// `end_time` only while the maximum short-range length is 480_000 ms. Pin it,
/// so widening the range fails here instead of silently redefining what
/// "the maximum accepted end_time" means.
test state_queue_commit_end_time_maximum_range_length_is_pinned() {
  env.max_validity_range_length == 480_000
}

/// Boundary 1 of 5 — lower-bound control. The window's *lower* bound does not
/// gate acceptance: moving it 479_000 ms later, so the interval is 1_000 ms
/// long instead of the maximal 480_000, leaves the same header times accepted.
/// Only the upper bound decides.
///
/// That is a statement about which bound is consulted, not a proof that the
/// accepted set is a single point: an interval-membership rule accepts this
/// fixture too. Boundary 5 is what excludes membership.
///
/// The lower bound cannot be moved all the way onto the upper bound: a
/// zero-length interval is not a closed range to
/// `utils.get_inclusive_bounds_of_a_short_validity_range`, which aborts before
/// the anchor is ever consulted.
test state_queue_commit_end_time_lower_bound_control_accepts_the_anchor() {
  state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    1_700_000_480_000,
    between(1_700_000_479_000, 1_700_000_480_000),
  )
}

/// Boundary 2 of 5 — the maximum accepted `end_time`: the inclusive upper bound
/// of the longest short validity range opening at 1_700_000_000_000.
test state_queue_commit_end_time_maximum_accepted_is_the_inclusive_upper_bound() {
  state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    1_700_000_480_000,
    between(1_700_000_000_000, 1_700_000_480_000),
  )
}

/// Boundary 3 of 5 — one millisecond above the bound is already rejected.
test state_queue_commit_end_time_rejects_one_millisecond_above_the_bound() {
  !state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    1_700_000_480_001,
    between(1_700_000_000_000, 1_700_000_480_000),
  )
}

/// Boundary 4 of 5 — the D-S12 far-future `end_time`, ten years (315_360_000_000
/// ms) past the window that commits it.
test state_queue_commit_end_time_rejects_a_far_future_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    2_015_360_480_000,
    between(1_700_000_000_000, 1_700_000_480_000),
  )
}

/// Boundary 5 of 5 — strictly *inside* the commit window, and the one fixture
/// that makes this family a proof of equality rather than of membership.
///
/// 1_700_000_479_000 sits 479_000 ms above the inclusive lower bound and
/// 1_000 ms below the inclusive upper bound, so `lower <= end_time <= upper`
/// holds of it and the rule refuses it anyway. Boundaries 1-4 all probe the
/// bound from at or above it, so an interval-membership rule agrees with every
/// one of them; it disagrees here.
///
/// Measured, not asserted. With `header_end_time == inclusive_commit_upper_bound`
/// replaced by `lower <= header_end_time <= upper` on a scratch copy of this
/// project, this is the test — and, with the sweep below, the only test — that
/// fails. The Q60 evidence artifact's `mutationExperiment` records both arms of
/// that run with their exact counts.
test state_queue_commit_end_time_rejects_an_end_strictly_inside_the_commit_window() {
  !state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    1_700_000_479_000,
    between(1_700_000_000_000, 1_700_000_480_000),
  )
}

/// Nothing past the bound is accepted at any distance. This pins the accepted
/// set from above only — an interval-membership rule passes this sweep
/// unchanged — so it is the companion sweep below, not this one, that shows the
/// accepted set is the single point at the bound.
test state_queue_commit_end_time_never_accepts_any_value_past_the_bound() {
  and {
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      1_700_000_480_001,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      1_700_000_480_010,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      1_700_001_000_000,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      2_015_360_480_000,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
  }
}

/// Nothing strictly *inside* the window is accepted either, at any depth: one
/// millisecond above the inclusive lower bound, the midpoint, and one
/// millisecond below the inclusive upper bound are all refused. The sweep above
/// pins the accepted set from above; this one pins it from below, and together
/// they leave exactly the single point at the upper bound.
///
/// Every case here is strictly interior, so each is refused by the real rule
/// and accepted by an interval-membership rule.
test state_queue_commit_end_time_never_accepts_any_value_inside_the_window() {
  and {
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      1_700_000_000_001,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      1_700_000_240_000,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      1_700_000_479_999,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
  }
}

// ---------------------------------------------------------------------------
// Q60 due-ness adjudication.
//
// D-S12's concrete harm is that a far-future `end_time` widens the window in
// which an L1 event counts as "due" for the block being committed, so an
// operator could sweep events that have not happened yet into their block — or
// be convicted for omitting them. Due-ness is decided by `timed_l1_event_is_due`
// in `lib/midgard/fraud-proofs/transition-trace/proof.ak:1294`, reached from the
// deposit (L1450), withdrawal (L1474) and forced-transaction (L1500) branches
// of `validate_omitted_due_l1_event`.
//
// Read the two tests below for exactly what they are. That predicate is
// module-private and cannot be imported here, so these tests run a *local
// mirror* of it: a hand transcription that lives in this file. They document
// the harm model. They do NOT execute proof.ak, and nothing here can detect the
// mirror drifting from the original — only the invalidation trigger recorded
// in the Q60 evidence artifact covers that.
//
// The other half of the argument — that the far-future `end_time` which would
// make these events due is refused at commit time — is proved against the real
// predicate by `state_queue_commit_end_time_rejects_a_far_future_end` and
// `state_queue_commit_end_time_maximum_accepted_is_the_inclusive_upper_bound`.
// Each test below used to restate those two calls on byte-identical fixtures;
// the restatements are gone, because a duplicated call adds no failure mode the
// original did not already have.
// ---------------------------------------------------------------------------

/// Mirror of `proof.ak:1294 timed_l1_event_is_due`.
fn timed_l1_event_is_due_mirror(
  header_start_time: Int,
  header_end_time: Int,
  inclusion_time: Int,
) -> Bool {
  and {
    header_start_time < inclusion_time,
    inclusion_time <= header_end_time,
  }
}

/// Due-event class 1 of 2 — deposit (`proof.ak:1450`), over the local mirror.
/// A forced transaction order is due on the same inclusion window alone, so
/// this case covers it too: a deposit that becomes includable one millisecond after the maximum accepted
/// `end_time` is not due for this block, and would be due under a far-future
/// `end_time`. Documents the harm; does not execute `proof.ak`.
test state_queue_commit_end_time_bound_keeps_a_future_deposit_not_due() {
  and {
    !timed_l1_event_is_due_mirror(
      1_700_000_000_000,
      1_700_000_480_000,
      1_700_000_480_001,
    ),
    timed_l1_event_is_due_mirror(
      1_700_000_000_000,
      2_015_360_480_000,
      1_700_000_480_001,
    ),
  }
}

/// Due-event class 2 of 2 — withdrawal (`proof.ak:1474`), over the local
/// mirror, taken a full year past the commit window to show the gap the anchor
/// closes is unbounded rather than an off-by-one. Documents the harm; does not
/// execute `proof.ak`.
test state_queue_commit_end_time_bound_keeps_a_future_withdrawal_not_due() {
  and {
    !timed_l1_event_is_due_mirror(
      1_700_000_000_000,
      1_700_000_480_000,
      1_731_536_480_000,
    ),
    timed_l1_event_is_due_mirror(
      1_700_000_000_000,
      2_015_360_480_000,
      1_731_536_480_000,
    ),
  }
}

/// Queue and merge liveness control. Committing the maximum accepted `end_time`
/// does not stall anything: each successor commit opens exactly where its
/// predecessor closed and re-anchors on its own window, and the block matures a
/// finite `block_maturity_duration_v1` after the `end_time` the anchor pinned —
/// the deadline the rejected far-future `end_time` would have pushed ten years
/// out, which is the stall half of D-S12.
///
/// Only two things here can move, so only two things are asserted: the rule
/// itself and the maturity constant, which is the sole symbol in
/// `end_time + block_maturity_duration_v1`. The second rule call over the next
/// consecutive window is a translation-invariance companion — the predicate is
/// a pure comparison over integer offsets, so the call cannot fail
/// independently of the first; it documents the advancing-window shape and is
/// not claimed as a distinct failure mode. Three further conjuncts comparing
/// the maturity sum against pre-computed literals were removed: with the
/// constant pinned on the line above them they reduced to comparisons between
/// two closed forms and could not fail unless the pin had already failed.
test state_queue_commit_end_time_maximum_accepted_keeps_queue_and_merge_live() {
  and {
    state_queue.commit_bound_header_time_is_valid(
      1_700_000_480_000,
      1_700_000_960_000,
      between(1_700_000_480_000, 1_700_000_960_000),
    ),
    state_queue.commit_bound_header_time_is_valid(
      1_700_000_960_000,
      1_700_001_440_000,
      between(1_700_000_960_000, 1_700_001_440_000),
    ),
    ledger_state.block_maturity_duration_v1 == 604_800_000,
  }
}

// ## State-queue element invariant: the DA core pins `reference_script`
//
// `da_availability_status_transition` is the one node-continuation core behind
// Apply (`validate_da_attestation_attachment`), Open
// (`validate_da_challenge_open_transition_v1`) and Close
// (`validate_da_availability_status_transition`). It requires
// `element_output_has_no_reference_script` of the node output, because
// `validate_completed_fraud_record` can only mark a node without one.

const da_core_policy =
  #"c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7"

const da_core_commitment_hash =
  #"c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8"

const da_core_challenge_asset_name =
  #"44414348c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9"

const da_core_terminal_commitment =
  #"cacacacacacacacacacacacacacacacacacacacacacacacacacacacacacacaca"

const da_core_probe_reference_script =
  #"f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3"

/// The node for `empty_header()` at the state-queue address, carrying `status`.
fn da_core_node(status: availability.StateQueueStatusV1) -> Output {
  let header = empty_header()
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(
      state_queue.StateQueueNode {
        header,
        da_attestation: status,
        proven_fraud: None,
      },
    ),
    link: None,
  }
  Output {
    address: address.from_script(da_core_policy),
    value: assets.from_lovelace(2_000_000)
      |> assets.add(
          da_core_policy,
          bytearray.concat(
            state_queue.block_asset_name_prefix,
            header_hash(header),
          ),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn da_core_inputs(status: availability.StateQueueStatusV1) -> List<Input> {
  [
    Input {
      output_reference: OutputReference {
        transaction_id: #"0303030303030303030303030303030303030303030303030303030303030303",
        output_index: 0,
      },
      output: da_core_node(status),
    },
  ]
}

fn with_probe_reference_script(output: Output) -> Output {
  Output { ..output, reference_script: Some(da_core_probe_reference_script) }
}

fn da_core_close(rewrite: fn(Output) -> Output) -> Bool {
  let challenged = availability.Challenged {
    commitment_hash: da_core_commitment_hash,
    challenge_asset_name: da_core_challenge_asset_name,
  }
  let published = availability.Published {
    terminal_commitment: da_core_terminal_commitment,
  }
  state_queue.validate_da_availability_status_transition(
    da_core_inputs(challenged),
    [rewrite(da_core_node(published))],
    da_core_policy,
    0,
    0,
    header_hash(empty_header()),
    challenged,
    published,
  )
}

fn da_core_open(rewrite: fn(Output) -> Output) -> Int {
  state_queue.validate_da_challenge_open_transition_v1(
    da_core_inputs(
      availability.Attested { commitment_hash: da_core_commitment_hash },
    ),
    [
      rewrite(
        da_core_node(
          availability.Challenged {
            commitment_hash: da_core_commitment_hash,
            challenge_asset_name: da_core_challenge_asset_name,
          },
        ),
      ),
    ],
    da_core_policy,
    0,
    0,
    header_hash(empty_header()),
    da_core_commitment_hash,
    da_core_challenge_asset_name,
  )
}

fn da_core_apply(rewrite: fn(Output) -> Output) -> Bool {
  let end_time = empty_header().end_time
  state_queue.validate_da_attestation_attachment(
    da_core_inputs(state_queue.no_da_attestation),
    [
      rewrite(
        da_core_node(
          availability.Attested { commitment_hash: da_core_commitment_hash },
        ),
      ),
    ],
    da_core_policy,
    0,
    0,
    header_hash(empty_header()),
    da_core_commitment_hash,
    between(end_time, end_time + 1_000),
  )
}

test state_queue_element_reference_script_predicate_is_exact() {
  let node = da_core_node(state_queue.no_da_attestation)
  and {
    state_queue.element_output_has_no_reference_script(node),
    !state_queue.element_output_has_no_reference_script(
      with_probe_reference_script(node),
    ),
  }
}

/// Controls for the three DA core users: each accepts the honest continuation,
/// whose node output carries no reference script.
test da_core_controls_accept_node_output_without_reference_script() {
  and {
    da_core_apply(fn(output) { output }),
    da_core_open(fn(output) { output }) == empty_header().end_time,
    da_core_close(fn(output) { output }),
  }
}

/// Control: `da_core_controls_accept_node_output_without_reference_script`.
/// Refused by `expect element_output_has_no_reference_script(output)` in
/// `da_availability_status_transition`, reached through
/// `validate_da_attestation_attachment` (Apply).
test da_core_apply_rejects_node_output_reference_script() fail {
  da_core_apply(with_probe_reference_script)
}

/// Control: `da_core_controls_accept_node_output_without_reference_script`.
/// Refused by `expect element_output_has_no_reference_script(output)` in
/// `da_availability_status_transition`, reached through
/// `validate_da_challenge_open_transition_v1` (Open).
test da_core_open_rejects_node_output_reference_script() fail {
  da_core_open(with_probe_reference_script) == empty_header().end_time
}

/// Control: `da_core_controls_accept_node_output_without_reference_script`.
/// Refused by `expect element_output_has_no_reference_script(output)` in
/// `da_availability_status_transition`, reached through
/// `validate_da_availability_status_transition` (Close).
test da_core_close_rejects_node_output_reference_script() fail {
  da_core_close(with_probe_reference_script)
}
