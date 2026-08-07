use aiken/interval.{Finite, Interval, IntervalBound, between}
use env
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
// All four boundary cases share one absolute commit window so the accepted
// value and its neighbours are directly comparable:
//
//   inclusive lower bound  1_700_000_000_000
//   inclusive upper bound  1_700_000_480_000  (lower + 480_000)
//
// 1_700_000_480_000 is therefore both the only accepted `end_time` for this
// window and the largest `end_time` any commit opening at 1_700_000_000_000
// can carry, because a longer interval is not a short validity range at all.
// ---------------------------------------------------------------------------

/// The absolute fixtures in this family are the adjacent bounds of the accepted
/// `end_time` only while the maximum short-range length is 480_000 ms. Pin it,
/// so widening the range fails here instead of silently redefining what
/// "the maximum accepted end_time" means.
test state_queue_commit_end_time_maximum_range_length_is_pinned() {
  env.max_validity_range_length == 480_000
}

/// Boundary 1 of 4 — lower-bound control. The window's *lower* bound does not
/// gate acceptance: moving it 479_000 ms later, so the interval is 1_000 ms
/// long instead of the maximal 480_000, leaves the same header times accepted.
/// Only the upper bound decides, which is what makes this an anchor rather
/// than mere membership in the interval.
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

/// Boundary 2 of 4 — the maximum accepted `end_time`: the inclusive upper bound
/// of the longest short validity range opening at 1_700_000_000_000.
test state_queue_commit_end_time_maximum_accepted_is_the_inclusive_upper_bound() {
  state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    1_700_000_480_000,
    between(1_700_000_000_000, 1_700_000_480_000),
  )
}

/// Boundary 3 of 4 — one millisecond above the bound is already rejected.
test state_queue_commit_end_time_rejects_one_millisecond_above_the_bound() {
  !state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    1_700_000_480_001,
    between(1_700_000_000_000, 1_700_000_480_000),
  )
}

/// Boundary 4 of 4 — the D-S12 far-future `end_time`, ten years (315_360_000_000
/// ms) past the window that commits it.
test state_queue_commit_end_time_rejects_a_far_future_end() {
  !state_queue.commit_bound_header_time_is_valid(
    1_700_000_000_000,
    2_015_360_480_000,
    between(1_700_000_000_000, 1_700_000_480_000),
  )
}

/// Nothing past the bound is accepted at any distance. Acceptance is equality
/// with the inclusive upper bound, so the accepted set and the at-the-bound set
/// are the same set and no accepted case can carry a later `end_time`.
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
// Those predicates are module-private, so they are mirrored here rather than
// called. The mirrors are exact transcriptions; the Q60 evidence artifact
// records that a change to the originals invalidates this family.
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

/// Mirror of `proof.ak:1304 forced_tx_is_due`.
fn forced_tx_is_due_mirror(
  header_start_time: Int,
  header_end_time: Int,
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> Bool {
  if validity_interval_start == -1 {
    validity_interval_end == -1 || header_start_time <= validity_interval_end
  } else if validity_interval_end == -1 {
    validity_interval_start <= header_end_time
  } else {
    and {
      validity_interval_start <= validity_interval_end,
      validity_interval_start <= header_end_time,
      header_start_time <= validity_interval_end,
    }
  }
}

/// Due-event class 1 of 3 — deposit (`proof.ak:1450`). A deposit that becomes
/// includable one millisecond after the maximum accepted `end_time` is not due
/// for this block; it would be due under a far-future `end_time`; and that
/// far-future `end_time` is exactly what the commit anchor refuses.
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
    state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      1_700_000_480_000,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      2_015_360_480_000,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
  }
}

/// Due-event class 2 of 3 — withdrawal (`proof.ak:1474`), taken a full year
/// past the commit window to show the gap the anchor closes is unbounded.
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
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      2_015_360_480_000,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
  }
}

/// Due-event class 3 of 3 — forced transaction (`proof.ak:1500`), whose due-ness
/// is `timed_l1_event_is_due` *and* `forced_tx_is_due` over the forced
/// transaction's own validity interval. Both halves flip together under a
/// far-future `end_time`.
test state_queue_commit_end_time_bound_keeps_a_future_forced_event_not_due() {
  and {
    !timed_l1_event_is_due_mirror(
      1_700_000_000_000,
      1_700_000_480_000,
      1_700_000_480_001,
    ),
    !forced_tx_is_due_mirror(
      1_700_000_000_000,
      1_700_000_480_000,
      1_700_000_480_001,
      1_700_100_000_000,
    ),
    timed_l1_event_is_due_mirror(
      1_700_000_000_000,
      2_015_360_480_000,
      1_700_000_480_001,
    ),
    forced_tx_is_due_mirror(
      1_700_000_000_000,
      2_015_360_480_000,
      1_700_000_480_001,
      1_700_100_000_000,
    ),
    !state_queue.commit_bound_header_time_is_valid(
      1_700_000_000_000,
      2_015_360_480_000,
      between(1_700_000_000_000, 1_700_000_480_000),
    ),
  }
}

/// Queue and merge liveness control. Committing the maximum accepted `end_time`
/// does not stall anything: the successor commit starts exactly there and
/// re-anchors in the next window, and the block matures 604_800_000 ms later —
/// a finite deadline that the rejected far-future `end_time` would have pushed
/// ten years out, which is the stall half of D-S12.
test state_queue_commit_end_time_maximum_accepted_keeps_queue_and_merge_live() {
  and {
    state_queue.commit_bound_header_time_is_valid(
      1_700_000_480_000,
      1_700_000_960_000,
      between(1_700_000_480_000, 1_700_000_960_000),
    ),
    ledger_state.block_maturity_duration_v1 == 604_800_000,
    1_700_605_280_000 >= 1_700_000_480_000 + ledger_state.block_maturity_duration_v1,
    !(1_700_605_279_999 >= 1_700_000_480_000 + ledger_state.block_maturity_duration_v1),
    2_015_360_480_000 + ledger_state.block_maturity_duration_v1 > 1_700_605_280_000,
  }
}
