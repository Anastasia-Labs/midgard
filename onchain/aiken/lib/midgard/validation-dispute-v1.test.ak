use aiken/cbor
use aiken/interval
use env
use midgard/common/utils
use midgard/validation_dispute_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_d = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const h_e = #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const h_f = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn descriptor4(
  a: ByteArray,
  b: ByteArray,
  c: ByteArray,
  d: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  let left = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(a),
    validation_trace_v1.trace_leaf_hash(b),
  )
  let right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(c),
    validation_trace_v1.trace_leaf_hash(d),
  )
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(left, right),
    step_count: 3,
    initial_state_hash: a,
    terminal_state_hash: d,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero,
  }
}

fn proof4(
  index: Int,
  state: ByteArray,
  sibling: ByteArray,
  other_pair_root: ByteArray,
) -> validation_trace_v1.ValidationTraceProof {
  validation_trace_v1.ValidationTraceProof {
    state_index: index,
    state_hash: state,
    siblings: [validation_trace_v1.trace_leaf_hash(sibling), other_pair_root],
  }
}

fn descriptor2(
  initial: ByteArray,
  terminal: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(
      validation_trace_v1.trace_leaf_hash(initial),
      validation_trace_v1.trace_leaf_hash(terminal),
    ),
    step_count: 1,
    initial_state_hash: initial,
    terminal_state_hash: terminal,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero,
  }
}

fn fixture_pre_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_b,
    transaction_commitment: h_c,
    validation_context_hash: h_d,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: h_e,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: h_f,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_a,
  }
}

test dispute_turn_and_winner_tags_match_typescript() {
  and {
    cbor.serialise(validation_dispute_v1.AwaitingOperator { midpoint: 1 }) == #"d8799f01ff",
    cbor.serialise(
      validation_dispute_v1.AwaitingChallenger {
        midpoint: 1,
        operator_midpoint_hash: h_a,
      },
    ) == #"d87a9f015820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaff",
    cbor.serialise(validation_dispute_v1.ReadyForOneStep) == #"d87b80",
    cbor.serialise(validation_dispute_v1.OperatorWins) == #"d87980",
    cbor.serialise(validation_dispute_v1.ChallengerWins) == #"d87a80",
    cbor.serialise(validation_dispute_v1.NeitherClaimValid) == #"d87b80",
  }
}

test bisection_keeps_the_equal_left_boundary_and_first_different_right_boundary() {
  let operator = descriptor4(h_a, h_b, h_c, h_d)
  let challenger = descriptor4(h_a, h_b, h_e, h_f)
  let opened = validation_dispute_v1.open(
    operator,
    challenger,
    operator.initial_state_hash,
    100,
  )
  let operator_right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_c),
    validation_trace_v1.trace_leaf_hash(h_d),
  )
  let challenger_right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_e),
    validation_trace_v1.trace_leaf_hash(h_f),
  )
  let with_operator_1 = validation_dispute_v1.reveal_operator_midpoint(
    opened,
    proof4(1, h_b, h_a, operator_right),
    101,
    101,
  )
  let after_equal_1 = validation_dispute_v1.reveal_challenger_midpoint(
    with_operator_1,
    proof4(1, h_b, h_a, challenger_right),
    102,
    102,
  )
  let operator_left = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_a),
    validation_trace_v1.trace_leaf_hash(h_b),
  )
  let with_operator_2 = validation_dispute_v1.reveal_operator_midpoint(
    after_equal_1,
    proof4(2, h_c, h_d, operator_left),
    103,
    103,
  )
  let ready = validation_dispute_v1.reveal_challenger_midpoint(
    with_operator_2,
    proof4(2, h_e, h_f, operator_left),
    104,
    104,
  )
  and {
    ready.low_index == 1,
    ready.high_index == 2,
    ready.agreed_low_hash == h_b,
    ready.operator_high_hash == h_c,
    ready.challenger_high_hash == h_e,
    ready.turn == validation_dispute_v1.ReadyForOneStep,
    ready.round == 2,
  }
}

test timeout_assigns_loss_to_the_party_that_owes_the_move() {
  let clock = validation_dispute_v1.dispute_side_clock_milliseconds
  let operator = descriptor4(h_a, h_b, h_c, h_d)
  let challenger = descriptor4(h_a, h_b, h_e, h_f)
  let opened = validation_dispute_v1.open(
    operator,
    challenger,
    operator.initial_state_hash,
    100,
  )
  let operator_right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_c),
    validation_trace_v1.trace_leaf_hash(h_d),
  )
  let awaiting_challenger = validation_dispute_v1.reveal_operator_midpoint(
    opened,
    proof4(1, h_b, h_a, operator_right),
    101,
    101,
  )
  and {
    validation_dispute_v1.timeout_winner(opened, 100 + clock + 1) == validation_dispute_v1.ChallengerWins,
    validation_dispute_v1.timeout_winner(awaiting_challenger, 101 + clock + 1) == validation_dispute_v1.OperatorWins,
  }
}

const clock = validation_dispute_v1.dispute_side_clock_milliseconds

const l_d = validation_dispute_v1.dispute_max_validity_range_milliseconds

const opened_at = 1_000

fn clock_fixture() -> validation_dispute_v1.ValidationDisputeV1 {
  validation_dispute_v1.open(
    descriptor4(h_a, h_b, h_c, h_d),
    descriptor4(h_a, h_b, h_e, h_f),
    h_a,
    opened_at,
  )
}

fn operator_first_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(
    1,
    h_b,
    h_a,
    validation_trace_v1.trace_branch_hash(
      validation_trace_v1.trace_leaf_hash(h_c),
      validation_trace_v1.trace_leaf_hash(h_d),
    ),
  )
}

fn challenger_first_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(
    1,
    h_b,
    h_a,
    validation_trace_v1.trace_branch_hash(
      validation_trace_v1.trace_leaf_hash(h_e),
      validation_trace_v1.trace_leaf_hash(h_f),
    ),
  )
}

fn left_pair_root() -> ByteArray {
  validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_a),
    validation_trace_v1.trace_leaf_hash(h_b),
  )
}

fn operator_second_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(2, h_c, h_d, left_pair_root())
}

fn challenger_second_proof() -> validation_trace_v1.ValidationTraceProof {
  proof4(2, h_e, h_f, left_pair_root())
}

/// Operator reveal at `lower`/`upper`, then challenger reveal at the given
/// bounds: the first round of the fixture, agreeing at index 1.
fn first_round(
  operator_lower: Int,
  operator_upper: Int,
  challenger_lower: Int,
  challenger_upper: Int,
) -> validation_dispute_v1.ValidationDisputeV1 {
  validation_dispute_v1.reveal_challenger_midpoint(
    validation_dispute_v1.reveal_operator_midpoint(
      clock_fixture(),
      operator_first_proof(),
      operator_lower,
      operator_upper,
    ),
    challenger_first_proof(),
    challenger_lower,
    challenger_upper,
  )
}

fn ready_fixture() -> validation_dispute_v1.ValidationDisputeV1 {
  let after_first = first_round(1_100, 1_200, 1_300, 1_400)
  validation_dispute_v1.reveal_challenger_midpoint(
    validation_dispute_v1.reveal_operator_midpoint(
      after_first,
      operator_second_proof(),
      1_500,
      1_600,
    ),
    challenger_second_proof(),
    1_700,
    1_800,
  )
}

test opening_starts_both_clocks_full_at_the_source_upper_bound() {
  let opened = clock_fixture()
  and {
    opened.turn_started_at == opened_at,
    opened.operator_clock_ms == clock,
    opened.challenger_clock_ms == clock,
  }
}

test a_reveal_is_charged_the_time_since_the_turn_started_to_its_lower_bound() {
  let moved = validation_dispute_v1.reveal_operator_midpoint(
    clock_fixture(),
    operator_first_proof(),
    opened_at + 5_000,
    opened_at + 5_000 + l_d,
  )
  and {
    moved.operator_clock_ms == clock - 5_000,
    moved.challenger_clock_ms == clock,
    moved.turn_started_at == opened_at + 5_000 + l_d,
  }
}

test clock_debits_accumulate_per_side_across_moves() {
  let after_first = first_round(1_100, 1_200, 1_350, 1_400)
  let moved = validation_dispute_v1.reveal_operator_midpoint(
    after_first,
    operator_second_proof(),
    1_470,
    1_480,
  )
  and {
    moved.operator_clock_ms == clock - 100 - 70,
    moved.challenger_clock_ms == clock - 150,
    moved.turn_started_at == 1_480,
  }
}

test a_side_may_spend_its_clock_to_the_last_millisecond_over_several_moves() {
  let after_first = first_round(
    opened_at + clock - 10,
    opened_at + clock,
    opened_at + clock,
    opened_at + clock,
  )
  let moved = validation_dispute_v1.reveal_operator_midpoint(
    after_first,
    operator_second_proof(),
    opened_at + clock + 10,
    opened_at + clock + 10,
  )
  moved.operator_clock_ms == 0
}

test a_side_that_overdraws_its_clock_over_several_moves_is_refused() fail {
  let after_first = first_round(
    opened_at + clock - 10,
    opened_at + clock,
    opened_at + clock,
    opened_at + clock,
  )
  expect _moved = validation_dispute_v1.reveal_operator_midpoint(
    after_first,
    operator_second_proof(),
    opened_at + clock + 11,
    opened_at + clock + 11,
  )
}

test one_long_gap_within_the_clock_is_accepted() {
  let moved = validation_dispute_v1.reveal_operator_midpoint(
    clock_fixture(),
    operator_first_proof(),
    opened_at + clock,
    opened_at + clock,
  )
  moved.operator_clock_ms == 0
}

test one_gap_longer_than_the_clock_is_refused() fail {
  expect _moved = validation_dispute_v1.reveal_operator_midpoint(
    clock_fixture(),
    operator_first_proof(),
    opened_at + clock + 1,
    opened_at + clock + 1,
  )
}

test a_lower_bound_before_the_turn_start_is_charged_nothing() {
  let moved = validation_dispute_v1.reveal_operator_midpoint(
    clock_fixture(),
    operator_first_proof(),
    opened_at - 500,
    opened_at + 10,
  )
  and {
    moved.operator_clock_ms == clock,
    moved.turn_started_at == opened_at + 10,
  }
}

test a_movers_upper_bound_never_overcharges_either_side() {
  let narrow = validation_dispute_v1.reveal_operator_midpoint(
    clock_fixture(),
    operator_first_proof(),
    opened_at + 100,
    opened_at + 100,
  )
  let wide = validation_dispute_v1.reveal_operator_midpoint(
    clock_fixture(),
    operator_first_proof(),
    opened_at + 100,
    opened_at + 100 + l_d,
  )
  // The opponent's next move may land anywhere inside the stretched range and
  // is charged only from the stretched turn start.
  let answered = validation_dispute_v1.reveal_challenger_midpoint(
    wide,
    challenger_first_proof(),
    opened_at + 100 + l_d - 1,
    opened_at + 100 + l_d,
  )
  and {
    narrow.operator_clock_ms == wide.operator_clock_ms,
    answered.challenger_clock_ms == clock,
  }
}

test enter_resolution_charges_the_challenger_and_restarts_the_turn() {
  let ready = ready_fixture()
  let entered = validation_dispute_v1.enter_resolution(ready, 1_900, 1_950)
  and {
    ready.turn == validation_dispute_v1.ReadyForOneStep,
    entered.challenger_clock_ms == ready.challenger_clock_ms - 100,
    entered.operator_clock_ms == ready.operator_clock_ms,
    entered.turn_started_at == 1_950,
    entered.turn == validation_dispute_v1.ReadyForOneStep,
  }
}

test enter_resolution_refuses_a_game_still_bisecting() fail {
  expect _entered = validation_dispute_v1.enter_resolution(
    clock_fixture(),
    opened_at,
    opened_at,
  )
}

test enter_resolution_refuses_a_challenger_with_no_clock_left() fail {
  let ready = ready_fixture()
  expect _entered = validation_dispute_v1.enter_resolution(
    ready,
    1_800 + ready.challenger_clock_ms + 1,
    1_800 + ready.challenger_clock_ms + 1,
  )
}

test timeout_lands_one_millisecond_after_the_movers_clock_runs_out() {
  let ready = ready_fixture()
  and {
    validation_dispute_v1.timeout_winner(clock_fixture(), opened_at + clock + 1) == validation_dispute_v1.ChallengerWins,
    validation_dispute_v1.timeout_winner(
      ready,
      1_800 + ready.challenger_clock_ms + 1,
    ) == validation_dispute_v1.NeitherClaimValid,
  }
}

test timeout_is_refused_while_the_mover_still_has_clock() fail {
  expect _winner = validation_dispute_v1.timeout_winner(
    clock_fixture(),
    opened_at + clock,
  )
}

test a_dispute_move_range_of_exactly_l_d_is_accepted() {
  let (lower, upper) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
    interval.between(0, l_d),
    l_d,
  )
  and {
    lower == 0,
    upper == l_d,
  }
}

test a_dispute_move_range_longer_than_l_d_is_refused() fail {
  expect _bounds = utils.get_inclusive_bounds_of_a_bounded_validity_range(
    interval.between(0, l_d + 1),
    l_d,
  )
}

/// The case open guard is inclusive at `block_end + M/2` and closed one
/// millisecond later.
test case_open_guard_is_inclusive_at_half_maturity() {
  let block_end = 1_000_000
  let deadline = block_end + env.block_maturity_duration_v1 / 2
  and {
    validation_dispute_v1.case_open_window_milliseconds == env.block_maturity_duration_v1 / 2,
    validation_dispute_v1.case_can_open(deadline, block_end),
    !validation_dispute_v1.case_can_open(deadline + 1, block_end),
  }
}

test one_step_boundary_is_authenticated_before_resolver_handoff() {
  let base = fixture_pre_state()
  let work_witness_cbor = #"80"
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..base,
    phase: validation_trace_v1.CompactBinding,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CompactBinding,
      0,
      work_witness_cbor,
    ),
  }
  let operator_successor = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.InputSets,
    program_counter: 1,
    work_root: h_c,
  }
  let challenger_successor = validation_trace_v1.ValidationMachineStateV1 {
    ..operator_successor,
    work_root: h_d,
  }
  let pre_hash = validation_trace_v1.hash_machine_state(pre)
  let operator_post_hash = validation_trace_v1.hash_machine_state(
    operator_successor,
  )
  let challenger_post_hash = validation_trace_v1.hash_machine_state(
    challenger_successor,
  )
  let operator = descriptor2(pre_hash, operator_post_hash)
  let challenger = descriptor2(pre_hash, challenger_post_hash)
  let ready = validation_dispute_v1.open(
    operator,
    challenger,
    operator.initial_state_hash,
    100,
  )
  let resolution = validation_resolution_v1.prepare(ready, pre)
  and {
    resolution.version == validation_resolution_v1.resolution_version,
    resolution.pre_state == pre,
    resolution.operator_successor_hash == operator_post_hash,
    resolution.challenger_successor_hash == challenger_post_hash,
    validation_resolution_v1.resolver_index(pre.phase) == 1,
    validation_resolution_v1.state_is_well_formed(resolution),
  }
}

/// A ReadyForOneStep dispute on `pre` whose two high hashes are the given
/// successors (Gap A tests).
fn ready_for_one_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  operator_successor_hash: ByteArray,
  challenger_successor_hash: ByteArray,
) -> validation_dispute_v1.ValidationDisputeV1 {
  let pre_hash = validation_trace_v1.hash_machine_state(pre)
  validation_dispute_v1.open(
    descriptor2(pre_hash, operator_successor_hash),
    descriptor2(pre_hash, challenger_successor_hash),
    pre_hash,
    100,
  )
}

fn compact_binding_pre_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..fixture_pre_state(),
    phase: validation_trace_v1.CompactBinding,
  }
}

test gap_a_successor_hashes_come_from_the_dispute_datum() {
  let resolution = validation_resolution_v1.prepare(
    ready_for_one_step(compact_binding_pre_state(), h_c, h_d),
    compact_binding_pre_state(),
  )
  and {
    resolution.operator_successor_hash == h_c,
    resolution.challenger_successor_hash == h_d,
  }
}

test gap_a_one_pre_state_two_dispute_hash_pairs_each_prepare_their_own() {
  let pre = compact_binding_pre_state()
  let first = validation_resolution_v1.prepare(
    ready_for_one_step(pre, h_c, h_d),
    pre,
  )
  let second = validation_resolution_v1.prepare(
    ready_for_one_step(pre, h_e, h_f),
    pre,
  )
  and {
    first.pre_state == second.pre_state,
    first.operator_successor_hash == h_c,
    second.operator_successor_hash == h_e,
    first != second,
  }
}

test gap_a_refuses_a_pre_state_that_is_not_the_agreed_low_state() fail {
  let pre = compact_binding_pre_state()
  let other = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    work_root: h_e,
  }
  validation_resolution_v1.prepare(ready_for_one_step(pre, h_c, h_d), other) == validation_resolution_v1.prepare(
    ready_for_one_step(pre, h_c, h_d),
    pre,
  )
}

test gap_a_refuses_a_dispute_still_bisecting() fail {
  let pre = compact_binding_pre_state()
  let pre_hash = validation_trace_v1.hash_machine_state(pre)
  let bisecting = validation_dispute_v1.open(
    descriptor4(pre_hash, h_b, h_c, h_d),
    descriptor4(pre_hash, h_b, h_c, h_e),
    pre_hash,
    100,
  )
  validation_resolution_v1.prepare(bisecting, pre).version == validation_resolution_v1.resolution_version
}

test gap_a_refuses_a_terminal_pre_state() fail {
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..fixture_pre_state(),
    phase: validation_trace_v1.Terminal,
    verdict: validation_trace_v1.Accepted,
  }
  validation_resolution_v1.prepare(ready_for_one_step(pre, h_c, h_d), pre).version == validation_resolution_v1.resolution_version
}

test descriptors_with_different_initial_hashes_can_dispute() {
  validation_dispute_v1.descriptors_can_dispute(
    descriptor2(h_a, h_c),
    descriptor2(h_b, h_d),
  )
}

test descriptors_agreeing_on_the_terminal_claim_cannot_dispute_whatever_their_initial_hashes() {
  !validation_dispute_v1.descriptors_can_dispute(
    descriptor2(h_a, h_c),
    validation_trace_v1.ValidationTraceDescriptorV1 {
      ..descriptor2(h_b, h_c),
      trace_root: descriptor2(h_a, h_c).trace_root,
    },
  )
}

test resolver_routing_is_total_for_nonterminal_phases_and_rejects_terminal() {
  and {
    validation_resolution_v1.resolver_index(validation_trace_v1.CanonicalDecode) == 0,
    validation_resolution_v1.resolver_index(validation_trace_v1.CompactBinding) == 1,
    validation_resolution_v1.resolver_index(
      validation_trace_v1.StaticLedgerRules,
    ) == 2,
    validation_resolution_v1.resolver_index(validation_trace_v1.InputSets) == 3,
    validation_resolution_v1.resolver_index(validation_trace_v1.Signatures) == 4,
    validation_resolution_v1.resolver_index(
      validation_trace_v1.PhaseANativeScripts,
    ) == 5,
    validation_resolution_v1.resolver_index(
      validation_trace_v1.PhaseAScriptPreconditions,
    ) == 6,
    validation_resolution_v1.resolver_index(validation_trace_v1.ResolveInputs) == 7,
    validation_resolution_v1.resolver_index(validation_trace_v1.ScriptSources) == 8,
    validation_resolution_v1.resolver_index(validation_trace_v1.NativeScripts) == 9,
    validation_resolution_v1.resolver_index(validation_trace_v1.ScriptIntegrity) == 10,
    validation_resolution_v1.resolver_index(validation_trace_v1.Cek) == 11,
    validation_resolution_v1.resolver_index(validation_trace_v1.ValueAndMint) == 12,
    validation_resolution_v1.resolver_index(validation_trace_v1.LedgerDelta) == 13,
    validation_resolution_v1.resolver_index(validation_trace_v1.Terminal) == -1,
    validation_resolution_v1.resolver_count == 14,
  }
}

test malformed_resolution_states_fail_closed() {
  let pre = fixture_pre_state()
  let valid = validation_resolution_v1.ValidationResolutionStateV1 {
    version: validation_resolution_v1.resolution_version,
    pre_state: pre,
    operator_successor_hash: h_b,
    challenger_successor_hash: h_c,
  }
  and {
    validation_resolution_v1.state_is_well_formed(valid),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        version: 2,
      },
    ),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        operator_successor_hash: h_c,
      },
    ),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        challenger_successor_hash: #"00",
      },
    ),
    !validation_resolution_v1.state_is_well_formed(
      validation_resolution_v1.ValidationResolutionStateV1 {
        ..valid,
        pre_state: validation_trace_v1.ValidationMachineStateV1 {
          ..pre,
          phase: validation_trace_v1.Terminal,
        },
      },
    ),
  }
}

fn terminal_padding_fixture() -> (
  validation_dispute_v1.ValidationDisputeV1,
  validation_trace_v1.ValidationMachineStateV1,
) {
  let base = fixture_pre_state()
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..base,
    phase: validation_trace_v1.Terminal,
    verdict: validation_trace_v1.Accepted,
    program_counter: 2,
  }
  let hash = validation_trace_v1.hash_machine_state(terminal)
  let operator = descriptor4(h_a, h_b, hash, h_d)
  let challenger = descriptor4(h_a, h_b, hash, h_f)
  let opened = validation_dispute_v1.open(
    operator,
    challenger,
    operator.initial_state_hash,
    100,
  )
  let right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(hash),
    validation_trace_v1.trace_leaf_hash(h_d),
  )
  let operator_first = validation_dispute_v1.reveal_operator_midpoint(
    opened,
    proof4(1, h_b, h_a, right),
    101,
    101,
  )
  let challenger_right = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(hash),
    validation_trace_v1.trace_leaf_hash(h_f),
  )
  let agreed_first = validation_dispute_v1.reveal_challenger_midpoint(
    operator_first,
    proof4(1, h_b, h_a, challenger_right),
    102,
    102,
  )
  let left = validation_trace_v1.trace_branch_hash(
    validation_trace_v1.trace_leaf_hash(h_a),
    validation_trace_v1.trace_leaf_hash(h_b),
  )
  let operator_second = validation_dispute_v1.reveal_operator_midpoint(
    agreed_first,
    proof4(2, hash, h_d, left),
    103,
    103,
  )
  let ready = validation_dispute_v1.reveal_challenger_midpoint(
    operator_second,
    proof4(2, hash, h_f, left),
    104,
    104,
  )
  (ready, terminal)
}

test authenticated_terminal_before_the_operator_end_awards_padding() {
  let (ready, terminal) = terminal_padding_fixture()
  and {
    ready.low_index == 2,
    ready.high_index == 3,
    validation_resolution_v1.terminal_padding_is_authenticated(ready, terminal),
  }
}

test padding_refuses_a_nonterminal_authenticated_boundary() {
  let (ready, terminal) = terminal_padding_fixture()
  let nonterminal = validation_trace_v1.ValidationMachineStateV1 {
    ..terminal,
    phase: validation_trace_v1.LedgerDelta,
  }
  let honest = validation_dispute_v1.ValidationDisputeV1 {
    ..ready,
    agreed_low_hash: validation_trace_v1.hash_machine_state(nonterminal),
  }
  !validation_resolution_v1.terminal_padding_is_authenticated(
    honest,
    nonterminal,
  )
}

test padding_refuses_a_terminal_forged_under_an_honest_low_hash() {
  let (ready, terminal) = terminal_padding_fixture()
  !validation_resolution_v1.terminal_padding_is_authenticated(
    ready,
    validation_trace_v1.ValidationMachineStateV1 { ..terminal, work_root: h_c },
  )
}

test padding_refuses_the_actual_final_state() {
  let (ready, terminal) = terminal_padding_fixture()
  !validation_resolution_v1.terminal_padding_is_authenticated(
    validation_dispute_v1.ValidationDisputeV1 {
      ..ready,
      low_index: 3,
      high_index: 4,
    },
    terminal,
  )
}

test padding_refuses_a_not_ready_wrong_version_or_nonadjacent_dispute() {
  let (ready, terminal) = terminal_padding_fixture()
  and {
    !validation_resolution_v1.terminal_padding_is_authenticated(
      validation_dispute_v1.ValidationDisputeV1 {
        ..ready,
        turn: validation_dispute_v1.AwaitingOperator { midpoint: 2 },
      },
      terminal,
    ),
    !validation_resolution_v1.terminal_padding_is_authenticated(
      validation_dispute_v1.ValidationDisputeV1 { ..ready, version: 2 },
      terminal,
    ),
    !validation_resolution_v1.terminal_padding_is_authenticated(
      validation_dispute_v1.ValidationDisputeV1 { ..ready, high_index: 4 },
      terminal,
    ),
  }
}
