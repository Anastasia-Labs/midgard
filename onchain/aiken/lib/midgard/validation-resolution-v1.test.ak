use midgard/validation_case_v1.{CaseThreadV1, ThreadHeaderV1, thread_version}
use midgard/validation_dispute_v1.{
  ChallengerWins, DefenderWins, NeitherClaimValid, ReadyForOneStep,
}
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_d = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const h_e = #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const h_f = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

fn running_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_b,
    transaction_commitment: h_c,
    validation_context_hash: h_d,
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: h_e,
    phase: validation_trace_v1.LedgerDelta,
    program_counter: 1,
    work_root: h_f,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_a,
  }
}

fn terminal_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..running_state(),
    phase: validation_trace_v1.Terminal,
    program_counter: 2,
    verdict: validation_trace_v1.Accepted,
  }
}

fn hash(state: validation_trace_v1.ValidationMachineStateV1) -> ByteArray {
  validation_trace_v1.hash_machine_state(state)
}

/// A one-step thread at ReadyForOneStep, whose agreed state is `agreed` and
/// whose two high hashes are the given successors.
fn ready(
  agreed: validation_trace_v1.ValidationMachineStateV1,
  defender_successor: ByteArray,
  challenger_successor: ByteArray,
) -> CaseThreadV1 {
  CaseThreadV1 {
    header: ThreadHeaderV1 {
      started_at: 0,
      defender_vkh: #"01010101010101010101010101010101010101010101010101010101",
    },
    version: thread_version,
    case_id: h_a,
    challenger_root: h_b,
    defender_root: h_c,
    step_count: 1,
    low_index: 0,
    high_index: 1,
    agreed_low_hash: hash(agreed),
    defender_high_hash: defender_successor,
    challenger_high_hash: challenger_successor,
    round: 1,
    turn_started_at: 100,
    defender_clock_ms: 1000,
    challenger_clock_ms: 1000,
    turn: ReadyForOneStep,
  }
}

fn winner(
  dispute: CaseThreadV1,
  terminal: validation_trace_v1.ValidationMachineStateV1,
) -> validation_dispute_v1.DisputeWinner {
  validation_resolution_v1.terminal_fixed_point_winner(dispute, terminal)
}

/// The challenger padded: its successor repeats the agreed Terminal state.
fn challenger_padding() -> CaseThreadV1 {
  ready(terminal_state(), h_c, hash(terminal_state()))
}

// Padding on each side.

test a_challenger_that_repeats_the_agreed_terminal_state_wins() {
  winner(challenger_padding(), terminal_state()) == ChallengerWins
}

test a_defender_that_repeats_the_agreed_terminal_state_wins() {
  winner(ready(terminal_state(), hash(terminal_state()), h_c), terminal_state()) == DefenderWins
}

test a_terminal_agreed_state_that_neither_side_repeats_is_neither_claim() {
  winner(ready(terminal_state(), h_c, h_d), terminal_state()) == NeitherClaimValid
}

test both_sides_repeating_the_terminal_state_is_the_defenders_win() {
  let both = CaseThreadV1 {
    ..challenger_padding(),
    defender_high_hash: hash(terminal_state()),
  }
  winner(both, terminal_state()) == DefenderWins
}

test the_fixed_point_reads_only_the_two_high_hashes_not_the_roots_or_depth() {
  let padded = challenger_padding()
  let reshaped = CaseThreadV1 {
    ..padded,
    challenger_root: padded.defender_root,
    defender_root: padded.challenger_root,
    step_count: 3,
    low_index: 7,
    high_index: 8,
  }
  winner(reshaped, terminal_state()) == ChallengerWins
}

// An early fake terminal on each side: the agreed state is still running, so
// the fixed point does not apply and the phase resolver decides the step.

test a_challengers_early_terminal_goes_to_the_phase_resolver() {
  let dispute = ready(running_state(), h_c, hash(terminal_state()))
  and {
    validation_resolution_v1.one_step_boundary_is_authenticated(
      dispute,
      running_state(),
    ),
    validation_resolution_v1.resolver_index(running_state().phase) == 13,
  }
}

test a_challengers_early_terminal_is_refused_by_the_fixed_point() fail {
  let dispute = ready(running_state(), h_c, hash(terminal_state()))
  winner(dispute, running_state()) == ChallengerWins
}

test a_defenders_early_terminal_goes_to_the_phase_resolver() {
  let dispute = ready(running_state(), hash(terminal_state()), h_c)
  validation_resolution_v1.one_step_boundary_is_authenticated(
    dispute,
    running_state(),
  )
}

test a_defenders_early_terminal_is_refused_by_the_fixed_point() fail {
  let dispute = ready(running_state(), hash(terminal_state()), h_c)
  winner(dispute, running_state()) == DefenderWins
}

// Each guard, against the challenger-padding baseline, refuses alone.

test the_fixed_point_refuses_a_terminal_state_that_is_not_the_agreed_one() fail {
  winner(
    challenger_padding(),
    validation_trace_v1.ValidationMachineStateV1 {
      ..terminal_state(),
      work_root: h_c,
    },
  ) == ChallengerWins
}

test the_fixed_point_refuses_a_running_state_under_its_own_agreed_hash() fail {
  let agreed = running_state()
  winner(
    CaseThreadV1 {
      ..challenger_padding(),
      agreed_low_hash: hash(agreed),
      challenger_high_hash: hash(agreed),
    },
    agreed,
  ) == ChallengerWins
}

test the_fixed_point_refuses_a_malformed_terminal_state() fail {
  let malformed = validation_trace_v1.ValidationMachineStateV1 {
    ..terminal_state(),
    ledger_delta_root: #"00",
  }
  winner(
    CaseThreadV1 {
      ..challenger_padding(),
      agreed_low_hash: h_e,
      challenger_high_hash: h_e,
    },
    malformed,
  ) == ChallengerWins
}

test the_fixed_point_refuses_a_dispute_still_bisecting() fail {
  winner(
    CaseThreadV1 {
      ..challenger_padding(),
      turn: validation_dispute_v1.AwaitingOperator { midpoint: 0 },
    },
    terminal_state(),
  ) == ChallengerWins
}

test the_fixed_point_refuses_another_thread_version() fail {
  winner(
    CaseThreadV1 { ..challenger_padding(), version: thread_version + 1 },
    terminal_state(),
  ) == ChallengerWins
}

test the_fixed_point_refuses_a_non_adjacent_boundary() fail {
  winner(
    CaseThreadV1 { ..challenger_padding(), high_index: 2 },
    terminal_state(),
  ) == ChallengerWins
}
