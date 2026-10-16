use midgard/validation_dispute_v1.{
  ChallengerWins, DefenderWins, NeitherClaimValid,
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

fn descriptor(
  initial: ByteArray,
  last: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(
      validation_trace_v1.trace_leaf_hash(initial),
      validation_trace_v1.trace_leaf_hash(last),
    ),
    step_count: 1,
    initial_state_hash: initial,
    terminal_state_hash: last,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: zero,
  }
}

/// A one-step dispute, ReadyForOneStep from the open, whose agreed state is
/// `agreed` and whose two high hashes are the given successors.
fn ready(
  agreed: validation_trace_v1.ValidationMachineStateV1,
  defender_successor: ByteArray,
  challenger_successor: ByteArray,
) -> validation_dispute_v1.ValidationDisputeV1 {
  let agreed_hash = hash(agreed)
  validation_dispute_v1.open(
    descriptor(agreed_hash, defender_successor),
    descriptor(agreed_hash, challenger_successor),
    agreed_hash,
    100,
  )
}

fn winner(
  dispute: validation_dispute_v1.ValidationDisputeV1,
  terminal: validation_trace_v1.ValidationMachineStateV1,
) -> validation_dispute_v1.DisputeWinner {
  validation_resolution_v1.terminal_fixed_point_winner(dispute, terminal)
}

/// The challenger padded: its successor repeats the agreed Terminal state.
fn challenger_padding() -> validation_dispute_v1.ValidationDisputeV1 {
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
  let both = validation_dispute_v1.ValidationDisputeV1 {
    ..challenger_padding(),
    operator_high_hash: hash(terminal_state()),
  }
  winner(both, terminal_state()) == DefenderWins
}

test the_fixed_point_reads_only_the_two_high_hashes_not_the_descriptors() {
  let padded = challenger_padding()
  let reshaped = validation_dispute_v1.ValidationDisputeV1 {
    ..padded,
    operator_descriptor: padded.challenger_descriptor,
    challenger_descriptor: padded.operator_descriptor,
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
    validation_dispute_v1.ValidationDisputeV1 {
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
    validation_dispute_v1.ValidationDisputeV1 {
      ..challenger_padding(),
      agreed_low_hash: h_e,
      challenger_high_hash: h_e,
    },
    malformed,
  ) == ChallengerWins
}

test the_fixed_point_refuses_a_dispute_still_bisecting() fail {
  winner(
    validation_dispute_v1.ValidationDisputeV1 {
      ..challenger_padding(),
      turn: validation_dispute_v1.AwaitingOperator { midpoint: 0 },
    },
    terminal_state(),
  ) == ChallengerWins
}

test the_fixed_point_refuses_another_dispute_version() fail {
  winner(
    validation_dispute_v1.ValidationDisputeV1 {
      ..challenger_padding(),
      version: validation_dispute_v1.dispute_version + 1,
    },
    terminal_state(),
  ) == ChallengerWins
}

test the_fixed_point_refuses_a_non_adjacent_boundary() fail {
  winner(
    validation_dispute_v1.ValidationDisputeV1 {
      ..challenger_padding(),
      high_index: 2,
    },
    terminal_state(),
  ) == ChallengerWins
}
