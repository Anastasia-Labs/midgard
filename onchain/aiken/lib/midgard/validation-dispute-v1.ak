use env
use midgard/validation_trace_v1

pub const dispute_version: Int = 1

/// C: each side's cumulative move budget (the chess clock).
pub const dispute_side_clock_milliseconds: Int =
  env.dispute_side_clock_milliseconds

/// L_d: the longest validity range any dispute move may carry.
pub const dispute_max_validity_range_milliseconds: Int =
  env.dispute_max_validity_range_milliseconds

/// R: the slack for everything after the game leaves ReadyForOneStep.
pub const dispute_resolution_allowance_milliseconds: Int =
  env.dispute_resolution_allowance_milliseconds

pub const max_bisection_rounds: Int = env.max_bisection_rounds

/// The game charges at most `2 * max_bisection_rounds + 2` moves (every reveal,
/// the ReadyForOneStep exit and the late landing of the last move), and each can
/// be undercharged by at most two validity ranges. Both clocks, that slack and
/// the resolution allowance bound a game from its opening `upper` to its last
/// move.
pub const max_dispute_duration_milliseconds: Int =
  2 * dispute_side_clock_milliseconds + 2 * ( 2 * max_bisection_rounds + 2 ) * dispute_max_validity_range_milliseconds + dispute_resolution_allowance_milliseconds

/// The fee reserve a thread carries so that it can pay the fee of every move
/// the game can charge, each at the per-move cap. It is the defender's, posted
/// per thread with the defense; VerifySource adds none.
pub const thread_fee_reserve_lovelace: Int =
  ( 2 * max_bisection_rounds + 2 ) * env.max_dispute_move_fee_lovelace

/// M/2: how long after the challenged block's commit time a case may still
/// open.
pub const case_open_window_milliseconds: Int =
  env.block_maturity_duration_v1 / 2

/// The case open guard (M1): VerifySource's validity upper bound is at most
/// `block_end + M/2`, where `block_end` is the challenged block's state-queue
/// `block_time`. The post-game allowance P = M - M/2 - D_case then covers the
/// closes and the removal before the block can merge.
pub fn case_can_open(current_time_upper: Int, block_end: Int) -> Bool {
  current_time_upper <= block_end + case_open_window_milliseconds
}

pub type DisputeTurn {
  AwaitingOperator { midpoint: Int }
  AwaitingChallenger { midpoint: Int, operator_midpoint_hash: ByteArray }
  ReadyForOneStep
}

pub type ValidationDisputeV1 {
  version: Int,
  operator_descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  challenger_descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  low_index: Int,
  high_index: Int,
  agreed_low_hash: ByteArray,
  operator_high_hash: ByteArray,
  challenger_high_hash: ByteArray,
  round: Int,
  turn_started_at: Int,
  operator_clock_ms: Int,
  challenger_clock_ms: Int,
  turn: DisputeTurn,
}

pub type DisputeWinner {
  OperatorWins
  ChallengerWins
  NeitherClaimValid
}

fn midpoint(low: Int, high: Int) -> Int {
  low + ( high - low ) / 2
}

/// Both descriptors start from the one initial state VerifySource fixes as
/// `agreed_low_hash`, so neither descriptor's own initial hash is compared.
/// Well-formedness pins both machine versions to the one supported version.
pub fn descriptors_can_dispute(
  operator: validation_trace_v1.ValidationTraceDescriptorV1,
  challenger: validation_trace_v1.ValidationTraceDescriptorV1,
) -> Bool {
  and {
    validation_trace_v1.descriptor_is_well_formed(operator),
    validation_trace_v1.descriptor_is_well_formed(challenger),
    operator.step_count == challenger.step_count,
    or {
      operator.terminal_state_hash != challenger.terminal_state_hash,
      operator.verdict != challenger.verdict,
      operator.rejection_code_hash != challenger.rejection_code_hash,
    },
  }
}

fn next_turn(low: Int, high: Int) -> DisputeTurn {
  if high == low + 1 {
    ReadyForOneStep
  } else {
    AwaitingOperator { midpoint: midpoint(low, high) }
  }
}

pub fn open(
  operator: validation_trace_v1.ValidationTraceDescriptorV1,
  challenger: validation_trace_v1.ValidationTraceDescriptorV1,
  agreed_low_hash: ByteArray,
  source_time_upper: Int,
) -> ValidationDisputeV1 {
  expect source_time_upper >= 0
  expect descriptors_can_dispute(operator, challenger)
  ValidationDisputeV1 {
    version: dispute_version,
    operator_descriptor: operator,
    challenger_descriptor: challenger,
    low_index: 0,
    high_index: operator.step_count,
    agreed_low_hash,
    operator_high_hash: operator.terminal_state_hash,
    challenger_high_hash: challenger.terminal_state_hash,
    round: 0,
    turn_started_at: source_time_upper,
    operator_clock_ms: dispute_side_clock_milliseconds,
    challenger_clock_ms: dispute_side_clock_milliseconds,
    turn: next_turn(0, operator.step_count),
  }
}

/// The mover's clock after this move. A move is charged the time since the
/// turn started, read from its validity lower bound; a lower bound before the
/// turn start is charged nothing, so no move ever credits a clock. The move
/// must fit in what is left.
fn charge(clock: Int, turn_started_at: Int, current_time_lower: Int) -> Int {
  let elapsed = if current_time_lower > turn_started_at {
    current_time_lower - turn_started_at
  } else {
    0
  }
  expect elapsed <= clock
  clock - elapsed
}

pub fn reveal_operator_midpoint(
  dispute: ValidationDisputeV1,
  proof: validation_trace_v1.ValidationTraceProof,
  current_time_lower: Int,
  current_time_upper: Int,
) -> ValidationDisputeV1 {
  expect dispute.version == dispute_version
  expect AwaitingOperator { midpoint } = dispute.turn
  expect proof.state_index == midpoint
  expect
    validation_trace_v1.verify_trace_proof(dispute.operator_descriptor, proof)
  ValidationDisputeV1 {
    ..dispute,
    turn_started_at: current_time_upper,
    operator_clock_ms: charge(
      dispute.operator_clock_ms,
      dispute.turn_started_at,
      current_time_lower,
    ),
    turn: AwaitingChallenger {
      midpoint,
      operator_midpoint_hash: proof.state_hash,
    },
  }
}

pub fn reveal_challenger_midpoint(
  dispute: ValidationDisputeV1,
  proof: validation_trace_v1.ValidationTraceProof,
  current_time_lower: Int,
  current_time_upper: Int,
) -> ValidationDisputeV1 {
  expect dispute.version == dispute_version
  expect AwaitingChallenger { midpoint, operator_midpoint_hash } = dispute.turn
  expect proof.state_index == midpoint
  expect
    validation_trace_v1.verify_trace_proof(dispute.challenger_descriptor, proof)
  let midpoint_agrees = operator_midpoint_hash == proof.state_hash
  let next_low = if midpoint_agrees {
    midpoint
  } else {
    dispute.low_index
  }
  let next_high = if midpoint_agrees {
    dispute.high_index
  } else {
    midpoint
  }
  expect next_high > next_low
  let next_round = dispute.round + 1
  expect next_round <= max_bisection_rounds
  ValidationDisputeV1 {
    ..dispute,
    low_index: next_low,
    high_index: next_high,
    agreed_low_hash: if midpoint_agrees {
      proof.state_hash
    } else {
      dispute.agreed_low_hash
    },
    operator_high_hash: if midpoint_agrees {
      dispute.operator_high_hash
    } else {
      operator_midpoint_hash
    },
    challenger_high_hash: if midpoint_agrees {
      dispute.challenger_high_hash
    } else {
      proof.state_hash
    },
    round: next_round,
    turn_started_at: current_time_upper,
    challenger_clock_ms: charge(
      dispute.challenger_clock_ms,
      dispute.turn_started_at,
      current_time_lower,
    ),
    turn: next_turn(next_low, next_high),
  }
}

/// The only exit from ReadyForOneStep. It is the challenger's move, so it is
/// charged to the challenger's clock; everything after it runs on the
/// resolution allowance.
pub fn enter_resolution(
  dispute: ValidationDisputeV1,
  current_time_lower: Int,
  current_time_upper: Int,
) -> ValidationDisputeV1 {
  expect dispute.version == dispute_version
  expect ReadyForOneStep = dispute.turn
  ValidationDisputeV1 {
    ..dispute,
    turn_started_at: current_time_upper,
    challenger_clock_ms: charge(
      dispute.challenger_clock_ms,
      dispute.turn_started_at,
      current_time_lower,
    ),
  }
}

/// The side to move loses once its whole remaining clock has passed since
/// the turn started, read from the validity lower bound so that a timeout can
/// never land before the real deadline.
pub fn timeout_winner(
  dispute: ValidationDisputeV1,
  current_time_lower: Int,
) -> DisputeWinner {
  expect dispute.version == dispute_version
  when dispute.turn is {
    AwaitingOperator { .. } -> {
      expect
        current_time_lower > dispute.turn_started_at + dispute.operator_clock_ms
      ChallengerWins
    }
    AwaitingChallenger { .. } -> {
      expect
        current_time_lower > dispute.turn_started_at + dispute.challenger_clock_ms
      OperatorWins
    }
    ReadyForOneStep -> {
      expect
        current_time_lower > dispute.turn_started_at + dispute.challenger_clock_ms
      NeitherClaimValid
    }
  }
}
