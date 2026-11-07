use env

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

/// The most clock-charged moves a thread can make: the defense post, every
/// reveal on both sides, the ReadyForOneStep exit and the late landing of the
/// last timeout or exit.
pub const max_thread_charged_moves: Int = 2 * max_bisection_rounds + 3

/// D_case. Each charged move can be undercharged by at most two validity
/// ranges, so both clocks, that slack and the resolution allowance bound every
/// thread from its case's `started_at` to its last move.
pub const max_dispute_duration_milliseconds: Int =
  2 * dispute_side_clock_milliseconds + 2 * max_thread_charged_moves * dispute_max_validity_range_milliseconds + dispute_resolution_allowance_milliseconds

/// The fee reserve a thread carries so that it can pay the fee of every
/// charged move, each at the per-move cap. It is the defender's, posted per
/// thread with the defense; VerifySource adds none.
pub const thread_fee_reserve_lovelace: Int =
  max_thread_charged_moves * env.max_dispute_move_fee_lovelace

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

pub type DisputeWinner {
  DefenderWins
  ChallengerWins
  NeitherClaimValid
}

fn midpoint(low: Int, high: Int) -> Int {
  low + ( high - low ) / 2
}

/// The turn after the bisection narrows to `low..high`.
pub fn next_turn(low: Int, high: Int) -> DisputeTurn {
  if high == low + 1 {
    ReadyForOneStep
  } else {
    AwaitingOperator { midpoint: midpoint(low, high) }
  }
}

/// The mover's clock after this move. A move is charged the time since the
/// turn started, read from its validity lower bound; a lower bound before the
/// turn start is charged nothing, so no move ever credits a clock. The move
/// must fit in what is left.
pub fn charge(clock: Int, turn_started_at: Int, current_time_lower: Int) -> Int {
  let elapsed = if current_time_lower > turn_started_at {
    current_time_lower - turn_started_at
  } else {
    0
  }
  expect elapsed <= clock
  clock - elapsed
}
