use midgard/validation_trace_v1

pub const dispute_version: Int = 1

pub const response_window_milliseconds: Int = 300_000

pub const max_bisection_rounds: Int = 32

pub const max_dispute_duration_milliseconds: Int =
  ( 2 * max_bisection_rounds + 2 ) * response_window_milliseconds

/// A dispute may open only while the entire worst-case alternating response
/// schedule still fits inside the challenged block's V1 maturity
/// horizon. The boundary is inclusive: a dispute that uses the final
/// permitted millisecond remains resolvable, while the next millisecond fails
/// closed.
pub fn can_open_before_maturity(
  current_time_upper: Int,
  challenged_block_end_time: Int,
  maturity_duration: Int,
) -> Bool {
  and {
    current_time_upper >= 0,
    challenged_block_end_time >= 0,
    maturity_duration >= max_dispute_duration_milliseconds,
    current_time_upper + max_dispute_duration_milliseconds <= challenged_block_end_time + maturity_duration,
  }
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
  response_deadline: Int,
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

pub fn descriptors_can_dispute(
  operator: validation_trace_v1.ValidationTraceDescriptorV1,
  challenger: validation_trace_v1.ValidationTraceDescriptorV1,
) -> Bool {
  and {
    validation_trace_v1.descriptor_is_well_formed(operator),
    validation_trace_v1.descriptor_is_well_formed(challenger),
    operator.machine_version == challenger.machine_version,
    operator.step_count == challenger.step_count,
    operator.initial_state_hash == challenger.initial_state_hash,
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
  current_slot: Int,
) -> ValidationDisputeV1 {
  expect current_slot >= 0
  expect descriptors_can_dispute(operator, challenger)
  expect operator.step_count > 0
  ValidationDisputeV1 {
    version: dispute_version,
    operator_descriptor: operator,
    challenger_descriptor: challenger,
    low_index: 0,
    high_index: operator.step_count,
    agreed_low_hash: operator.initial_state_hash,
    operator_high_hash: operator.terminal_state_hash,
    challenger_high_hash: challenger.terminal_state_hash,
    round: 0,
    response_deadline: current_slot + response_window_milliseconds,
    turn: next_turn(0, operator.step_count),
  }
}

/// Starts the response clock only after the source-verification transaction
/// authenticates the committed claim. The source hop cannot travel backwards
/// relative to the opener, and it must still leave the complete worst-case
/// dispute schedule inside the challenged block's maturity horizon.
pub fn open_after_source_verification(
  operator: validation_trace_v1.ValidationTraceDescriptorV1,
  challenger: validation_trace_v1.ValidationTraceDescriptorV1,
  open_time_upper: Int,
  source_time_upper: Int,
  challenged_block_end_time: Int,
  maturity_duration: Int,
) -> ValidationDisputeV1 {
  expect open_time_upper >= 0
  expect source_time_upper >= open_time_upper
  expect
    can_open_before_maturity(
      source_time_upper,
      challenged_block_end_time,
      maturity_duration,
    )
  open(operator, challenger, source_time_upper)
}

pub fn reveal_operator_midpoint(
  dispute: ValidationDisputeV1,
  proof: validation_trace_v1.ValidationTraceProof,
  current_slot: Int,
) -> ValidationDisputeV1 {
  expect dispute.version == dispute_version
  expect current_slot >= 0
  expect current_slot <= dispute.response_deadline
  expect AwaitingOperator { midpoint } = dispute.turn
  expect proof.state_index == midpoint
  expect
    validation_trace_v1.verify_trace_proof(dispute.operator_descriptor, proof)
  ValidationDisputeV1 {
    ..dispute,
    response_deadline: current_slot + response_window_milliseconds,
    turn: AwaitingChallenger {
      midpoint,
      operator_midpoint_hash: proof.state_hash,
    },
  }
}

pub fn reveal_challenger_midpoint(
  dispute: ValidationDisputeV1,
  proof: validation_trace_v1.ValidationTraceProof,
  current_slot: Int,
) -> ValidationDisputeV1 {
  expect dispute.version == dispute_version
  expect current_slot >= 0
  expect current_slot <= dispute.response_deadline
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
    response_deadline: current_slot + response_window_milliseconds,
    turn: next_turn(next_low, next_high),
  }
}

pub fn timeout_winner(
  dispute: ValidationDisputeV1,
  current_slot: Int,
) -> DisputeWinner {
  expect dispute.version == dispute_version
  expect current_slot > dispute.response_deadline
  when dispute.turn is {
    AwaitingOperator { .. } -> ChallengerWins
    AwaitingChallenger { .. } -> OperatorWins
    ReadyForOneStep -> NeitherClaimValid
  }
}
