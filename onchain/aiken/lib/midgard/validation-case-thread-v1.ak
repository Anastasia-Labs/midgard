//// Validation case threads: the moves of one thread over `CaseThreadV1`, and
//// how a thread ends.
////
//// A thread is opened by Defend (`validation_case_v1.open_thread`) at the game
//// validator, with the datum `ThreadDatum`: the challenger is its
//// `fraud_prover`. Both sides reveal their midpoints against their own trace
//// roots, so any party may carry either side's reveal; the chess clocks and the
//// turn rule are those of `validation_dispute_v1`.
////
//// A thread ends in one of two ways:
//// - it closes into its case (`closes_into_case`): its script fixes the outcome
////   and binds it to the case's `ThreadClose`, and the case spend checks the
////   case continuation, the burn, the receipt and the leftover;
//// - once its case can no longer take it (`expires`), past `started_at +
////   D_case`, its token is burned without the case and its value returns to
////   the defender.

use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/math
use cardano/address.{Script, from_verification_key}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Spend, Transaction,
}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/validation_case_v1.{CaseRedeemerV1, CaseThreadV1, ThreadClose}
use midgard/validation_dispute_v1.{
  AwaitingChallenger, AwaitingOperator, ChallengerWins, DefenderWins,
  DisputeWinner, ReadyForOneStep, charge, next_turn,
}
use midgard/validation_trace_v1.{
  ValidationTraceProof, verify_trace_proof_against_root,
}

/// The datum of a thread at the game and timeout validators.
pub type ThreadDatum =
  ct.StepDatum<CaseThreadV1>

// ## Moves

/// The defender's midpoint, proven under the defender's root. Charged to the
/// defender's clock; the challenger's turn starts at this move's upper bound.
pub fn reveal_defender_midpoint(
  thread: CaseThreadV1,
  proof: ValidationTraceProof,
  current_time_lower: Int,
  current_time_upper: Int,
) -> CaseThreadV1 {
  expect AwaitingOperator { midpoint } = thread.turn
  expect proof.state_index == midpoint
  expect
    verify_trace_proof_against_root(
      thread.defender_root,
      thread.step_count,
      proof,
    )
  CaseThreadV1 {
    ..thread,
    turn_started_at: current_time_upper,
    defender_clock_ms: charge(
      thread.defender_clock_ms,
      thread.turn_started_at,
      current_time_lower,
    ),
    turn: AwaitingChallenger {
      midpoint,
      operator_midpoint_hash: proof.state_hash,
    },
  }
}

/// The challenger's midpoint, proven under the challenger's root. The
/// bisection keeps the half on which the two sides disagree.
pub fn reveal_challenger_midpoint(
  thread: CaseThreadV1,
  proof: ValidationTraceProof,
  current_time_lower: Int,
  current_time_upper: Int,
) -> CaseThreadV1 {
  expect AwaitingChallenger { midpoint, operator_midpoint_hash } = thread.turn
  expect proof.state_index == midpoint
  expect
    verify_trace_proof_against_root(
      thread.challenger_root,
      thread.step_count,
      proof,
    )
  let midpoint_agrees = operator_midpoint_hash == proof.state_hash
  let next_low = if midpoint_agrees {
    midpoint
  } else {
    thread.low_index
  }
  let next_high = if midpoint_agrees {
    thread.high_index
  } else {
    midpoint
  }
  expect next_high > next_low
  let next_round = thread.round + 1
  expect next_round <= validation_dispute_v1.max_bisection_rounds
  CaseThreadV1 {
    ..thread,
    low_index: next_low,
    high_index: next_high,
    agreed_low_hash: if midpoint_agrees {
      proof.state_hash
    } else {
      thread.agreed_low_hash
    },
    defender_high_hash: if midpoint_agrees {
      thread.defender_high_hash
    } else {
      operator_midpoint_hash
    },
    challenger_high_hash: if midpoint_agrees {
      thread.challenger_high_hash
    } else {
      proof.state_hash
    },
    round: next_round,
    turn_started_at: current_time_upper,
    challenger_clock_ms: charge(
      thread.challenger_clock_ms,
      thread.turn_started_at,
      current_time_lower,
    ),
    turn: next_turn(next_low, next_high),
  }
}

/// The only exit from ReadyForOneStep. It is the challenger's move, so it is
/// charged to the challenger's clock; everything after it runs on the
/// resolution allowance.
pub fn enter_resolution(
  thread: CaseThreadV1,
  current_time_lower: Int,
  current_time_upper: Int,
) -> CaseThreadV1 {
  expect ReadyForOneStep = thread.turn
  CaseThreadV1 {
    ..thread,
    turn_started_at: current_time_upper,
    challenger_clock_ms: charge(
      thread.challenger_clock_ms,
      thread.turn_started_at,
      current_time_lower,
    ),
  }
}

/// The side to move loses once its whole remaining clock has passed since
/// the turn started, read from the validity lower bound. The defender's turn
/// times out as the challenger's win; AwaitingChallenger and ReadyForOneStep
/// are the challenger's turns and time out as the defender's win.
pub fn timeout_winner(
  thread: CaseThreadV1,
  current_time_lower: Int,
) -> DisputeWinner {
  when thread.turn is {
    AwaitingOperator { .. } -> {
      expect
        current_time_lower > thread.turn_started_at + thread.defender_clock_ms
      ChallengerWins
    }
    AwaitingChallenger { .. } | ReadyForOneStep -> {
      expect
        current_time_lower > thread.turn_started_at + thread.challenger_clock_ms
      DefenderWins
    }
  }
}

// ## Ends

/// The thread input at `input_index` is the one being validated, and holds
/// exactly one token under the thread token policy. Returns that token's name
/// and the input's lovelace.
fn own_thread_token(
  thread_token_policy_id: PolicyId,
  input_index: Int,
  own_out_ref: OutputReference,
  inputs: List<Input>,
) -> (ByteArray, Int) {
  expect Some(Input { output_reference, output: Output { value, .. } }) = list.at(
    inputs,
    input_index,
  )
  expect output_reference == own_out_ref
  expect [Pair(name, 1)] = dict.to_pairs(
    assets.tokens(value, thread_token_policy_id),
  )
  (name, lovelace_of(value))
}

/// The thread closes into its case with `outcome`: the input at
/// `case_input_index` sits at the case validator (whose hash is the thread
/// token policy) and is spent with `ThreadClose` naming this outcome, and the
/// thread's own token is burned. The case spend burns exactly one thread token,
/// so it is closing this thread, and it checks the case continuation, the
/// receipt and the leftover against `outcome`.
pub fn closes_into_case(
  thread_token_policy_id: PolicyId,
  input_index: Int,
  case_input_index: Int,
  own_out_ref: OutputReference,
  outcome: DisputeWinner,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, mint, redeemers, .. } = tx
  let (name, _) = own_thread_token(
    thread_token_policy_id,
    input_index,
    own_out_ref,
    inputs,
  )
  expect Some(Input { output_reference: case_out_ref, output: case_input }) = list.at(
    inputs,
    case_input_index,
  )
  expect Some(case_redeemer) = pairs.get_first(redeemers, Spend(case_out_ref))
  expect case_redeemer: CaseRedeemerV1 = case_redeemer
  expect ThreadClose { outcome: closed_as, .. } = case_redeemer
  and {
    case_input.address.payment_credential == Script(thread_token_policy_id),
    closed_as == outcome,
    assets.quantity_of(mint, thread_token_policy_id, name) == -1,
  }
}

/// A thread its case can no longer take: past `started_at + D_case` the case
/// may close with the thread still open, as a defender win. The token is
/// burned without the case: no input sits at the case validator, so no
/// `ThreadClose` can count this burn as a close with another outcome. The
/// thread's lovelace goes to the defender, tagged with this input's out-ref,
/// less at most `min(fee, cap)` when the thread is the only input.
pub fn expires(
  thread_token_policy_id: PolicyId,
  thread: CaseThreadV1,
  input_index: Int,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, mint, fee, validity_range, .. } = tx
  let (name, thread_lovelace) = own_thread_token(
    thread_token_policy_id,
    input_index,
    own_out_ref,
    inputs,
  )
  let fee_allowance = when inputs is {
    [_] -> math.min(fee, env.max_dispute_move_fee_lovelace)
    _ -> 0
  }
  let defender = from_verification_key(thread.defender_vkh)
  let tag: Data = own_out_ref
  and {
    utils.get_inclusive_lower_bound_of_interval(validity_range) > thread.started_at + validation_dispute_v1.max_dispute_duration_milliseconds,
    assets.quantity_of(mint, thread_token_policy_id, name) == -1,
    !list.any(
      inputs,
      fn(input) {
        input.output.address.payment_credential == Script(
          thread_token_policy_id,
        )
      },
    ),
    list.any(
      outputs,
      fn(output) {
        and {
          output.address == defender,
          output.datum == InlineDatum(tag),
          lovelace_of(output.value) >= thread_lovelace - fee_allowance,
        }
      },
    ),
  }
}
