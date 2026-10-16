use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction, ValidityRange}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{Anyone, ProverOnly, continue_validation_trace}
use midgard/validation_case_thread_v1.{ThreadDatum}
use midgard/validation_dispute_v1
use midgard/validation_trace_v1

pub type Datum =
  ThreadDatum

pub type GameActionV1 {
  RevealOperator {
    input_index: Int,
    output_index: Int,
    proof: validation_trace_v1.ValidationTraceProof,
  }
  RevealChallenger {
    input_index: Int,
    output_index: Int,
    proof: validation_trace_v1.ValidationTraceProof,
  }
  EnterResolution { input_index: Int, output_index: Int }
  /// The side to move has run out of clock: the thread moves unchanged to the
  /// timeout validator, which closes it with that side's loss.
  EnterChallengerTimeout { input_index: Int, output_index: Int }
  /// The case can no longer take the thread (`validation_case_thread_v1.expires`).
  ExpireThread { input_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<GameActionV1>

/// Every game move carries a validity range no longer than L_d, so the charge
/// read from its lower bound is exact to within that range.
fn dispute_move_bounds(validity_range: ValidityRange) -> (Int, Int) {
  utils.get_inclusive_bounds_of_a_bounded_validity_range(
    validity_range,
    validation_dispute_v1.dispute_max_validity_range_milliseconds,
  )
}

/// The bisection of one case thread (`validation_case_thread_v1`). Reveals on
/// both sides are open to anyone, since each is proven under its side's root;
/// EnterResolution is the challenger's. The thread has no cancel.
validator main(
  boundary_validator_script_hash: ScriptHash,
  timeout_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(step_datum) = datum
    when redeemer is {
      ct.Cancel { .. } -> False
      ct.Continue(ExpireThread { input_index }) -> {
        expect Some(thread) = step_datum.data
        validation_case_thread_v1.expires(
          computation_thread_token_policy_id,
          thread,
          input_index,
          own_out_ref,
          tx,
        )
      }
      ct.Continue(action) -> {
        let Transaction { validity_range, .. } = tx
        continue_validation_trace(
          computation_thread_token_policy_id,
          step_datum,
          when action is {
            RevealOperator { input_index, .. } -> input_index
            RevealChallenger { input_index, .. } -> input_index
            EnterResolution { input_index, .. } -> input_index
            EnterChallengerTimeout { input_index, .. } -> input_index
            ExpireThread { .. } -> fail
          },
          when action is {
            RevealOperator { output_index, .. } -> output_index
            RevealChallenger { output_index, .. } -> output_index
            EnterResolution { output_index, .. } -> output_index
            EnterChallengerTimeout { output_index, .. } -> output_index
            ExpireThread { .. } -> fail
          },
          own_out_ref,
          tx,
          when action is {
            EnterResolution { .. } -> ProverOnly
            _ -> Anyone
          },
          fn(
            input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(thread) = input_state
            let (current_time_lower, current_time_upper) = dispute_move_bounds(
              validity_range,
            )
            when action is {
              RevealOperator { proof, .. } -> {
                let expected_state: Data = validation_case_thread_v1.reveal_defender_midpoint(
                  thread,
                  proof,
                  current_time_lower,
                  current_time_upper,
                )
                and {
                  output_script_hash == input_script_hash,
                  output_state == expected_state,
                }
              }
              RevealChallenger { proof, .. } -> {
                let expected_state: Data = validation_case_thread_v1.reveal_challenger_midpoint(
                  thread,
                  proof,
                  current_time_lower,
                  current_time_upper,
                )
                and {
                  output_script_hash == input_script_hash,
                  output_state == expected_state,
                }
              }
              EnterResolution { .. } -> {
                let expected_state: Data = validation_case_thread_v1.enter_resolution(
                  thread,
                  current_time_lower,
                  current_time_upper,
                )
                and {
                  output_script_hash == boundary_validator_script_hash,
                  output_state == expected_state,
                }
              }
              EnterChallengerTimeout { .. } -> {
                let same_state: Data = thread
                // Either winner: the timeout validator closes the thread with
                // the same winner, read from the same turn. `timeout_winner`
                // fails before the clock runs out and never names neither.
                and {
                  validation_case_thread_v1.timeout_winner(
                    thread,
                    current_time_lower,
                  ) != validation_dispute_v1.NeitherClaimValid,
                  output_script_hash == timeout_validator_script_hash,
                  output_state == same_state,
                }
              }
              ExpireThread { .. } -> False
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
