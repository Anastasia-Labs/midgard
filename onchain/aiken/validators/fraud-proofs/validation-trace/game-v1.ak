use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.ValidationGameStateV1>

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
  EnterChallengerTimeout { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<GameActionV1>

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
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) -> {
        let
            Transaction {
              inputs,
              outputs,
              extra_signatories,
              validity_range,
              ..
            }
          = tx
        expect Some(step_datum) = datum
        continue(
          computation_thread_token_policy_id,
          step_datum,
          when action is {
            RevealOperator { input_index, .. } -> input_index
            RevealChallenger { input_index, .. } -> input_index
            EnterResolution { input_index, .. } -> input_index
            EnterChallengerTimeout { input_index, .. } -> input_index
          },
          when action is {
            RevealOperator { output_index, .. } -> output_index
            RevealChallenger { output_index, .. } -> output_index
            EnterResolution { output_index, .. } -> output_index
            EnterChallengerTimeout { output_index, .. } -> output_index
          },
          own_out_ref,
          inputs,
          outputs,
          fn(
            input_script_hash,
            _computation_thread_token_asset_name,
            fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            let same_state: Data = state
            when action is {
              RevealOperator { proof, .. } -> {
                let (_, current_time_upper) = utils.get_inclusive_bounds_of_a_short_validity_range(
                  validity_range,
                )
                expect
                  state.operator_vkey |> utils.has_signed(extra_signatories)
                let next_dispute = validation_dispute_v1.reveal_operator_midpoint(
                  state.dispute,
                  proof,
                  current_time_upper,
                )
                let expected_state: Data = validation_game_v1.ValidationGameStateV1 {
                  ..state,
                  dispute: next_dispute,
                }
                and {
                  output_script_hash == input_script_hash,
                  output_state == expected_state,
                }
              }
              RevealChallenger { proof, .. } -> {
                let (_, current_time_upper) = utils.get_inclusive_bounds_of_a_short_validity_range(
                  validity_range,
                )
                expect fraud_prover |> utils.has_signed(extra_signatories)
                let next_dispute = validation_dispute_v1.reveal_challenger_midpoint(
                  state.dispute,
                  proof,
                  current_time_upper,
                )
                let expected_state: Data = validation_game_v1.ValidationGameStateV1 {
                  ..state,
                  dispute: next_dispute,
                }
                and {
                  output_script_hash == input_script_hash,
                  output_state == expected_state,
                }
              }
              EnterResolution { .. } -> and {
                  state.dispute.turn == validation_dispute_v1.ReadyForOneStep,
                  output_script_hash == boundary_validator_script_hash,
                  output_state == same_state,
                }
              EnterChallengerTimeout { .. } -> {
                let (current_time_lower, _) = utils.get_inclusive_bounds_of_a_short_validity_range(
                  validity_range,
                )
                and {
                  validation_dispute_v1.timeout_winner(
                    state.dispute,
                    current_time_lower,
                  ) == validation_dispute_v1.ChallengerWins,
                  output_script_hash == timeout_validator_script_hash,
                  output_state == same_state,
                }
              }
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
