use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_game_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.ValidationGameStateV1>

pub type BoundaryActionV1 {
  PrepareResolution {
    input_index: Int,
    output_index: Int,
    resolver_index: Int,
    evidence: validation_resolution_v1.ValidationBoundaryEvidenceV1,
  }
  AwardTerminalPadding {
    input_index: Int,
    output_index: Int,
    terminal_state: validation_trace_v1.ValidationMachineStateV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<BoundaryActionV1>

validator main(
  one_step_resolver_script_hashes: List<ScriptHash>,
  award_validator_script_hash: ScriptHash,
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
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        continue(
          computation_thread_token_policy_id,
          step_datum,
          when action is {
            PrepareResolution { input_index, .. } -> input_index
            AwardTerminalPadding { input_index, .. } -> input_index
          },
          when action is {
            PrepareResolution { output_index, .. } -> output_index
            AwardTerminalPadding { output_index, .. } -> output_index
          },
          own_out_ref,
          inputs,
          outputs,
          fn(
            _input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            when action is {
              PrepareResolution { resolver_index, evidence, .. } -> {
                expect Some(expected_resolver_script_hash) = list.at(
                  one_step_resolver_script_hashes,
                  resolver_index,
                )
                let expected_resolution_state: Data = validation_resolution_v1.prepare(
                  state.dispute,
                  evidence.pre_state,
                  evidence.operator_post,
                  evidence.challenger_post,
                )
                and {
                  resolver_index == validation_resolution_v1.resolver_index(
                    evidence.pre_state.phase,
                  ),
                  output_script_hash == expected_resolver_script_hash,
                  output_state == expected_resolution_state,
                }
              }
              AwardTerminalPadding { terminal_state, .. } -> {
                let expected_state: Data = validation_resolution_v1.winning_resolution()
                and {
                  validation_resolution_v1.terminal_padding_is_authenticated(
                    state.dispute,
                    terminal_state,
                  ),
                  output_script_hash == award_validator_script_hash,
                  output_state == expected_state,
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
