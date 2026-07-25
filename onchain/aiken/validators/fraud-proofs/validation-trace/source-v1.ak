use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_resolution_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.PendingValidationClaimV1>

pub type VerifySourceActionV1 {
  VerifySource { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<VerifySourceActionV1>

/// Verifies the transaction source/commitment after the opener has
/// authenticated the block, transition roots, and trace endpoints.
validator main(
  game_validator_script_hash: ScriptHash,
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
      ct.Continue(VerifySource { input_index, output_index }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
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
            let committed_endpoints_are_valid = validation_claim_v1.committed_claim_endpoints_and_source_are_valid(
              state.challenged_header,
              state.claim,
            )
            and {
              validation_claim_v1.committed_claim_source_is_authenticated(
                state.challenged_header,
                state.claim,
              ),
              if committed_endpoints_are_valid {
                let expected_dispute = validation_dispute_v1.open(
                  state.claim.descriptor_membership.value,
                  state.challenger_descriptor,
                  state.open_time_upper,
                )
                let expected_state: Data = validation_game_v1.ValidationGameStateV1 {
                  challenged_header_hash: state.challenged_header_hash,
                  operator_vkey: state.challenged_header.operator_vkey,
                  dispute: expected_dispute,
                }
                and {
                  output_script_hash == game_validator_script_hash,
                  output_state == expected_state,
                }
              } else {
                let expected_state: Data = validation_resolution_v1.winning_resolution()
                and {
                  output_script_hash == award_validator_script_hash,
                  output_state == expected_state,
                }
              },
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
