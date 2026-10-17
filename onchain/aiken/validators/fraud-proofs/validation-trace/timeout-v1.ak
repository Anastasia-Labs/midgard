use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/validation_dispute_v1
use midgard/validation_game_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.ValidationGameStateV1>

pub type ChallengerTimeoutActionV1 {
  ChallengerTimeout {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ChallengerTimeoutActionV1>

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(ChallengerTimeout {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, validity_range, .. } = tx
        let (current_time_lower, _) = utils.get_inclusive_bounds_of_a_short_validity_range(
          validity_range,
        )
        expect Some(step_datum) = datum
        finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
          fn(
            _input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
          ) {
            expect Some(state) = input_state
            validation_dispute_v1.timeout_winner(
              state.dispute,
              current_time_lower,
            ) == validation_dispute_v1.ChallengerWins
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
