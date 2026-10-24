use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/rule.{
  authenticate_mint_fold_v1, skip_fold_v1,
}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/step_05.{
  Authenticate, Datum, Skip, SpendRedeemer,
}

validator main(
  step_06_validator_script_hash: ScriptHash,
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
        let input_index = when action is {
          Skip { input_index, .. } -> input_index
          Authenticate { input_index, .. } -> input_index
        }
        let output_index = when action is {
          Skip { output_index, .. } -> output_index
          Authenticate { output_index, .. } -> output_index
        }
        let _, _, _, input_state, next_script_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        let advanced = when action is {
          Skip { .. } -> skip_fold_v1(state, 2)
          Authenticate { evidence, .. } ->
            authenticate_mint_fold_v1(
              state,
              evidence.mint_index,
              evidence.policy_id,
              evidence.asset_name,
              evidence.quantity,
              evidence.siblings,
              evidence.mutation,
            )
        }
        let expected: Data = advanced
        and {
          next_script_hash == step_06_validator_script_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
