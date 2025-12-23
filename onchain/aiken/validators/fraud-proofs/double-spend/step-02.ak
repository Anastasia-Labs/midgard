use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{plutarch_phas, verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/double_spend/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/double_spend/step_03

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel {
        input_index,
        computation_thread_mint_redeemer_index,
      } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        double_spent_input_key,
        double_spent_input_value,
        input_membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

        expect Some(step_datum) = datum

        let
          _own_script_hash,
          computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
        <-
          continue(
            computation_thread_token_policy_id,
            step_datum,
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
          )

        expect Some(input_state) = m_input_state_data

        // 1. Provided input must be a member of the first transaction's inputs.
        expect
          plutarch_phas(
            merkle_root: input_state.verified_tx1_inputs_root,
            key: double_spent_input_key,
            value: double_spent_input_value,
            membership_proof: input_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
          )

        // 2. The proven input, along with the unused second transaction must be
        //    carried over in the datum of the next step.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data =
          step_03.State {
            verified_tx2_inputs_root,
            double_spent_input_key,
            double_spent_input_value,
          }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
