use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/double_spend/step_03.{
  Args, Datum, SpendRedeemer, State,
}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    input: OutputReference,
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
        index_output,
        output_index,
        computation_thread_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, mint, redeemers, .. } = tx
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
        <-
          finalize(
            computation_thread_token_policy_id,
            fraud_proof_token_policy_id,
            datum,
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
            mint,
          )

        expect Some(input_state) = m_input_state_data

        let State {
          verified_tx2_inputs_root,
          double_spent_input_key,
          double_spent_input_value,
        } = input_state

        // 1. Provided input must be a member of the second transaction's
        //    inputs.
        expect
          plutarch_phas(
            merkle_root: verified_tx2_inputs_root,
            key: double_spent_input_key,
            value: double_spent_input_value,
            membership_proof: input_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
          )

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
