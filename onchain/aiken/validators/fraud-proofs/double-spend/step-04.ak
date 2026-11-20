use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/double_spend/input_witness
use midgard/fraud_proofs/double_spend/step_04.{
  Args, Datum, SpendRedeemer, State,
}

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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        tx2_spend_inputs_ref_input_index,
        double_spent_input_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } =
          tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
        <-
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
          )

        expect Some(State { verified_tx2_spend_inputs_hash, double_spent_input }) =
          m_input_state_data

        // 2. Referenced native spend-inputs preimage for tx2 must be valid.
        let tx2_spend_inputs =
          input_witness.verified_spend_inputs_at(
            reference_inputs,
            tx2_spend_inputs_ref_input_index,
            verified_tx2_spend_inputs_hash,
          )

        // 3. The provided index must correspond to an input in the given list
        //    such that it matches the input carried over from previous step.
        let tx2_double_spent_input =
          tx2_spend_inputs |> list.expect_at(double_spent_input_index)
        expect double_spent_input == tx2_double_spent_input

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
