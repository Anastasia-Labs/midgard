use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/utils.{
  get_singleton_ct_token, handle_cancel, validate_final_step_output,
}
use midgard/fraud_proof/double_spend/step_04.{
  Cancel, Continue, StepRedeemer as Step4Redeemer,
}

validator main(
  fraud_proof_token_policy: PolicyId,
  ct_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<StepDatum>,
    redeemer: Step4Redeemer,
    input: OutputReference,
    tx: Transaction,
  ) {
    expect Some(step_datum) = datum
    expect Some(own_input) =
      list.find(tx.inputs, fn(a_input) { a_input.output_reference == input })

    when redeemer is {
      Cancel { index_output } -> {
        expect
          handle_cancel(
            own_input,
            index_output,
            step_datum,
            ct_token_policy_id,
            tx,
          )
        True
      }
      // Step 4: Final step to mint fraud token and burn CT token
      Continue { index_output, computation_thread_redeemer_index } -> {
        // get CT token
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)

        expect
          validate_final_step_output(
            tx,
            index_output,
            fraud_proof_token_policy,
            ct_token_asset_name,
            step_datum,
            computation_thread_redeemer_index,
          )

        True
      }
    }
  }

  else(_) {
    fail
  }
}
