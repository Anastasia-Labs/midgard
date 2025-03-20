use aiken/collection/list
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}
use midgard/common/utils.{get_singleton_asset_with_policy} as get_singleton_asset_with_policy
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{FraudProofDatum}
use midgard/fraud_proof/common/utils.{validate_output_to_fraud_prover}
use midgard/fraud_proof/double_spend/step_04.{
  Cancel, Continue, StepRedeemer as Step4Redeemer,
}

validator main(
  fraud_proof_token_policy: PolicyId,
  ct_token_policy_id: PolicyId,
) {
  spend(
    a_datum: Option<StepDatum>,
    redeemer: Step4Redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = a_datum
    when redeemer is {
      Cancel { index_output } -> {
        // (1.1)
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        // (1.2) Get computation thread token asset name in output
        expect Pair(ct_token_asset_name, 1) =
          get_singleton_asset_with_policy(
            own_input.output.value,
            ct_token_policy_id,
          )
        // (1.3) Burn the computation thread token, (3) Transaction must not mint or burn any other tokens
        expect
          Pair(ct_token_asset_name, -1) == get_singleton_asset_with_policy(
            self.mint,
            ct_token_policy_id,
          )

        // (2.1) get the lovelace amount from our own validator's input (ADA inside the input of the fraud proof thread)
        let lovelace = lovelace_of(own_input.output.value)
        // (2.2) verify an output is going to fraud prover address
        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)
        // (2.3) Validate output to fraud prover (output to fraud prover must match the fraud prover pubkey)
        expect
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          )

        // (2.4) Lovelace amount in output to return to the fraud prover
        expect lovelace == lovelace_of(output_to_fraud_prover.value)
        True
      }
      // last step spending validator Continue redeemer for double-spend fraud proof handles:
      // 1) State transition to Void
      // 2) Maintaining fraud_prover field
      // 3) Verifying fraud proof token minting with correct asset name (computation thread token)
      Continue { index_output, computation_thread_redeemer_index } -> {
        // 1) Verify state transition to Void while maintaining fraud_prover
        expect Some(output_to_next_step) = list.at(self.outputs, index_output)
        expect InlineDatum(output_to_next_step_datum) =
          output_to_next_step.datum
        expect parsed_output_to_next_step_datum: FraudProofDatum =
          output_to_next_step_datum

        // 2) Maintain fraud_prover field
        let final_datum =
          FraudProofDatum { fraud_prover: step_datum.fraud_prover }
        expect final_datum == parsed_output_to_next_step_datum

        // 3) Verify minting of fraud proof token
        // Get computation thread token from inputs
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        // (3.1) Verify the computation thread token by asset name in the inputs of the transaction
        expect Pair(ct_token_asset_name, 1) =
          get_singleton_asset_with_policy(
            own_input.output.value,
            ct_token_policy_id,
          )
        // (3.2) Mint fraud proof token
        expect
          Pair(ct_token_asset_name, 1) == get_singleton_asset_with_policy(
            self.mint,
            fraud_proof_token_policy,
          )
        // (3.3) Verify computation thread token burn is called with Cancel redeemer implicitly as fraud token is minted
        expect Some(computation_thread_redeemer_pair) =
          list.at(self.redeemers, computation_thread_redeemer_index)
        expect computation_thread.Success {
          token_to_burn_asset_name: burn_token_asset_name,
        } = computation_thread_redeemer_pair.2nd
        expect burn_token_asset_name == ct_token_asset_name

        True
      }
    }
  }

  else(_) {
    fail
  }
}
