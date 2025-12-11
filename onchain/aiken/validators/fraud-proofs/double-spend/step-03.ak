use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proofs/common.{
  get_singleton_ct_token, handle_cancel, validate_step_output,
} as fraud_proof_utils
use midgard/fraud_proofs/double_spend/step_02.{StepState as Step2State}
use midgard/fraud_proofs/double_spend/step_03.{
  Cancel, Continue, StepRedeemer as Step3Redeemer, StepState as Step3State,
}
use midgard/ledger_state.{MidgardTxBodyCompact, MidgardTxCompact}

validator main(
  double_spend_step_04_validator_script_hash: ScriptHash,
  ct_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<StepDatum>,
    redeemer: Step3Redeemer,
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

      // Step 3: Double spend validation
      Continue {
        fraudulent_input,
        tx1_input_utxos,
        tx1_body,
        tx2_input_utxos,
        tx2_body,
        tx1,
        tx2,
        index_output,
      } -> {
        expect Step2State { verified_tx1_hash, verified_tx2_hash }: Step2State =
          step_datum.data

        // -------- double spend validation specific logic START --------
        // fraud prover provides a list of output references that are known to be part of the fraudulent block as the tx1, and tx2 full transactions,
        // once serialized and hashed match the verified_tx1_hash and verified_tx2_hash hashes we have verified to be a part of the supposedly "fraudulent" block in previous steps.
        verify_hash_32(tx1, verified_tx1_hash)
        verify_hash_32(tx2, verified_tx2_hash)

        // tx bodies provided by fraud prover, once serialized and hashed, match the hashed body field part of the inputted transactions
        verify_hash_32(tx1_body, tx1.body)
        verify_hash_32(tx2_body, tx2.body)

        // input lists match tx body spend_inputs
        verify_hash_32(tx1_input_utxos, tx1_body.spend_inputs)
        verify_hash_32(tx2_input_utxos, tx2_body.spend_inputs)

        // both input lists contain the fraudulent input
        expect list.has(builtin.un_list_data(tx1_input_utxos), fraudulent_input)
        expect list.has(builtin.un_list_data(tx2_input_utxos), fraudulent_input)
        expect parsed_fraudulent_input: OutputReference = fraudulent_input

        // -------- double spend validation specific logic END --------
        let step_3_state =
          Step3State {
            verified_tx1_hash,
            verified_tx2_hash,
            verified_common_input: parsed_fraudulent_input,
          }
        let expected_output_datum =
          StepDatum {
            fraud_prover: step_datum.fraud_prover,
            data: step_3_state,
          }

        // Get CT token
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)

        expect
          validate_step_output(
            tx,
            index_output,
            double_spend_step_04_validator_script_hash,
            ct_token_policy_id,
            ct_token_asset_name,
            step_datum,
            expected_output_datum,
          )

        True
      }
    }
  }

  else(_) {
    fail
  }
}
