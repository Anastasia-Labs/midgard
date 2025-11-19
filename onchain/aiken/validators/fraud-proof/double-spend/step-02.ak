use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofInputs}
use midgard/fraud_proof/common/utils.{
  get_singleton_ct_token, get_tx_root_validate_block_hash, handle_cancel,
  validate_membership_proof, validate_step_output,
}
use midgard/fraud_proof/double_spend/step_01.{StepState as Step1State}
use midgard/fraud_proof/double_spend/step_02.{
  Cancel, Continue, StepRedeemer as Step2Redeemer, StepState as Step2State,
}
use midgard/hub_oracle as hub

validator main(
  double_spend_step_03_validator_script_hash: ScriptHash,
  plutarch_midgard_merkle_validator: ScriptHash,
  ct_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<StepDatum>,
    redeemer: Step2Redeemer,
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
      // Step 2: Membership proof for tx2 and uniqueness check
      Continue {
        bad_state_queue_node_ref_input_index,
        bad_tx2_hash,
        bad_tx2_info_hash,
        bad_tx2_to_root_proof_path,
        index_output,
        hub_ref_input_index,
      } -> {
        //TODO get it from the oracle hub
        let hub.Datum { state_queue, .. } =
          hub.get_datum(tx.reference_inputs, hub_oracle, hub_ref_input_index)
        expect Step1State { verified_tx1_hash }: Step1State = step_datum.data
        expect bad_tx2_hash != verified_tx1_hash
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)
        let extracted_tx_root =
          get_tx_root_validate_block_hash(
            tx,
            bad_state_queue_node_ref_input_index,
            ct_token_asset_name,
            state_queue,
          )

        // -------- double spend validation specific logic START --------
        // Membership proof for tx2
        expect
          validate_membership_proof(
            tx,
            plutarch_midgard_merkle_validator,
            MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: bad_tx2_hash,
              tx_info_hash: bad_tx2_info_hash,
              tx_to_root_proof_path: bad_tx2_to_root_proof_path,
            },
          )

        // -------- double spend validation specific logic END --------
        let step_2_state =
          Step2State { verified_tx1_hash, verified_tx2_hash: bad_tx2_hash }
        let expected_output_datum =
          StepDatum {
            fraud_prover: step_datum.fraud_prover,
            data: step_2_state,
          }

        expect
          validate_step_output(
            tx,
            index_output,
            double_spend_step_03_validator_script_hash,
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
