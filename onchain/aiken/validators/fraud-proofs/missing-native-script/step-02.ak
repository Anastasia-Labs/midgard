use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/common/constants
use midgard/common/utils
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proofs/common.{
  get_singleton_ct_token, get_tx_root_validate_block_hash, handle_cancel,
  validate_final_step_output, validate_step_output,
}
use midgard/fraud_proofs/missing_native_script/step_01.{StepState as Step1State}
use midgard/fraud_proofs/missing_native_script/step_02.{
  Cancel, Continue, StepRedeemer as Step2Redeemer,
}
use midgard/ledger_state.{Plutus, Timelock}

validator main(
  // Fraud proof token policy
  fraud_proof_token_policy: PolicyId,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion: ScriptHash,
  // Computation thread token policy
  ct_token_policy_id: PolicyId,
  // State queue token policy
  state_queue_policy_id: PolicyId,
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
    expect Script(own_script_hash) = own_input.output.address.payment_credential

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
      Continue {
        bad_state_queue_node_ref_input_index,
        script_wit_to_delete_key,
        script_wit_to_delete_value,
        script_wits_delete_proof,
        script_wits_new_root,
        index_output,
        computation_thread_redeemer_index,
      } -> {
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)
        let extracted_tx_root =
          get_tx_root_validate_block_hash(
            tx,
            bad_state_queue_node_ref_input_index,
            ct_token_asset_name,
            state_queue_policy_id,
          )

        // -------- Missing native script: validation-specific logic START --------
        expect initialState: Step1State = step_datum.data
        //If you try to delete the bad output from the merkletree, it means it existed!
        expect
          initialState.bad_output_script_hash != when
            script_wit_to_delete_value
          is {
            Timelock(script) -> script
            Plutus(script) -> script
          }
        //Continuing the recursion of the merkle proof deletion to demonstrate that 
        //(script_hash(address(u)), s) doesn't belong to script_tx_wits(tx)
        expect
          utils.plutarch_pdelete(
            initialState.current_root,
            script_wit_to_delete_key,
            script_wit_to_delete_value,
            script_wits_delete_proof,
            tx.redeemers,
          ) == script_wits_new_root
        // -------- Missing native script: validation-specific logic END --------
        if script_wits_new_root == constants.empty_merkle_tree_root {
          //Base step
          expect
            validate_final_step_output(
              tx,
              index_output,
              fraud_proof_token_policy,
              ct_token_asset_name,
              step_datum,
              computation_thread_redeemer_index,
            )
        } else {
          //Recursion step
          let step_1_state =
            Step1State {
              bad_output_script_hash: initialState.bad_output_script_hash,
              current_root: script_wits_new_root,
            }
          let expected_output_datum =
            StepDatum {
              fraud_prover: step_datum.fraud_prover,
              data: step_1_state,
            }

          expect
            validate_step_output(
              tx,
              index_output,
              own_script_hash,
              ct_token_policy_id,
              ct_token_asset_name,
              step_datum,
              expected_output_datum,
            )
        }

        True
      }
    }
  }

  else(_) {
    fail
  }
}
