use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}
use aiken_design_patterns/linked_list/unordered.{Key, NodeDatum}
use aiken_design_patterns/merkelized_validator
use cardano/address.{Script}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/common/utils.{get_singleton_asset_with_policy} as get_singleton_asset_with_policy
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofInputs}
use midgard/fraud_proof/common/utils.{validate_output_to_fraud_prover}
use midgard/fraud_proof/double_spend/step_01.{StepState as Step1State}
use midgard/fraud_proof/double_spend/step_02.{
  Cancel, Continue, StepRedeemer as Step2Redeemer, StepState as Step2State,
}
use midgard/ledger_state
use midgard/state_queue

validator main(
  // Script hash of next step (backwards compilation order of step validators required)
  double_spend_step_03_validator_script_hash: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator: ScriptHash,
  // Computation thread token policy
  ct_token_policy_id: PolicyId,
) {
  spend(
    a_datum: Option<StepDatum>,
    redeemer: Step2Redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = a_datum
    when redeemer is {
      // -------------- REDEEMER: CANCEL -----------------
      // (1) Burn the computation thread token 
      // (2) Return the ADA from the computation thread utxo to the fraud prover pub-key defined in the input datum
      // (3) Transaction must not mint or burn any other tokens
      // --------------------------------------------------
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

      Continue {
        bad_state_queue_node_ref_input_index,
        bad_tx2_hash,
        bad_tx2_info_hash,
        bad_tx2_to_root_proof_path,
        index_output,
      } -> {
        // Check for membership tx_2 in the fraudulent blocks extracted tx_root from previous step output state
        // we get the previous datum and find the tx_hash that is the one we want to check
        expect Step1State { verified_tx1_hash }: Step1State = step_datum.data
        // Check for tx hash uniqueness
        expect bad_tx2_hash != verified_tx1_hash

        // (1.1) Reference input with block_hash 
        expect Some(bad_state_queue_node_input) =
          list.at(self.reference_inputs, bad_state_queue_node_ref_input_index)

        // (1.2) Extract tx_root via the state queue block hash
        expect InlineDatum(bad_state_queue_datum_data) =
          bad_state_queue_node_input.output.datum
        expect bad_state_queue_node_datum: state_queue.Datum =
          bad_state_queue_datum_data
        expect parsed_state_queue_datum: ledger_state.Header =
          bad_state_queue_node_datum.data
        expect Key(block_hash) = bad_state_queue_node_datum.key
        let extracted_tx_root = parsed_state_queue_datum.transactions_root

        // (1.3) Verify the computation thread token by asset name in the output
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Pair(ct_token_asset_name, 1) =
          get_singleton_asset_with_policy(
            own_input.output.value,
            ct_token_policy_id,
          )
        // (1.4) tx_root provided by fraud prover in the redeemer matches block_hash from the state queue block from the reference input (1.1) (ct_token_asset_name: last 28 bytes is the block_hash)
        expect drop(ct_token_asset_name, n: 4) == block_hash

        // (2) Forward proof logic to a stake validator (plutarch_midgard_merkle_validator)
        // Delegate merkle proof verification logic to a stake validator - call out midgard-merkle endpoint to verify the inclusion of tx1 in tx_root 
        // The plutarch membership proof script will likely have multiple redeemer "endpoints" and we need to ensure that always a specific/intended redeemer is being executed 
        // This function expects the script hash of the membership proof validator written in Plutarch & a function that takes the redeemer format expected by the membership proof validator and outputs a Boolean 
        // Fields from fraud prover input (parameters of the Continue Redeemer) need to be matched to the redeemer arguments expected by the stake validator (withdraw_redeemer)
        expect
          merkelized_validator.delegated_validation(
            function_input: MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: bad_tx2_hash,
              tx_info_hash: bad_tx2_info_hash,
              tx_to_root_proof_path: bad_tx2_to_root_proof_path,
            },
            staking_validator: plutarch_midgard_merkle_validator,
            input_data_coercer: fn(withdraw_redeemer: Data) {
              expect coerced: MembershipProofInputs = withdraw_redeemer
              coerced
            },
            redeemers: self.redeemers,
          )

        // (3) The fraud prover field must match between the output state and the input datum
        expect Some(output_to_step_3) = list.at(self.outputs, index_output)

        // (3.2) Fraud prover field of the input datum matches output state fraud prover field
        expect InlineDatum(output_to_step_3_datum) = output_to_step_3.datum
        expect parsed_output_to_step_3_datum: StepDatum = output_to_step_3_datum
        expect
          parsed_output_to_step_3_datum.fraud_prover == step_datum.fraud_prover

        // (4) Verify the state transition from the previous state to the new state
        // (4.1) Define state transition (input -> output)    
        let step_2_state =
          Step2State { verified_tx1_hash, verified_tx2_hash: bad_tx2_hash }
        expect parsed_step_2_state: Step2State =
          parsed_output_to_step_3_datum.data
        // (4.2) State transition verified to be correct in the output
        expect step_2_state == parsed_step_2_state

        // (5) Verify that output destination matches the expected hash of the next step spending validator
        expect Script(hash) = output_to_step_3.address.payment_credential
        expect hash == double_spend_step_03_validator_script_hash

        // (6) Computation thread token sent to the next step validator
        expect
          Pair(ct_token_asset_name, 1) == get_singleton_asset_with_policy(
            output_to_step_3.value,
            ct_token_policy_id,
          )
        True
      }
    }
  }

  else(_) {
    fail
  }
}
