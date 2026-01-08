//// Fraud proof validator for the non-existing input violation.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofInputs}
use midgard/fraud_proof/common/utils.{
  get_singleton_ct_token, get_tx_root_validate_block_hash, handle_cancel,
  validate_final_step_output, validate_membership_proof,
} as fraud_proof_utils
use midgard/fraud_proof/non_existing_input_no_index/step.{
  Cancel, Continue, StepRedeemer as Step1Redeemer,
}
use midgard/hub_oracle as hub

validator main(
  // Fraud proof token policy
  fraud_proof_token_policy: PolicyId,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_input_hash_inclusion: ScriptHash,
  // Computation thread token policy
  ct_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<StepDatum>,
    redeemer: Step1Redeemer,
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
      // Step 1: Membership proof for tx1
      Continue {
        bad_state_queue_node_ref_input_index,
        bad_tx_hash,
        bad_tx_info_hash,
        bad_tx_to_root_proof_path,
        bad_tx,
        bad_tx_body,
        bad_spend_inputs,
        bad_input_index,
        bad_input_as_data,
        bad_input_membership_proof,
        bad_input_tx_info_hash,
        bad_input_tx_to_root_proof_path,
        index_output,
        computation_thread_redeemer_index,
        previous_tx,
        previous_tx_body,
        real_outputs,
        hub_ref_input_index,
      } -> {
        //TODO get it from the oracle hub
        let hub.Datum { state_queue, .. } =
          hub.get_datum(tx.reference_inputs, hub_oracle, hub_ref_input_index)
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)
        let extracted_tx_root =
          get_tx_root_validate_block_hash(
            tx,
            bad_state_queue_node_ref_input_index,
            ct_token_asset_name,
            state_queue,
          )

        // -------- Non-existent input: validation-specific logic START --------
        // Membership proof for tx1
        //we have a transaction that is attemping to spend utxo HASH#INDEX
        //HASH is actually inside the current block however the INDEX is an invalid index
        expect
          validate_membership_proof(
            tx,
            plutarch_midgard_merkle_validator_inclusion,
            MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: bad_tx_hash,
              tx_info_hash: bad_tx_info_hash,
              tx_to_root_proof_path: bad_tx_to_root_proof_path,
            },
          )

        // Membership of the bad_input among the tx1 spent inputs
        //validation of the redeemer data
        verify_hash_32(bad_tx, bad_tx_hash)
        verify_hash_32(bad_tx_body, bad_tx.body)

        //Prove that bad_input belongs to inputs(tx)
        expect
          utils.plutarch_phas(
            bad_tx_body.spend_inputs,
            bad_input_index,
            bad_input_as_data,
            bad_input_membership_proof,
            tx.redeemers,
          )

        // Inclusion proof that the transaction id of the bad input is in current block txs
        expect invalid_input: Input = bad_input_as_data
        expect
          validate_membership_proof(
            tx,
            plutarch_midgard_merkle_validator_input_hash_inclusion,
            MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: invalid_input.output_reference.transaction_id,
              tx_info_hash: bad_input_tx_info_hash,
              tx_to_root_proof_path: bad_input_tx_to_root_proof_path,
            },
          )

        //now we know that we have an input that has a proper txHash because it's in a merkle tree
        //Validation of the redeemer info regardin the previous transaction
        verify_hash_32(
          previous_tx,
          invalid_input.output_reference.transaction_id,
        )
        verify_hash_32(previous_tx_body, previous_tx.body)
        verify_hash_32(real_outputs, previous_tx_body.outputs)
        //the previous transaction ouput size is smaller than the output index of the invalid utxo, doing -1 because size=1 means output index=0
        expect
          invalid_input.output_reference.output_index > list.length(
            builtin.un_list_data(real_outputs),
          ) - 1

        // -------- Non-existent input: validation-specific logic END --------
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
