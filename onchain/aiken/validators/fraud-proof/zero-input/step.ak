use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/linked_list/unordered as unordered
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/common/constants
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofInputs}
use midgard/fraud_proof/common/utils.{
  get_singleton_ct_token, get_tx_root_validate_block_hash, handle_cancel,
  validate_final_step_output, validate_membership_proof,
} as fraud_proof_utils
use midgard/fraud_proof/zero_input/step.{
  Cancel, Continue, StepRedeemer as Step1Redeemer,
}
use midgard/ledger_state.{Header}

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

        // -------- Zero input: validation-specific logic START --------
        // Membership proof for tx
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

        verify_hash_32(bad_tx, bad_tx_hash)
        verify_hash_32(bad_tx_body, bad_tx.body)

        //The merkletree of inputs must be empty
        expect bad_tx_body.spend_inputs == constants.empty_merkle_tree_root

        // -------- Zero input: validation-specific logic END --------
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

pub fn get_validity_range_validate_block_hash(
  tx: Transaction,
  state_queue_node_ref_input_index: Int,
) -> (Int, Int) {
  // Reference input with block_hash 
  expect Some(state_queue_node_input) =
    list.at(tx.reference_inputs, state_queue_node_ref_input_index)

  // Extract tx_root via the state queue block hash
  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum
  expect state_queue_node_datum: unordered.NodeDatum = state_queue_datum_data
  expect parsed_state_queue_datum: Header = state_queue_node_datum.data
  (parsed_state_queue_datum.start_time, parsed_state_queue_datum.end_time)
}
