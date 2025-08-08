//// Fraud proof validator for the non-existing input violation.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{VerificationKey}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{InclusionProofUtxo, MembershipProofInputs}
use midgard/fraud_proof/common/utils.{
  get_prev_utxo_root, get_singleton_ct_token, get_tx_root_validate_block_hash,
  handle_cancel, validate_final_step_output, validate_inclusion_utxo_proof,
  validate_membership_proof,
} as fraud_proof_utils
use midgard/fraud_proof/missing_req_signer_utxo/step.{
  Cancel, Continue, StepRedeemer as Step1Redeemer,
}

validator main(
  // Fraud proof token policy
  fraud_proof_token_policy: PolicyId,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion: ScriptHash,
  plutarch_midgard_merkle_validator_inclusion_parent_utxo: ScriptHash,
  // Computation thread token policy
  ct_token_policy_id: PolicyId,
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
        parent_utxo,
        parent_utxo_info_hash,
        parent_utxo_to_root_proof_path,
        bad_spend_inputs,
        required_signers,
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
          )

        let prev_utxos_root =
          get_prev_utxo_root(tx, bad_state_queue_node_ref_input_index)

        // -------- Missing required signature: validation-specific logic START --------
        // Membership proof for tx1
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
        verify_hash_32(bad_tx, bad_tx_hash)
        verify_hash_32(bad_tx_body, bad_tx.body)
        verify_hash_32(bad_spend_inputs, bad_tx_body.spend_inputs)

        expect parent_input: ByteArray = parent_utxo

        //Validation that parent utxo is in the prev block
        expect
          validate_inclusion_utxo_proof(
            tx,
            plutarch_midgard_merkle_validator_inclusion_parent_utxo,
            InclusionProofUtxo {
              utxo_root: prev_utxos_root,
              valid_utxo: parent_input,
              valid_utxo_info_hash: parent_utxo_info_hash,
              utxo_to_root_proof_path: parent_utxo_to_root_proof_path,
            },
          )
        // Exclusion proof that bad_input.tx_hash is not in the block's tx_root.
        expect invalid_input: Input = parent_utxo
        //let's look for the output at the right index
        expect
          list.has(
            builtin.un_list_data(bad_spend_inputs),
            invalid_input.output_reference,
          )

        //retrieval of the pubkeyhash of the input
        expect VerificationKey(missing_signature) =
          invalid_input.output.address.payment_credential

        //let's verify the list of required signers passed by the user in the redeemer
        verify_hash_32(required_signers, bad_tx_body.required_signer_hashes)
        //the list of signers does not include the one of the bad input
        expect !list.has(required_signers, missing_signature)
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
