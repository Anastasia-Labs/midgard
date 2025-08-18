//// Fraud proof validator for the non-existing input violation.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{
  ExclusionProofInputTx, ExclusionProofUtxo, MembershipProofInputs,
}
use midgard/fraud_proof/common/utils.{
  get_prev_utxo_root, get_singleton_ct_token, get_tx_root_validate_block_hash,
  handle_cancel, validate_exclusion_proof, validate_exclusion_utxo_proof,
  validate_final_step_output, validate_membership_proof,
} as fraud_proof_utils
use midgard/fraud_proof/non_existing_input/step.{
  Cancel, Continue, StepRedeemer as Step1Redeemer,
}

// ----------------- DOUBLE SPEND FRAUD PROOF: DEFINITION --------------------------
// Proves membership Merkle proofs for 2 unique txs which spends the same input UTxO

// Transaction1 (tx1):
//   Inputs (tx_in1): [UTxO_A]
//   Outputs (tx_out1): [UTxO_B, UTxO_C]

// Transaction2 (tx2)
//   Inputs (tx_in2): [UTxO_A]  <- Same UTxO as tx1
//   Outputs (tx_out2): [UTxO_D]

validator main(
  // Fraud proof token policy
  fraud_proof_token_policy: PolicyId,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_utxo_exclusion: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_exclusion: ScriptHash,
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
        bad_spend_inputs,
        bad_input,
        bad_input_utxo_info_hash,
        bad_input_utxo_to_root_proof_path,
        bad_input_tx_info_hash,
        bad_input_tx_to_root_proof_path,
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

        // -------- Non-existent input: validation-specific logic START --------
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

        expect list.has(builtin.un_list_data(bad_spend_inputs), bad_input)

        // Exclusion proof that bad_input is not in the block's prev_utxos_root.
        expect True
        let extracted_prev_utxo_root =
          get_prev_utxo_root(tx, bad_state_queue_node_ref_input_index)
        expect invalid_input: ByteArray = bad_input
        expect
          validate_exclusion_utxo_proof(
            tx,
            plutarch_midgard_merkle_validator_utxo_exclusion,
            ExclusionProofUtxo {
              utxo_root: extracted_prev_utxo_root,
              invalid_utxo: invalid_input,
              invalid_utxo_info_hash: bad_input_utxo_info_hash,
              utxo_to_root_proof_path: bad_input_utxo_to_root_proof_path,
            },
          )

        // Exclusion proof that bad_input.tx_hash is not in the block's tx_root.
        expect invalid_input: Input = bad_input
        expect
          validate_exclusion_proof(
            tx,
            plutarch_midgard_merkle_validator_exclusion,
            ExclusionProofInputTx {
              tx_root: extracted_tx_root,
              invalid_input_tx_hash: invalid_input.output_reference.transaction_id,
              invalid_input_info_hash: bad_input_tx_info_hash,
              input_to_root_proof_path: bad_input_tx_to_root_proof_path,
            },
          )

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
