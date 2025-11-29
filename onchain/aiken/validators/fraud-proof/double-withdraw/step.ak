//// Fraud proof validator for the non-existing input violation.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofWithdrawal}
use midgard/fraud_proof/common/utils.{
  get_singleton_ct_token, get_withdrawals_root, handle_cancel,
  validate_final_step_output, validate_withdrawal_inclusion_proof,
} as fraud_proof_utils
use midgard/fraud_proof/double_withdraw/step.{
  Cancel, Continue, StepRedeemer as Step1Redeemer,
}
use midgard/ledger_state.{WithdrawalInfo}

// ----------------- DOUBLE WITHDRAWAL FRAUD PROOF: DEFINITION --------------------------
// Proves membership Merkle proofs for 2 unique withdrawals referencing the same output reference
// we have 2 withdrawal events, and the proof that they are part of the merkle tree, we have to verify they are indeed part of it and they have same output_reference

validator main(
  // Fraud proof token policy
  fraud_proof_token_policy: PolicyId,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion_first: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion_second: ScriptHash,
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
      // Step 1: Membership of both withdrawals
      Continue {
        bad_state_queue_node_ref_input_index,
        first_withdrawal_info,
        first_withdrawal_info_hash,
        first_withdrawal_to_root_proof_path,
        second_withdrawal_info,
        second_withdrawal_info_hash,
        second_withdrawal_to_root_proof_path,
        index_output,
        computation_thread_redeemer_index,
      } -> {
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)
        let extracted_withdrawals_root =
          get_withdrawals_root(tx, bad_state_queue_node_ref_input_index)

        expect first_info_byte_array: ByteArray = first_withdrawal_info
        expect second_info_byte_array: ByteArray = second_withdrawal_info

        expect
          validate_withdrawal_inclusion_proof(
            tx,
            plutarch_midgard_merkle_validator_inclusion_first,
            MembershipProofWithdrawal {
              withdrawal_root: extracted_withdrawals_root,
              withdrawal_info: first_info_byte_array,
              withdrawal_info_hash: first_withdrawal_info_hash,
              withdrawal_to_root_proof_path: first_withdrawal_to_root_proof_path,
            },
          )

        expect
          validate_withdrawal_inclusion_proof(
            tx,
            plutarch_midgard_merkle_validator_inclusion_second,
            MembershipProofWithdrawal {
              withdrawal_root: extracted_withdrawals_root,
              withdrawal_info: second_info_byte_array,
              withdrawal_info_hash: second_withdrawal_info_hash,
              withdrawal_to_root_proof_path: second_withdrawal_to_root_proof_path,
            },
          )

        expect first_witdrawal_parsed: WithdrawalInfo = first_withdrawal_info
        expect second_witdrawal_parsed: WithdrawalInfo = second_withdrawal_info

        expect
          first_witdrawal_parsed.l2_outref == second_witdrawal_parsed.l2_outref

        // -------- Double withdrawal: validation-specific logic END --------
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
