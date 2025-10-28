//// Double Spend Fraud Proof Validator
////
//// This is the abstracted version of the double spend fraud proof validator that uses
//// common utilities to reduce code duplication.
////
//// To see the original implementation that shows the raw logic of fraud proofs, you can view the diff at:
//// https://github.com/Anastasia-Labs/midgard/pull/91

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
use midgard/fraud_proof/double_spend/step_01.{
  Cancel, Continue, StepRedeemer as Step1Redeemer, StepState as Step1State,
}
use midgard/hub_oracle as hub

// ----------------- DOUBLE SPEND FRAUD PROOF: DEFINITION --------------------------
// Proves membership Merkle proofs for 2 unique txs which spends the same input UTxO

// Transaction1 (tx1):
//   Inputs (tx_in1): [UTxO_A]
//   Outputs (tx_out1): [UTxO_B, UTxO_C]

// Transaction2 (tx2):
//   Inputs (tx_in2): [UTxO_A]  <- Same UTxO as tx1
//   Outputs (tx_out2): [UTxO_D]

validator main(
  // Script hash of next step (backwards compilation order of step validators required)
  double_spend_step_02_validator_script_hash: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator: ScriptHash,
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
        bad_tx1_hash,
        bad_tx1_info_hash,
        bad_tx1_to_root_proof_path,
        index_output,
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

        // -------- Double spend validation specific logic START --------
        // Membership proof for tx1
        expect
          validate_membership_proof(
            tx,
            plutarch_midgard_merkle_validator,
            MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: bad_tx1_hash,
              tx_info_hash: bad_tx1_info_hash,
              tx_to_root_proof_path: bad_tx1_to_root_proof_path,
            },
          )

        // -------- Double spend validation specific logic END --------
        let step_1_state = Step1State { verified_tx1_hash: bad_tx1_hash }
        let expected_output_datum =
          StepDatum {
            fraud_prover: step_datum.fraud_prover,
            data: step_1_state,
          }

        expect
          validate_step_output(
            tx,
            index_output,
            double_spend_step_02_validator_script_hash,
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
