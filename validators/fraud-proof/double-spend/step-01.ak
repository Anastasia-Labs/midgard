use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}
use aiken_design_patterns/linked_list/ordered.{NodeDatum}
use aiken_design_patterns/merkelized_validator
use cardano/address.{Script}
use cardano/assets.{PolicyId, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/double_spend.{validate_output_to_fraud_prover}
use midgard/ledger_state
use midgard/state_queue

// -------- GOAL DEFINITION --------------------------------------
// double_spend fraud proof: Proves membership Merkle proofs for 2 unique txs who spent the same input UTxO

// tx1 and tx2 refers to entire tx that contain:
// Input UTxOs it's trying to spend
// Output UTxOs it's creating

// Double spending occurs when the same UTxO appears as an input in two different txs
// Transaction1 (tx1):
//   Inputs (tx_in1): [UTxO_A]
//   Outputs (tx_out1): [UTxO_B, UTxO_C]

// Transaction2 (tx2):
//   Inputs (tx_in2): [UTxO_A]  <- Same UTxO as tx1
//   Outputs (tx_out2): [UTxO_D]

// Common input: UTxO_A
// Verify i is an input of tx_1 and tx_2:
// Merkle proof of membership: i ∈ tx_1.inputs_root ∧ i ∈ tx_2.inputs_root

// -------- EXPECTATIONS ---------------------------------------
// -- tx_root provided from reference input of block header

// TODO
pub type Proof =
  Void

// TODO
pub type MembershipProofInputs {
  tx_root: ByteArray,
  tx1_hash: ByteArray,
  tx1_info_hash: ByteArray,
  tx1_to_root_proof_path: Proof,
}

pub type Redeemer {
  Cancel
  Continue {
    frauded_state_queue_node_ref_input_index: Int,
    unverified_frauded_state_queue_tx1_hash: ByteArray,
    unverified_frauded_state_queue_tx1_info_hash: ByteArray,
    unverified_frauded_state_queue_tx1_to_root_proof_path: Proof,
  }
}

pub type DoubleSpendStep1State {
  verified_tx1_hash: ByteArray,
}

validator step_01(
  // Script hash of next step (requires backwards compilation)
  double_spend_s_2_validator_script_hash: ScriptHash,
  // Script hash of stake validator (forwarded logic)
  plutarch_merkle_inclusion_validator: ScriptHash,
  // Computation thread token policy
  ct_token_policy_id: PolicyId,
) {
  spend(
    maybe_datum: Option<StepDatum>,
    redeemer: Redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = maybe_datum
    when redeemer is {
      // --------- REDEEMER: CANCEL -----------------------------------
      // (1) Burn the computation thread token 
      // (2) Return the ADA from the computation thread utxo to the fraud prover pub-key defined in the input datum
      // (3) Transaction must not mint or burn any other tokens
      Cancel -> {
        // (1.1) Get ct token asset name
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        expect Some((_, _, lovelace)) =
          list.at(flatten(own_input.output.value), 0)
        // (2.1)
        expect Some(output_to_fraud_prover) = list.at(self.outputs, 0)
        and {
          // (1.2)
          self.mint == from_asset(
            policy_id: ct_token_policy_id,
            asset_name: ct_token_asset_name,
            quantity: -1,
          ),
          // (2.2)
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          ),
          quantity_of(output_to_fraud_prover.value, "", "") == lovelace,
        }
      }

      // --------- REDEEMER: CONTINUE -------------------------------
      // (1) Reference a state queue block with a block hash matching the last 28 bytes of the computation thread token name
      // (2) Delegate compute to the merkleized validator to verify the inclusion of tx1 in the tx_root
      // (3) The fraud prover field must match between the output state and the input datum
      // (4) Verify the state transition from the previous state to the new state
      // (5) The computation thread token must be sent to the next step’s spending validator
      // (6) Transaction must not mint or burn any other tokens
      // -------- PARAMETERS DEFINITION ------------------------------
      // (i) state queue block with a block hash matching the last 28 bytes of the computation thread token name
      // (ii) tx_root, merkle root of transactions in the state queue block
      // (iii) tx_hash, hash of the transaction that is being proven to be in tx_root
      // (iv) proof_path: minimum set of hashes needed to reconstruct the path from the tx to the root
      Continue {
        frauded_state_queue_node_ref_input_index,
        unverified_frauded_state_queue_tx1_hash,
        unverified_frauded_state_queue_tx1_info_hash,
        unverified_frauded_state_queue_tx1_to_root_proof_path,
      } -> {
        // (1.1) Reference input with block_hash (last 28 bytes -> ct_token_asset_name)
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        expect InlineDatum(frauded_state_queue_datum_data) =
          frauded_state_queue_node_input.output.datum
        expect frauded_state_queue_node_datum: state_queue.Datum =
          frauded_state_queue_datum_data
        expect parsed_state_queue_datum: ledger_state.Header =
          frauded_state_queue_node_datum.data
        // TODO: `Header` does not contain the block hash. Which one should
        // change?
        // let block_hash = parsed_state_queue_datum.block_hash
        let block_hash = parsed_state_queue_datum.utxos_root
        // (1.2) Get ct_token_asset_name
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        // (2) -- Awaiting observer scripts to circumvent registering the stake validator --
        // Until then: Delegate merkle proof verification to a stake validator - call out midgard-merkle endpoint to 
        // verify the inclusion of tx1 in tx_root 
        // midgard-merkle endpoints are assumed to contain stake validator withdraw logic to verify merkle inclusion proofs 
        // TODO: (2.1) Extract tx_root from block_hash (implementation depends on block header format)
        // "data: block_compact" from codec.cddl ?
        // let extracted_tx_root = unwrap block_hash to get tx_root? block_hash.tx_root
        // (2.2) Forward proof logic to a stake validator (plutarch_merkle_inclusion_validator) 
        // The plutarch membership proof script will likely have multiple redeemer "endpoints" and we need to ensure that
        // always a specific/intended redeemer is being executed 
        // This function expects the script hash of the membership proof validator written in Plutarch
        // and a function that takes the redeemer format expected by the membership proof validator and outputs a Boolean 
        // Fraud prover inputs `Redeemer` arguments
        // They should match the redeemer arguments of the stake validator 
        // Essentially extracting fields from fraud prover data (parameters of the Redeemer) 
        // and matching them to the arguments expected by the stake validator (withdraw_redeemer)
        // TODO: `extracted_tx_root` was undefined. I've used an empty ByteArray
        // to bypass the compiler.
        let extracted_tx_root = #""
        expect
          merkelized_validator.delegated_validation(
            function_input: MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx1_hash: unverified_frauded_state_queue_tx1_hash,
              tx1_info_hash: unverified_frauded_state_queue_tx1_info_hash,
              tx1_to_root_proof_path: unverified_frauded_state_queue_tx1_to_root_proof_path,
            },
            staking_validator: plutarch_merkle_inclusion_validator,
            input_data_coercer: fn(withdraw_redeemer: Data) {
              expect coerced: MembershipProofInputs = withdraw_redeemer
              coerced
            },
            redeemers: self.redeemers,
          )

        // (3) Fraud prover field must match between the output state and the input datum
        expect Some(output_to_step_2) = list.at(self.outputs, 0)
        expect InlineDatum(output_to_step_2_datum) = output_to_step_2.datum
        expect parsed_output_to_step_2_datum: StepDatum = output_to_step_2_datum

        // (4.1) Define state transition 
        // At this point, tx1_hash has been verified by the Plutarch validator       
        let step1_state =
          DoubleSpendStep1State {
            verified_tx1_hash: unverified_frauded_state_queue_tx1_hash,
          }
        let step_1_datum =
          StepDatum {
            fraud_prover: parsed_output_to_step_2_datum.fraud_prover,
            data: step1_state,
          }

        // (5.1) Step 2 spending validator hash from output
        expect Script(hash) = output_to_step_2.address.payment_credential

        and {
          // (1.3) tx_root matches block_hash
          drop(ct_token_asset_name, n: 28) == block_hash,
          // (3) Fraud prover field matches
          parsed_output_to_step_2_datum.fraud_prover == step_datum.fraud_prover,
          // (4.2) State transition verified
          step_1_datum == parsed_output_to_step_2_datum,
          // (5.2) Next step spending validator hash matches
          hash == double_spend_s_2_validator_script_hash,
          // (6) No other mints or burns
          quantity_of(
            output_to_step_2.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ) == 1,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
// --------- MERKLE VALIDATOR EXAMPLE APPROACH ----------------------------
// merkle validator example would be:
// let [has_tx_hash] =
//   MerkleInputs { frauded_state_queue_tx_root, frauded_state_queue_tx1_hash, frauded_state_queue_tx1_info_hash, frauded_state_queue_tx1_to_root_proof_path } // (2.1)
//   |> merkelized_validator.delegated_compute(
//         plutarch_merkle_inclusion_validator,
//         // Input coercer - handles different types based on index  
//         fn(d: Data) -> MerkleInputs { expect merkle_inputs: MerkleInputs = d merkle_inputs },
//         // Output coercer
//         fn(d: Data) -> Bool { expect bool_result: Bool = d bool_result },
//         tx.redeemers,
//       )

// validator inclusion_proof {
//       withdraw(
//         redeemer: WithdrawRedeemer<MerkleParameters, Boolean>,
//         _own_credential: Credential,
//         _self: Transaction,
//       ) {
//        let MerkleInputs {tx_root, tx_hash, tx_info_hash, proof} = redeemer
//       }
//         has_tx_hash(tx_root, tx_hash, tx_info_hash, proof)
//     }

// Conclusion: As we are not interested in manipulating the return value of a membership proof other than True/False, we will use stake validator instead.
