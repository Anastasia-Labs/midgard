// imports 
// midgard-merkle is used as a library to verify merkle inclusion proofs 
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}
use aiken_design_patterns/linked_list/ordered.{Key, NodeDatum}
use aiken_design_patterns/merkelized_validator
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, Value, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}

// Invalid Input Reference fraud proof: A transaction tx in a proposed block contains an input referencing a nonexistent output.

// tx refers to entire tx that contain:
// Input UTxOs it's trying to spend
// Output UTxOs it's creating

// This fraud proof occurs when a UTxO appears as an input in a tx, however that input is not in the Midgard ledger
// Transaction (tx):
//   Inputs (tx_in): [UTxO_A] UTxO_A is not a valid input
//   Outputs (tx_out): [UTxO_B, UTxO_C]

// -------- Variable Definition ---------------------------------------
// (i) a state queue block with a block hash matching the last 28 bytes of the computation thread token name
// (ii) tx_root is merkle root of transactions in the state queue block
// (iii) tx_hash is the hash of the transaction that is being proven to be in tx_root
// (iv) input is the input inside the tx, that has no reference in the current state queue old_tx_root

//TODO This should be importend from a library, it's the state queue Datum
pub type StateDatum {
  block_hash: ByteArray,
  block_roots: BlockRoots,
}

pub type BlockRoots {
  block_withdraw_root: ByteArray,
  block_tx_root: ByteArray,
  block_deposit_root: ByteArray,
}

pub type MembershipProofInputs {
  root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  proof_path: ByteArray,
}

pub type InvalidInputSpendStep1State {
  tx_hash: ByteArray,
  merkle_proof: ByteArray,
}

pub type Step1redeemer {
  Cancel { index_output: Int }
  Continue {
    frauded_state_queue_node_ref_input_index: Int,
    frauded_state_queue_tx_hash: ByteArray,
    frauded_state_queue_tx_info_hash: ByteArray,
    tx_to_root_proof_path: ByteArray,
    index_output: Int,
  }
}

validator step_01(
  ct_token_policy_id: PolicyId,
  next_step_spending_validator: ScriptHash,
  stake_validator: ScriptHash,
) {
  spend(
    maybe_datum: Option<StepDatum>,
    redeemer: Step1redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = maybe_datum
    when redeemer is {
      Cancel { index_output } -> {
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        //let's find the assetname fraud token
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        expect Some((_, _, lovelace)) =
          list.at(flatten(own_input.output.value), 0)

        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)
        //The conditions are
        //- Burn the fraud token
        //- verify that the output is going to fraud prover address
        //- send the ada back to the prover
        and {
          self.mint == from_asset(
            policy_id: ct_token_policy_id,
            asset_name: ct_token_asset_name,
            quantity: -1,
          ),
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          ),
          quantity_of(output_to_fraud_prover.value, "", "") == lovelace,
        }
      }
      //  ------- Computation Step 1 - Membership Verification of tx ------- 
      // Step - 1: Fraud Prover Input: state_queue_node_ref_input_index, tx_hash, proof_path 
      // --------- PURPOSE - Continue Redeemer-------------------------------
      // (1) Reference a state queue block with a block hash matching the last 28 bytes of the computation thread token name
      // (2) Delegate compute to the merkleized validator to verify the inclusion of tx in the tx_root
      // (3) The fraud prover field must match between the output state and the input datum
      // (4) Verify the state transition from the previous state to the new state
      // (5) The computation thread token must be sent to the next step’s spending validator
      // (6) Transaction must not mint or burn any other tokens
      // --------------------------------------------------------------------
      Continue {
        frauded_state_queue_node_ref_input_index,
        frauded_state_queue_tx_hash,
        frauded_state_queue_tx_info_hash,
        tx_to_root_proof_path,
        index_output,
      } -> {
        // (1) Reference input with block_hash (tx_root) which carries the last 28 bytes of ct token name
        // --------------------------------------------------------------------
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        expect InlineDatum(frauded_state_queue_node_datum) =
          frauded_state_queue_node_input.output.datum

        expect parsed_node_datum: NodeDatum = frauded_state_queue_node_datum
        expect parsed_frauded_state_queue_node_datum: StateDatum =
          parsed_node_datum.data

        //let's extract BLOCKHASH and TX_ROOT from the block in state queue
        expect Key(block_hash) = parsed_node_datum.key
        let extracted_tx_root =
          parsed_frauded_state_queue_node_datum.block_roots.block_tx_root
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )

        //Let's find the assetname
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        //Let's find if the tx is part of the block in state queue
        let merkleized_logic =
          merkelized_validator.delegated_validation(
            function_input: MembershipProofInputs {
              root: extracted_tx_root,
              tx_hash: frauded_state_queue_tx_hash,
              tx_info_hash: frauded_state_queue_tx_info_hash,
              proof_path: tx_to_root_proof_path,
            },
            staking_validator: stake_validator,
            input_data_coercer: fn(withdraw_redeemer: Data) {
              expect coerced: MembershipProofInputs = withdraw_redeemer
              coerced
            },
            redeemers: self.redeemers,
          )

        //let's get the new datum
        expect Some(output_state) = list.at(self.outputs, index_output)
        expect InlineDatum(datum_output) = output_state.datum
        expect datum_parsed: StepDatum = datum_output

        expect Script(hash) = output_state.address.payment_credential
        let step1_state =
          InvalidInputSpendStep1State {
            tx_hash: frauded_state_queue_tx_hash,
            merkle_proof: tx_to_root_proof_path,
          }
        let step_1_datum =
          StepDatum { fraud_prover: step_datum.fraud_prover, data: step1_state }
        and {
          drop(ct_token_asset_name, n: 28) == block_hash,
          step_1_datum == datum_parsed,
          quantity_of(
            output_state.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ) == 1,
          hash == next_step_spending_validator,
          merkleized_logic,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

pub fn ct_token_burnt(value: Value, policy: PolicyId, assetname: ByteArray) {
  quantity_of(value, policy, assetname) == -1
}

pub fn validate_output_to_fraud_prover(
  output_to_fraud_prover: Output,
  prover_hash: ByteArray,
) {
  when output_to_fraud_prover.address.payment_credential is {
    VerificationKey(hash) -> hash == prover_hash
    _ -> False
  }
}
