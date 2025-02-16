use aiken/builtin
use aiken/cbor
// imports 
// midgard-merkle is used as a library to verify merkle inclusion proofs 
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray.{drop}
use aiken_design_patterns/linked_list/unordered.{Key}
use aiken_design_patterns/merkelized_validator
use cardano/address.{Address, VerificationKey}
use cardano/assets.{PolicyId, Value, lovelace_of, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/common/datum.{StateQueueDatum}
use midgard/common/utils.{get_singleton_asset_with_policy} as get_singleton_asset_with_policy
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/non_existing_input/main.{
  Cancel, Continue, MembershipProofInputs, SingleStepRedeemer,
}
use midgard/state_queue.{Datum as NodeDatum}

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

validator step(
  ct_token_policy_id: PolicyId,
  _next_step_spending_validator: ScriptHash,
  stake_validator: ScriptHash,
) {
  spend(
    maybe_datum: Option<StepDatum>,
    redeemer: SingleStepRedeemer,
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

        //*****************/
        //let's find the amount of ADA inside the input of the fraud proof to be cancelled
        let lovelace = lovelace_of(own_input.output.value)
        //let's find the output that goes to the fraud prover
        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)

        //*****************/
        //2)verify that the output is going to fraud prover address
        expect
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          )

        //*****************/
        //3)send the ada back to the prover
        expect lovelace_of(output_to_fraud_prover.value) == lovelace
        True
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
        bad_state_queue_node_ref_input_index,
        bad_state_queue_tx_hash,
        bad_state_queue_tx_info_hash,
        tx_to_root_proof_path,
        input_utxos,
        fraudolent_input,
        tx_body,
        tx,
      } -> {
        // (1) Reference input with block_hash (tx_root) which carries the last 28 bytes of ct token name
        // --------------------------------------------------------------------
        expect Some(bad_state_queue_node_input) =
          list.at(self.reference_inputs, bad_state_queue_node_ref_input_index)
        expect InlineDatum(bad_state_queue_node_datum) =
          bad_state_queue_node_input.output.datum

        expect parsed_node_datum: NodeDatum = bad_state_queue_node_datum
        expect parsed_bad_state_queue_node_datum: StateQueueDatum =
          parsed_node_datum.data

        //let's extract BLOCKHASH and TX_ROOT from the block in state queue
        expect Key(block_hash) = parsed_node_datum.key
        let extracted_tx_root =
          parsed_bad_state_queue_node_datum.data.utxos_root
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )

        //Let's find the assetname
        expect Pair(ct_token_asset_name, 1) =
          get_singleton_asset_with_policy(
            own_input.output.value,
            ct_token_policy_id,
          )
        //1) the token name removed the first 4 bytes is exactly the block_hash we are refering to
        expect drop(ct_token_asset_name, n: 4) == block_hash

        //Let's find if the tx is part of the block in state queue
        // is the tx part of the block? 
        let merkleized_logic =
          merkelized_validator.delegated_validation(
            function_input: MembershipProofInputs {
              root: extracted_tx_root,
              tx_hash: bad_state_queue_tx_hash,
              tx_info_hash: bad_state_queue_tx_info_hash,
              proof_path: tx_to_root_proof_path,
            },
            staking_validator: stake_validator,
            input_data_coercer: fn(withdraw_redeemer: Data) {
              expect coerced: MembershipProofInputs = withdraw_redeemer
              coerced
            },
            redeemers: self.redeemers,
          )
        expect merkleized_logic

        //The list of inputs is part of the transactio
        expect bytes_input_list: ByteArray = input_utxos
        expect blake2b_256(bytes_input_list) == tx_body.spend_inputs

        // The passed input list contains the bad list
        expect list.has(builtin.un_list_data(input_utxos), fraudolent_input)

        // Is the midgard body part of the Midgard tx that we are passing?
        expect blake2b_256(cbor.serialise(tx_body)) == tx.body

        // Is the midgard tx passing hashed the same one we got in the previous step
        expect blake2b_256(cbor.serialise(tx)) == bad_state_queue_tx_hash

        //TODO missing the part that we prove that this input is not part of any previous utxo root,  PROVING NOT INCLUSION
        // Since we are doing everything in one step we want to burn the thread token
        expect
          Pair(ct_token_asset_name, -1) == get_singleton_asset_with_policy(
            self.mint,
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
