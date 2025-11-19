use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray.{drop}
use aiken_design_patterns/linked_list/unordered as unordered
use aiken_design_patterns/merkelized_validator
use cardano/address.{Script, VerificationKey}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/common/utils.{get_singleton_asset_with_policy} as get_singleton_asset_with_policy
use midgard/common/utils.{get_authentic_input_with_policy_at} as get_authentic_input_with_policy_at
use midgard/computation_thread.{StepDatum, Success}
use midgard/fraud_proof/common/types.{
  ExclusionProofInputTx, ExclusionProofUtxo, FraudProofDatum,
  MembershipProofInputs,
}
use midgard/ledger_state.{Header}

// ------------ ABSTRACTIONS FOR ALL FRAUD PROOF STEPS ------------
// Each step in a computation thread must in the continue redeemer:
// Parse computation state from datum (previous step's output datum)
// Receive arguments from current steps redeemer
// Advance the computation by step, producing a new state
// Suspend computation by serializing new state into datum for next step and pass it on

// -------------- REDEEMER: CONTINUE -----------------
// (1) Reference a state queue block with a block hash matching the last 28 bytes of the computation thread token name
// (2) Delegate compute to the merkleized validator to verify the inclusion of tx1 in the tx_root
// (3) The fraud prover field must match between the output state and the input datum
// (4) Verify the state transition from the previous state to the new state
// (5) The computation thread token must be sent to the next step's spending validator
// (6) Transaction must not mint or burn any other tokens

// Relevant to (1): tx_root provided by fraud prover in redeemer matches block_hash from the state queue block from the reference input
pub fn get_tx_root_validate_block_hash(
  tx: Transaction,
  state_queue_node_ref_input_index: Int,
  ct_token_asset_name: ByteArray,
  state_queue_policy: ByteArray,
) -> ByteArray {
  // Reference input with block_hash 
  let state_queue_datum_data =
    get_confirmed_datum(
      tx,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )
  expect state_queue_node_datum: unordered.NodeDatum = state_queue_datum_data
  expect parsed_state_queue_datum: Header = state_queue_node_datum.data
  expect unordered.Key { key: block_hash } = state_queue_node_datum.key
  let extracted_tx_root = parsed_state_queue_datum.transactions_root

  // Get CT token and check match with the block hash
  expect drop(ct_token_asset_name, n: 4) == block_hash

  extracted_tx_root
}

pub fn get_confirmed_datum(
  tx: Transaction,
  state_queue_policy: ByteArray,
  state_queue_node_ref_input_index: Int,
) {
  let state_queue_node_input =
    get_authentic_input_with_policy_at(
      tx.reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )

  // Extract tx_root via the state queue block hash
  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum
  state_queue_datum_data
}

pub fn get_block_datum(tx: Transaction, state_queue_node_ref_input_index: Int) {
  expect Some(state_queue_node_input) =
    list.at(tx.reference_inputs, state_queue_node_ref_input_index)

  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum

  state_queue_datum_data
}

// Relevant to (1): tx_root provided by fraud prover in redeemer matches block_hash from the state queue block from the reference input
pub fn get_prev_utxo_root(
  tx: Transaction,
  state_queue_node_ref_input_index: Int,
) -> ByteArray {
  // Reference input with block_hash 
  expect Some(state_queue_node_input) =
    list.at(tx.reference_inputs, state_queue_node_ref_input_index)

  // Extract tx_root via the state queue block hash
  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum
  expect state_queue_node_datum: unordered.NodeDatum = state_queue_datum_data
  expect parsed_state_queue_datum: Header = state_queue_node_datum.data
  let extracted_prev_utxo_root = parsed_state_queue_datum.prev_utxos_root

  extracted_prev_utxo_root
}

// tx_root, merkle root of transactions in the state queue block
// tx_hash, hash of the transaction that is being proven to be in tx_root
// proof_path: minimum set of hashes needed to reconstruct the path from the tx to the root

// Relevant to (2)
pub fn validate_membership_proof(
  tx: Transaction,
  plutarch_midgard_merkle_validator: ScriptHash,
  proof_inputs: MembershipProofInputs,
) -> Bool {
  expect
    merkelized_validator.delegated_validation(
      function_input: MembershipProofInputs {
        tx_root: proof_inputs.tx_root,
        tx_hash: proof_inputs.tx_hash,
        tx_info_hash: proof_inputs.tx_info_hash,
        tx_to_root_proof_path: proof_inputs.tx_to_root_proof_path,
      },
      staking_validator: plutarch_midgard_merkle_validator,
      input_data_coercer: fn(withdraw_redeemer: Data) {
        expect coerced: MembershipProofInputs = withdraw_redeemer
        coerced
      },
      redeemers: tx.redeemers,
    )
  True
}

// Relevant to (2)
pub fn validate_exclusion_proof(
  tx: Transaction,
  plutarch_midgard_merkle_validator: ScriptHash,
  proof_input: ExclusionProofInputTx,
) -> Bool {
  expect
    merkelized_validator.delegated_validation(
      function_input: ExclusionProofInputTx {
        tx_root: proof_input.tx_root,
        invalid_input_tx_hash: proof_input.invalid_input_tx_hash,
        invalid_input_info_hash: proof_input.invalid_input_info_hash,
        input_to_root_proof_path: proof_input.input_to_root_proof_path,
      },
      staking_validator: plutarch_midgard_merkle_validator,
      input_data_coercer: fn(withdraw_redeemer: Data) {
        expect coerced: ExclusionProofInputTx = withdraw_redeemer
        coerced
      },
      redeemers: tx.redeemers,
    )
  True
}

// Relevant to (2)
pub fn validate_exclusion_utxo_proof(
  tx: Transaction,
  plutarch_midgard_merkle_validator: ScriptHash,
  proof_input: ExclusionProofUtxo,
) -> Bool {
  expect
    merkelized_validator.delegated_validation(
      function_input: ExclusionProofUtxo {
        utxo_root: proof_input.utxo_root,
        invalid_utxo: proof_input.invalid_utxo,
        invalid_utxo_info_hash: proof_input.invalid_utxo_info_hash,
        utxo_to_root_proof_path: proof_input.utxo_to_root_proof_path,
      },
      staking_validator: plutarch_midgard_merkle_validator,
      input_data_coercer: fn(withdraw_redeemer: Data) {
        expect coerced: ExclusionProofUtxo = withdraw_redeemer
        coerced
      },
      redeemers: tx.redeemers,
    )
  True
}

// Common validation for any step's output state (except final step)
// Relevant to (3), (4), (5), (6)
pub fn validate_step_output(
  tx: Transaction,
  index_output: Int,
  next_validator_hash: ScriptHash,
  ct_token_policy_id: PolicyId,
  ct_token_asset_name: ByteArray,
  step_datum: StepDatum,
  expected_output_datum: StepDatum,
) -> Bool {
  expect Some(output_to_next_step) = list.at(tx.outputs, index_output)
  // (3) Validate fraud prover field matches
  expect InlineDatum(output_datum) = output_to_next_step.datum
  expect parsed_output_datum: StepDatum = output_datum
  expect parsed_output_datum.fraud_prover == step_datum.fraud_prover

  // (4) Validate state transition
  expect parsed_output_datum.data == expected_output_datum.data

  // (5) Validate output goes to next validator
  expect Script(hash) = output_to_next_step.address.payment_credential
  expect hash == next_validator_hash

  // (6) Validate CT token forwarding and no other minting/burning
  expect
    Pair(ct_token_asset_name, 1) == get_singleton_asset_with_policy(
      output_to_next_step.value,
      ct_token_policy_id,
    )

  True
}

// -------------- REDEEMER: CONTINUE (FINAL STEP) -----------------
// Validations for all final step output states

// (1) Mint the fraud token
// (2) Burn computation thread token with Success redeemer
// (3) Output datum type must be StepDatum(Void)
// (4) The fraud_prover field must match between output datum and input datum
// (5) Transaction must not mint or burn any other tokens
pub fn validate_final_step_output(
  tx: Transaction,
  index_output: Int,
  fraud_proof_token_policy: PolicyId,
  ct_token_asset_name: ByteArray,
  step_datum: StepDatum,
  computation_thread_redeemer_index: Int,
) -> Bool {
  // (3, 4) Validate output state is Void and fraud prover matches
  expect Some(output) = list.at(tx.outputs, index_output)
  expect InlineDatum(output_datum) = output.datum
  expect parsed_output_datum: FraudProofDatum = output_datum
  let expected_output_datum =
    FraudProofDatum { fraud_prover: step_datum.fraud_prover }
  expect expected_output_datum == parsed_output_datum

  // (1, 5) Validate fraud proof token minting and no other minting
  expect
    Pair(ct_token_asset_name, 1) == get_singleton_asset_with_policy(
      tx.mint,
      fraud_proof_token_policy,
    )

  // (2) Validate computation thread token burn with Success redeemer
  expect Some(computation_thread_redeemer_pair) =
    list.at(tx.redeemers, computation_thread_redeemer_index)
  expect Success { token_to_burn_asset_name } =
    computation_thread_redeemer_pair.2nd
  expect token_to_burn_asset_name == ct_token_asset_name

  True
}

// ------------ ABSTRACTED FUNCTIONS FOR ALL CANCEL REDEEMERS ------------
// -------------- REDEEMER: CANCEL -----------------
// (1) Burn the computation thread token 
// (2) Return the ADA from the double spend computation thread utxo to the fraud prover pub-key defined in the input datum
// (3) Transaction must not mint or burn any other tokens

// Umbrella Cancel Redeemer function
pub fn handle_cancel(
  own_input: Input,
  index_output: Int,
  step_datum: StepDatum,
  ct_token_policy_id: PolicyId,
  tx: Transaction,
) -> Bool {
  expect validate_ct_token_burn(own_input, ct_token_policy_id, tx)
  expect
    validate_ada_return_to_fraud_prover(
      own_input,
      index_output,
      step_datum.fraud_prover,
      tx,
    )
  True
}

// Relevant Cancel Redeemer Conditions: (1), (3)
pub fn validate_ct_token_burn(
  own_input: Input,
  ct_token_policy_id: PolicyId,
  tx: Transaction,
) -> Bool {
  // (1.2) Get computation thread token asset name in output
  expect Pair(ct_token_asset_name, 1) =
    get_singleton_asset_with_policy(own_input.output.value, ct_token_policy_id)
  // (1.3) Burn the computation thread token, (3) Transaction must not mint or burn any other tokens
  expect
    Pair(ct_token_asset_name, -1) == get_singleton_asset_with_policy(
      tx.mint,
      ct_token_policy_id,
    )
  True
}

// Relevant Cancel Redeemer Conditions: (2)
pub fn validate_ada_return_to_fraud_prover(
  own_input: Input,
  index_output: Int,
  fraud_prover: ByteArray,
  tx: Transaction,
) -> Bool {
  // (2.1) Get the lovelace amount from our own validator's input (ADA inside the input of the fraud proof thread)
  let lovelace = lovelace_of(own_input.output.value)
  // (2.2) Verify an output is going to fraud prover address
  expect Some(output_to_fraud_prover) = list.at(tx.outputs, index_output)
  // (2.3) Validate output address matches fraud prover address
  expect validate_output_to_fraud_prover(output_to_fraud_prover, fraud_prover)
  // (2.4) Lovelace amount from our own validator's input to fraud prover
  expect lovelace == lovelace_of(output_to_fraud_prover.value)
  True
}

// -------------- HELPER FUNCTIONS -----------------

// Get computation thread token from input
// This is used by all redeemers to identify the computation thread token passed in from the previous step
pub fn get_singleton_ct_token(
  own_input: Input,
  ct_token_policy_id: PolicyId,
) -> ByteArray {
  expect Pair(ct_token_asset_name, 1) =
    get_singleton_asset_with_policy(own_input.output.value, ct_token_policy_id)
  ct_token_asset_name
}

// Validate output goes to fraud prover address
pub fn validate_output_to_fraud_prover(
  output_to_fraud_prover: Output,
  prover_hash: ByteArray,
) -> Bool {
  when output_to_fraud_prover.address.payment_credential is {
    VerificationKey(hash) -> hash == prover_hash
    _ -> False
  }
}
