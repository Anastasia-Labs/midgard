use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/common/utils.{
  get_redeemer_at, get_single_asset_from_value_apart_from_ada,
  get_singleton_asset_with_policy, has_signed, verify_hash_32,
}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardTxCompact, MidgardTxId}

pub fn cancel(
  computation_thread_token_policy_id: PolicyId,
  m_datum: Option<ct.StepDatum<a>>,
  input_index: Int,
  own_out_ref: OutputReference,
  mint_redeemer_index: Int,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, redeemers, extra_signatories, .. } = tx
  // 1. Validate the `Cancel` redeemer from the computation thread policy is
  //    invoked (this in turn validates the burning of the specified computation
  //    thread token).
  let mint_redeemer_data =
    get_redeemer_at(
      redeemers: redeemers,
      expected_purpose: Mint(computation_thread_token_policy_id),
      redeemer_index: mint_redeemer_index,
    )
  expect ct.BurnForCancellation {
    burning_token_asset_name: redeemer_burning_asset_name,
  } = mint_redeemer_data

  // 2. Input UTxO must be authentic.
  expect Some(Input {
    output: Output { value: input_value, .. },
    output_reference,
  }) = list.at(inputs, input_index)
  expect output_reference == own_out_ref
  let input_nft_triplet =
    get_single_asset_from_value_apart_from_ada(input_value)
  expect
    input_nft_triplet == (
      computation_thread_token_policy_id,
      redeemer_burning_asset_name,
      1,
    )

  // 3. Transaction must be signed by the fraud prover.
  expect Some(ct.StepDatum { fraud_prover, .. }) = m_datum
  expect fraud_prover |> has_signed(extra_signatories)

  // Done.
  True
}

pub fn continue(
  computation_thread_token_policy_id: PolicyId,
  step_datum: ct.StepDatum<a>,
  input_index: Int,
  output_index: Int,
  own_out_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  validation_fn: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<a>,
    ScriptHash,
    Data,
  ) ->
    Bool,
) -> Bool {
  let
    input_script_hash,
    computation_thread_token_asset_name,
    fraud_prover,
    m_input_state_data,
    output,
  <-
    validate_input_and_provide_output(
      computation_thread_token_policy_id,
      step_datum,
      input_index,
      output_index,
      own_out_ref,
      inputs,
      outputs,
    )

  // 1. Output UTxO must:
  //    - Go to an address which its payment credential is a script.
  //    - Carry the same NFT as the input UTxO, with no other tokens.
  //    - Have an inline datum attached.
  //    - Not have a reference script attached.
  expect Output {
    address: Address { payment_credential: Script(output_script_hash), .. },
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  } = output
  let output_nft_triplet =
    get_single_asset_from_value_apart_from_ada(output_value)
  expect
    output_nft_triplet == (
      computation_thread_token_policy_id,
      computation_thread_token_asset_name,
      1,
    )

  // 2. Fraud prover must not change during the transition.
  expect ct.StepDatum {
    fraud_prover: output_fraud_prover,
    data: Some(output_state_data),
  }: ct.StepDatum<Data> = output_datum_data
  expect output_fraud_prover == fraud_prover

  // 3. Custom validation for each fraud proof must pass.
  validation_fn(
    input_script_hash,
    computation_thread_token_asset_name,
    fraud_prover,
    m_input_state_data,
    output_script_hash,
    output_state_data,
  )
}

pub fn finalize(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  step_datum: ct.StepDatum<a>,
  input_index: Int,
  output_index: Int,
  own_out_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: Value,
  validation_fn: fn(ScriptHash, AssetName, VerificationKeyHash, Option<a>) ->
    Bool,
) -> Bool {
  let
    input_script_hash,
    computation_thread_token_asset_name,
    fraud_prover,
    m_input_state_data,
    output,
  <-
    validate_input_and_provide_output(
      computation_thread_token_policy_id,
      step_datum,
      input_index,
      output_index,
      own_out_ref,
      inputs,
      outputs,
    )

  // 1. Output UTxO must:
  //    - Go to an address which its payment credential is a script.
  //    - Have an inline datum attached.
  //    - Not have a reference script attached.
  expect Output {
    address: Address { payment_credential: Script(output_script_hash), .. },
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  } = output

  // 1. Successful fraud proofs must be reserved at an address which its
  //    payment credential is that of `fraud_proof`'s script hash.
  expect output_script_hash == fraud_proof_token_policy_id

  // 2. Produced UTxO at `fraud_proof` must have only 1 NFT of its own script
  //    hash, with the same asset name as the input NFT's.
  let output_nft_triplet =
    get_single_asset_from_value_apart_from_ada(output_value)
  expect
    output_nft_triplet == (
      fraud_proof_token_policy_id,
      computation_thread_token_asset_name,
      1,
    )

  // 3. Fraud prover must be presreved, with no additional `data`.
  expect ct.StepDatum { fraud_prover: output_fraud_prover, data: None }: ct.StepDatum<
    Data,
  > = output_datum_data
  expect output_fraud_prover == fraud_prover

  // 4. Exactly one `fraud_proof` NFT must be minted, with the same asset name
  //    as the input NFT's. Here we are implicitly assuming this minting also
  //    validates the burning of the computation thread NFT.
  expect Pair(mint_asset_name, 1) =
    mint |> get_singleton_asset_with_policy(fraud_proof_token_policy_id)
  expect mint_asset_name == computation_thread_token_asset_name

  // 5. Custom validation for each fraud proof must pass.
  validation_fn(
    input_script_hash,
    computation_thread_token_asset_name,
    fraud_prover,
    m_input_state_data,
  )
}

pub fn verify_tx_hash(bad_tx_id: MidgardTxId, bad_tx: MidgardTxCompact) -> Bool {
  let bad_tx_data: Data = bad_tx
  verify_hash_32(data: bad_tx_data, data_hash: bad_tx_id)
}

pub fn validate_output_to_fraud_prover(
  output_to_fraud_prover: Output,
  prover_hash: ByteArray,
) -> Bool {
  when output_to_fraud_prover.address.payment_credential is {
    VerificationKey(hash) -> hash == prover_hash
    _ -> False
  }
}

// Internal Helpers

fn validate_input_and_provide_output(
  computation_thread_token_policy_id: PolicyId,
  step_datum: ct.StepDatum<a>,
  input_index: Int,
  output_index: Int,
  own_out_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  validation_fn: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<a>,
    Output,
  ) ->
    Bool,
) -> Bool {
  let
    input,
    output,
  <-
    singular_utxo_indexer.one_to_one(
      input_index,
      output_index,
      own_out_ref,
      inputs,
      outputs,
      // Double satisfaction prevented by validating the output UTxO carries the
      // same computation thread token as the input's.
      True,
    )

  // 1. Input UTxO must carry exactly one computation thread token and no other
  //    tokens.
  expect Input {
    output: Output {
      address: Address { payment_credential: Script(input_script_hash), .. },
      value: input_value,
      ..
    },
    ..
  } = input
  let (input_nft_policy_id, computation_thread_token_asset_name, input_nft_qty) =
    get_single_asset_from_value_apart_from_ada(input_value)
  expect input_nft_policy_id == computation_thread_token_policy_id
  expect input_nft_qty == 1

  let ct.StepDatum { data: m_input_state_data, fraud_prover } = step_datum

  validation_fn(
    input_script_hash,
    computation_thread_token_asset_name,
    fraud_prover,
    m_input_state_data,
    output,
  )
}
