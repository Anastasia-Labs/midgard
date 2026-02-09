use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, ScriptPurpose,
  Transaction,
}
use midgard/common/constants.{fraud_category_id_byte_count}
use midgard/common/types.{Proof}
use midgard/common/utils.{
  get_redeemer_at, get_single_asset_from_value_apart_from_ada, has_signed,
  plutarch_phas, verify_hash_32,
}
use midgard/computation_thread as ct
use midgard/fraud_proof
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  Header, MidgardTxBodyCompact, MidgardTxCompact, MidgardTxId,
}
use midgard/state_queue

pub type TxInclusionArgs {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  tx_id: MidgardTxId,
  tx: MidgardTxCompact,
  tx_body: MidgardTxBodyCompact,
  tx_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub fn pass_tx_to_next_step(
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  datum: Option<ct.StepDatum<input_state>>,
  tx_inclusion_args: TxInclusionArgs,
  own_out_ref: OutputReference,
  l1_tx: Transaction,
  validation: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<input_state>,
    ScriptHash,
    Data<output_state>,
    Header,
    MidgardTxId,
    MidgardTxCompact,
    MidgardTxBodyCompact,
  ) ->
    Bool,
) -> Bool {
  let TxInclusionArgs {
    input_index,
    output_index,
    hub_ref_input_index,
    state_queue_node_ref_input_index,
    tx_id,
    tx,
    tx_body,
    tx_membership_proof,
    inclusion_proof_script_withdraw_redeemer_index,
  } = tx_inclusion_args

  let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = l1_tx

  expect Some(step_datum) = datum

  // 1. Specified input must be authentic and properly reproduced.
  let
    own_script_hash,
    computation_thread_token_asset_name,
    fraud_prover,
    m_input_state_data,
    output_script_hash,
    output_state_data,
  <-
    continue(
      computation_thread_token_policy_id,
      step_datum,
      input_index,
      output_index,
      own_out_ref,
      inputs,
      outputs,
    )

  // 2. The provided transaction must be valid, and must be included in
  //    the specified state queue node.
  let header =
    verify_tx_in_state_queue_node_and_get_header(
      tx_id,
      tx,
      tx_body,
      tx_membership_proof,
      computation_thread_token_asset_name,
      hub_oracle,
      hub_ref_input_index,
      state_queue_node_ref_input_index,
      inclusion_proof_script_withdraw_redeemer_index,
      reference_inputs,
      redeemers,
    )

  // 3. Custom validation must pass.
  expect
    validation(
      own_script_hash,
      computation_thread_token_asset_name,
      fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data,
      header,
      tx_id,
      tx,
      tx_body,
    )

  // Done.
  True
}

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

/// Generic helper for validating requirements specific for progressing a fraud
/// proof. It provides:
/// - Script hash of the spending/input UTxO (i.e. fraud proof's step script).
/// - Asset name of the computation thread token carried by the input UTxO.
/// - Verification key hash of the fraud prover.
/// - The `data` field of the spending `StepDatum` (which is of type
///   `Option<a>`).
/// - Output's script hash (i.e. the payment part of its address).
/// - Output's state data (of type `Data`, i.e. it is validated `data` field of
///   `StepDatum` is `Some<Data>`).
///
/// The reason input state data is provided with `Option` is to allow this
/// function to also be used in the first step of the fraud proofs (since all
/// fraud proofs start with `None` in the `data` field).
///
/// On the other hand, output state data is provided as `Data` directly to
/// disallow this function to be used for final steps. [`finalize`](#finalize)
/// should be used instead.
pub fn continue(
  computation_thread_token_policy_id: PolicyId,
  step_datum: ct.StepDatum<input_state>,
  input_index: Int,
  output_index: Int,
  own_out_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  validation_fn: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<input_state>,
    ScriptHash,
    Data<output_state>,
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
  //    - Carry the same NFT as the input UTxO, with no other tokens
  //      (consequently preventing double satisfaction).
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
  fraud_proof_address: Address,
  step_datum: ct.StepDatum<input_state>,
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  own_out_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  validation_fn: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<input_state>,
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
  //    - Have an inline datum attached.
  //    - Not have a reference script attached.
  expect Output {
    address: output_address,
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  } = output

  // 2. Successful fraud proofs must be reserved at fraud proof's spending
  //    script which is an always fails script.
  expect output_address == fraud_proof_address

  // 3. Produced UTxO at `fraud_proof` must have only 1 NFT of `fraud_proof`
  //    policy, with the same asset name as the input NFT's.
  let output_nft_triplet =
    get_single_asset_from_value_apart_from_ada(output_value)
  expect
    output_nft_triplet == (
      fraud_proof_token_policy_id,
      computation_thread_token_asset_name,
      1,
    )

  // 4. Fraud prover must be presreved, with no additional `data`.
  expect ct.StepDatum { fraud_prover: output_fraud_prover, data: None }: ct.StepDatum<
    Data,
  > = output_datum_data
  expect output_fraud_prover == fraud_prover

  // 5. Fraud proof's minting script must be invoked (which also validates the
  //    burning of the computation thread token).
  let fraud_proof_mint_redeemer_data =
    get_redeemer_at(
      redeemers: redeemers,
      expected_purpose: Mint(fraud_proof_token_policy_id),
      redeemer_index: fraud_proof_mint_redeemer_index,
    )
  expect fraud_proof.MintRedeemer {
    computation_thread_token_asset_name: fraud_proof_redeemer_burning_asset_name,
    ..
  } = fraud_proof_mint_redeemer_data

  // 6. The asset name specified in fraud proof's redeemer must match the token
  //    included in the spending input.
  expect
    fraud_proof_redeemer_burning_asset_name == computation_thread_token_asset_name

  // 7. Custom validation for each fraud proof must pass.
  validation_fn(
    input_script_hash,
    computation_thread_token_asset_name,
    fraud_prover,
    m_input_state_data,
  )
}

pub fn verify_tx_and_its_body(
  tx_id: MidgardTxId,
  tx: MidgardTxCompact,
  tx_body: MidgardTxBodyCompact,
) -> Bool {
  let tx_data: Data = tx
  and {
    verify_hash_32(data: tx_data, data_hash: tx_id),
    verify_hash_32(data: tx_body, data_hash: tx.body),
  }
}

pub fn verify_tx_in_state_queue_node_and_get_header(
  tx_id: MidgardTxId,
  tx: MidgardTxCompact,
  tx_body: MidgardTxBodyCompact,
  tx_membership_proof: Proof,
  computation_thread_token_asset_name: AssetName,
  hub_oracle: PolicyId,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Header {
  // 1. Verify authenticity of the provided tx and its body against its ID, and
  //    body hash.
  expect verify_tx_and_its_body(tx_id, tx, tx_body)

  // 2. Hub oracle must be provided as a reference input.
  let hub.Datum { state_queue: state_queue_policy, .. } =
    hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

  let
    state_queue_datum,
    state_queue_node_key,
  <-
    state_queue.get_block_datum(
      reference_inputs,
      state_queue_policy,
      state_queue_node_ref_input_index,
    )

  // 3. The asset name of the computation thread token must match the state
  //    queue node key (after dropping the fraud category ID prefix).
  expect
    state_queue_node_key == bytearray.drop(
      computation_thread_token_asset_name,
      fraud_category_id_byte_count,
    )

  // 4. Specified transaction must be proven to be included in the referenced
  //    state queue node.
  let tx_data: Data = tx
  expect
    plutarch_phas(
      merkle_root: state_queue_datum.transactions_root,
      key: builtin.b_data(tx_id),
      value: tx_data,
      membership_proof: tx_membership_proof,
      redeemers: redeemers,
      withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
    )

  // Done. Return the block header.
  state_queue_datum
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

/// Validates authenticity of the input at `input_index` (i.e. that it comes
/// from a script, and that it carries only a computation thread token), and
/// provides:
/// - Script hash of the input's address.
/// - Asset name of the computation thread.
/// - Verification key hash of the fraud prover.
/// - The `data` field of the spending `StepDatum` (which is of type
///   `Option<a>`). `Option` so that first steps can also be covered.
/// - The output at `output_index` (no validation is performed on it).
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
      // same computation thread token as the input's. This is _NOT_ carried out
      // in this function though, hence this helper itself not being exposed.
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
