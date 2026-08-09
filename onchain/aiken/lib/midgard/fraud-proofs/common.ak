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
use env
use midgard/common/types.{Proof}
use midgard/common/utils.{
  get_redeemer_at, get_single_asset_from_value_apart_from_ada, has_signed,
  plutarch_pexcludes_raw, plutarch_phas_raw,
}
use midgard/computation_thread as ct
use midgard/fraud_proof
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/fraud_proofs/native_tx/types.{VerifiedMidgardNativeTxCompact} as native_tx_types
use midgard/hub_oracle as hub
use midgard/ledger_state.{HeaderV1}
use midgard/state_queue
use midgard/transition_trace

pub type NativeTxInclusionArgs {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
  // Raw transactions MPF root the prover supplies. It is authenticated against
  // the header's counted `transactions_root` before being used for the
  // membership check (see `verify_native_tx_in_state_queue_node`).
  transactions_phas_root: ByteArray,
  tx_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

/// The same evidence as [`NativeTxInclusionArgs`](#NativeTxInclusionArgs) with
/// the membership proof taken out of the transaction: the proof's steps live in
/// published chunk UTxOs and only their order reaches the redeemer, as indices
/// into `tx.reference_inputs` (issue #545).
///
/// Field-for-field identical to `NativeTxInclusionArgs` up to the proof: the
/// two carriages authenticate the very same commitment and differ only in where
/// the proof's bytes are.
pub type PublishedChunkInclusionArgs {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
  transactions_phas_root: ByteArray,
  ordered_chunk_reference_input_indices: List<Int>,
}

/// How a step's transactions-root membership opening reaches L1.
///
/// The prover chooses. Both arms end in the same verified opening against the
/// same header-authenticated root, so the choice is a cost decision (one
/// transaction versus two) and not a trust one. Small proofs belong on the
/// direct route; a proof an adversary has grinded past the envelope belongs on
/// the published-chunk route, which is the point of the type.
pub type NativeTxInclusionCarriage {
  /// The proof travels in this transaction's redeemers, verified by the
  /// merkelized `phas` validator. ~276 signed bytes per proof level.
  RedeemerCarriedInclusion(NativeTxInclusionArgs)
  /// The proof was published beforehand; this transaction names its chunks.
  /// One integer and one reference input per chunk.
  PublishedChunkInclusion(PublishedChunkInclusionArgs)
}

/// How a step's non-membership opening reaches L1. The membership twin's
/// reasoning applies unchanged.
pub type NonMembershipCarriage {
  /// The proof travels in this transaction's redeemers, verified by the
  /// merkelized `pexcludes` validator.
  RedeemerCarriedNonMembership {
    non_membership_proof: Proof,
    non_membership_proof_script_redeemer_index: Int,
  }
  /// The proof was published beforehand; this transaction names its chunks.
  PublishedChunkNonMembership(PublishedProofCarriage)
}

/// The raw transactions MPF root a carriage names, whichever arm it is.
///
/// Safe for a step to thread forward only *after* the carriage has been
/// verified, because that is where the root is authenticated against the
/// challenged header's counted `transactions_root`.
pub fn carriage_transactions_phas_root(
  carriage: NativeTxInclusionCarriage,
) -> ByteArray {
  when carriage is {
    RedeemerCarriedInclusion(args) -> args.transactions_phas_root
    PublishedChunkInclusion(args) -> args.transactions_phas_root
  }
}

/// Verifies the absence of `key_bytes` under an already-authenticated
/// `merkle_root`, by whichever route the prover chose.
pub fn verify_non_membership_carried(
  carriage: NonMembershipCarriage,
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  when carriage is {
    RedeemerCarriedNonMembership {
      non_membership_proof,
      non_membership_proof_script_redeemer_index,
    } ->
      plutarch_pexcludes_raw(
        merkle_root: merkle_root,
        key_bytes: key_bytes,
        non_membership_proof: non_membership_proof,
        redeemers: redeemers,
        withdraw_redeemer_index: non_membership_proof_script_redeemer_index,
      )
    PublishedChunkNonMembership(published) ->
      // Delegated to the merkelized verifier, which reads the published chunks
      // from the very same `reference_inputs` this step sees. Keeping the walk
      // out of the step's own script is what stops the chunked route from
      // enlarging the direct one (issue #545).
      chunked_inclusion_v1.delegated_chunk_non_membership(
        env.mpf_chunked_verify_validator_hash,
        redeemers,
        published,
        merkle_root,
        key_bytes,
      )
  }
}

/// Native v1 transaction inclusion helper for fault proofs whose evidence is
/// committed by the node's native transaction root rather than PlutusData
/// `MidgardTxCompact` values, taking the membership opening by whichever
/// carriage the prover chose.
///
/// This is the single seam through which all four foundational families reach
/// published-chunk carriage; the per-family semantics that follow the opening
/// are untouched by it.
pub fn pass_native_tx_to_next_step_carried(
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  datum: Option<ct.StepDatum<input_state>>,
  carriage: NativeTxInclusionCarriage,
  own_out_ref: OutputReference,
  l1_tx: Transaction,
  validation: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<input_state>,
    ScriptHash,
    Data<output_state>,
    HeaderV1,
    ByteArray,
    VerifiedMidgardNativeTxCompact,
  ) ->
    Bool,
) -> Bool {
  when carriage is {
    RedeemerCarriedInclusion(tx_inclusion_args) ->
      pass_native_tx_to_next_step(
        computation_thread_token_policy_id,
        hub_oracle,
        datum,
        tx_inclusion_args,
        own_out_ref,
        l1_tx,
        validation,
      )
    PublishedChunkInclusion(published_args) -> {
      let
          PublishedChunkInclusionArgs {
            input_index,
            output_index,
            hub_ref_input_index,
            state_queue_node_ref_input_index,
            native_tx_id,
            native_tx_compact_cbor,
            transactions_phas_root,
            ordered_chunk_reference_input_indices,
          }
        = published_args

      let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = l1_tx

      expect Some(step_datum) = datum

      let
          own_script_hash,
          computation_thread_token_asset_name,
          fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data
        <- continue(
        computation_thread_token_policy_id,
        step_datum,
        input_index,
        output_index,
        own_out_ref,
        inputs,
        outputs,
      )

      let _verified_tx_id, header, native_tx_view <- verify_native_tx_in_state_queue_node_with(
        native_tx_id,
        native_tx_compact_cbor,
        transactions_phas_root,
        computation_thread_token_asset_name,
        hub_oracle,
        hub_ref_input_index,
        state_queue_node_ref_input_index,
        reference_inputs,
        fn(authenticated_root, key_bytes, value_bytes) {
          chunked_inclusion_v1.delegated_chunk_membership(
            env.mpf_chunked_verify_validator_hash,
            redeemers,
            PublishedProofCarriage { ordered_chunk_reference_input_indices },
            authenticated_root,
            key_bytes,
            value_bytes,
          )
        },
      )

      expect
        validation(
          own_script_hash,
          computation_thread_token_asset_name,
          fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
          header,
          native_tx_id,
          native_tx_view,
        )

      True
    }
  }
}

/// Native v1 transaction inclusion helper for fault proofs whose evidence is
/// committed by the node's native transaction root rather than PlutusData
/// `MidgardTxCompact` values.
pub fn pass_native_tx_to_next_step(
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  datum: Option<ct.StepDatum<input_state>>,
  tx_inclusion_args: NativeTxInclusionArgs,
  own_out_ref: OutputReference,
  l1_tx: Transaction,
  validation: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<input_state>,
    ScriptHash,
    Data<output_state>,
    HeaderV1,
    ByteArray,
    VerifiedMidgardNativeTxCompact,
  ) ->
    Bool,
) -> Bool {
  let
      NativeTxInclusionArgs {
        input_index,
        output_index,
        hub_ref_input_index,
        state_queue_node_ref_input_index,
        native_tx_id,
        native_tx_compact_cbor,
        transactions_phas_root,
        tx_membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      }
    = tx_inclusion_args

  let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = l1_tx

  expect Some(step_datum) = datum

  let
      own_script_hash,
      computation_thread_token_asset_name,
      fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
  )

  let _verified_tx_id, header, native_tx_view <- verify_native_tx_in_state_queue_node(
    native_tx_id,
    native_tx_compact_cbor,
    transactions_phas_root,
    tx_membership_proof,
    computation_thread_token_asset_name,
    hub_oracle,
    hub_ref_input_index,
    state_queue_node_ref_input_index,
    inclusion_proof_script_withdraw_redeemer_index,
    reference_inputs,
    redeemers,
  )

  expect
    validation(
      own_script_hash,
      computation_thread_token_asset_name,
      fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data,
      header,
      native_tx_id,
      native_tx_view,
    )

  True
}

/// First-step helper for fault proofs whose evidence is a **raw committed
/// leaf** of the block's `transactions_root` rather than a decoded native
/// transaction.
///
/// It is the codec-free twin of
/// [`pass_native_tx_to_next_step`](#pass_native_tx_to_next_step): identical
/// computation-thread, hub, state-queue, counted-root and MPF-membership
/// binding, but the leaf key and leaf value are handed to `validation`
/// verbatim. Only the `da-hash-preimage` family may use it — every other
/// family must keep the codec precondition.
pub fn pass_committed_transactions_leaf_to_next_step(
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  datum: Option<ct.StepDatum<input_state>>,
  tx_inclusion_args: NativeTxInclusionArgs,
  own_out_ref: OutputReference,
  l1_tx: Transaction,
  validation: fn(
    ScriptHash,
    AssetName,
    VerificationKeyHash,
    Option<input_state>,
    ScriptHash,
    Data<output_state>,
    HeaderV1,
    ByteArray,
    ByteArray,
  ) ->
    Bool,
) -> Bool {
  let
      NativeTxInclusionArgs {
        input_index,
        output_index,
        hub_ref_input_index,
        state_queue_node_ref_input_index,
        native_tx_id: committed_tx_id,
        native_tx_compact_cbor: committed_leaf_value,
        transactions_phas_root,
        tx_membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      }
    = tx_inclusion_args

  let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = l1_tx

  expect Some(step_datum) = datum

  let
      own_script_hash,
      computation_thread_token_asset_name,
      fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
  )

  let header <- verify_committed_transactions_leaf_in_state_queue_node(
    committed_tx_id,
    committed_leaf_value,
    transactions_phas_root,
    tx_membership_proof,
    computation_thread_token_asset_name,
    hub_oracle,
    hub_ref_input_index,
    state_queue_node_ref_input_index,
    inclusion_proof_script_withdraw_redeemer_index,
    reference_inputs,
    redeemers,
  )

  expect
    validation(
      own_script_hash,
      computation_thread_token_asset_name,
      fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data,
      header,
      committed_tx_id,
      committed_leaf_value,
    )

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
  let mint_redeemer_data = get_redeemer_at(
    redeemers: redeemers,
    expected_purpose: Mint(computation_thread_token_policy_id),
    redeemer_index: mint_redeemer_index,
  )
  expect
      ct.BurnForCancellation {
        burning_token_asset_name: redeemer_burning_asset_name,
      }
    = mint_redeemer_data

  // 2. Input UTxO must be authentic.
  expect
      Some(Input { output: Output { value: input_value, .. }, output_reference })
    = list.at(inputs, input_index)
  expect output_reference == own_out_ref
  let input_nft_triplet = get_single_asset_from_value_apart_from_ada(
    input_value,
  )
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
      output
    <- validate_input_and_provide_output(
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
  expect
      Output {
        address: Address { payment_credential: Script(output_script_hash), .. },
        value: output_value,
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      }
    = output
  let output_nft_triplet = get_single_asset_from_value_apart_from_ada(
    output_value,
  )
  expect
    output_nft_triplet == (
      computation_thread_token_policy_id,
      computation_thread_token_asset_name,
      1,
    )

  // 2. Fraud prover must not change during the transition.
  expect
      ct.StepDatum {
        fraud_prover: output_fraud_prover,
        data: Some(output_state_data),
      }: ct.StepDatum<Data>
    = output_datum_data
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
      output
    <- validate_input_and_provide_output(
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
  expect
      Output {
        address: output_address,
        value: output_value,
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      }
    = output

  // 2. Successful fraud proofs must be reserved at fraud proof's spending
  //    script which is an always fails script.
  expect output_address == fraud_proof_address

  // 3. Produced UTxO at `fraud_proof` must have only 1 NFT of `fraud_proof`
  //    policy, with the same asset name as the input NFT's.
  let output_nft_triplet = get_single_asset_from_value_apart_from_ada(
    output_value,
  )
  expect
    output_nft_triplet == (
      fraud_proof_token_policy_id,
      computation_thread_token_asset_name,
      1,
    )

  // 4. Fraud prover must be preserved, with no additional `data`.
  let expected_output_datum_data: Data = fraud_proof.Datum { fraud_prover }
  expect output_datum_data == expected_output_datum_data

  // 5. Fraud proof's minting script must be invoked (which also validates the
  //    burning of the computation thread token).
  let fraud_proof_mint_redeemer_data = get_redeemer_at(
    redeemers: redeemers,
    expected_purpose: Mint(fraud_proof_token_policy_id),
    redeemer_index: fraud_proof_mint_redeemer_index,
  )
  expect
      fraud_proof.MintRedeemer {
        computation_thread_token_asset_name: fraud_proof_redeemer_burning_asset_name,
        ..
      }
    = fraud_proof_mint_redeemer_data

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

/// Authenticates one raw `(key, value)` leaf of a committed block's
/// `transactions_root` **without** requiring the leaf value to be a
/// well-formed canonical native-V1 transaction and **without** requiring the
/// leaf key to be that value's transaction id.
///
/// This is the evidence primitive of the `da-hash-preimage` family
/// (`GOAL_SPEC.md` Q44): the very fault it proves is a committed leaf whose
/// key is not the hash-preimage commitment of its value, so the codec check
/// performed by [`verify_native_tx_in_state_queue_node`](#verify_native_tx_in_state_queue_node)
/// must not be a precondition here. Everything else — hub identity,
/// state-queue node identity, counted-root authentication and MPF membership —
/// is identical, so the returned leaf is exactly as trustworthy as the header
/// commitment.
pub fn verify_committed_transactions_leaf_in_state_queue_node(
  committed_tx_id: ByteArray,
  committed_leaf_value: ByteArray,
  transactions_phas_root: ByteArray,
  tx_membership_proof: Proof,
  computation_thread_token_asset_name: AssetName,
  hub_oracle: ScriptHash,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  return: fn(HeaderV1) -> result,
) -> result {
  let hub.Datum { state_queue: state_queue_policy, .. } = hub.get_datum(
    reference_inputs,
    hub_oracle,
    hub_ref_input_index,
  )

  let state_queue_datum, state_queue_node_key <- state_queue.get_block_datum_v1(
    reference_inputs,
    state_queue_policy,
    state_queue_node_ref_input_index,
  )

  expect
    state_queue_node_key == bytearray.drop(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    )

  // Authenticate the prover-supplied raw phas root against the header's counted
  // `transactions_root`: only the genuine raw root re-hashes to the committed
  // value under this block's `l2_transaction_count`. After this check the raw
  // root is as trustworthy as the header commitment.
  expect
    transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      transactions_phas_root,
      state_queue_datum.l2_transaction_count,
    ) == state_queue_datum.transactions_root

  // MPF membership check runs against the authenticated raw root.
  expect
    plutarch_phas_raw(
      merkle_root: transactions_phas_root,
      key_bytes: committed_tx_id,
      value_bytes: committed_leaf_value,
      membership_proof: tx_membership_proof,
      redeemers: redeemers,
      withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
    )

  return(state_queue_datum)
}

pub fn verify_native_tx_in_state_queue_node(
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
  transactions_phas_root: ByteArray,
  tx_membership_proof: Proof,
  computation_thread_token_asset_name: AssetName,
  hub_oracle: ScriptHash,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  inclusion_proof_script_withdraw_redeemer_index: Int,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  return: fn(ByteArray, HeaderV1, VerifiedMidgardNativeTxCompact) -> result,
) -> result {
  verify_native_tx_in_state_queue_node_with(
    native_tx_id,
    native_tx_compact_cbor,
    transactions_phas_root,
    computation_thread_token_asset_name,
    hub_oracle,
    hub_ref_input_index,
    state_queue_node_ref_input_index,
    reference_inputs,
    fn(authenticated_root, key_bytes, value_bytes) {
      plutarch_phas_raw(
        merkle_root: authenticated_root,
        key_bytes: key_bytes,
        value_bytes: value_bytes,
        membership_proof: tx_membership_proof,
        redeemers: redeemers,
        withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
      )
    },
    return,
  )
}

/// The carriage-agnostic core of
/// [`verify_native_tx_in_state_queue_node`](#verify_native_tx_in_state_queue_node).
///
/// Everything that makes the evidence trustworthy lives here — the canonical
/// codec precondition, hub identity, state-queue node identity and the
/// counted-root authentication that turns a prover-supplied raw root into the
/// header's own commitment — and the opening itself is delegated to
/// `membership_check`, which receives `(authenticated root, key, value)`. A
/// carriage therefore cannot choose what it proves, only where the proof's
/// bytes travelled.
pub fn verify_native_tx_in_state_queue_node_with(
  native_tx_id: ByteArray,
  native_tx_compact_cbor: ByteArray,
  transactions_phas_root: ByteArray,
  computation_thread_token_asset_name: AssetName,
  hub_oracle: ScriptHash,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  reference_inputs: List<Input>,
  membership_check: fn(ByteArray, ByteArray, ByteArray) -> Bool,
  return: fn(ByteArray, HeaderV1, VerifiedMidgardNativeTxCompact) -> result,
) -> result {
  let native_tx_view = verify_native_tx_compact_cbor_v1(
    native_tx_id,
    native_tx_compact_cbor,
  )

  let hub.Datum { state_queue: state_queue_policy, .. } = hub.get_datum(
    reference_inputs,
    hub_oracle,
    hub_ref_input_index,
  )

  let state_queue_datum, state_queue_node_key <- state_queue.get_block_datum_v1(
    reference_inputs,
    state_queue_policy,
    state_queue_node_ref_input_index,
  )

  expect
    state_queue_node_key == bytearray.drop(
      computation_thread_token_asset_name,
      fraud_proof_catalogue.id_byte_count,
    )

  // Authenticate the prover-supplied raw phas root against the header's counted
  // `transactions_root`: only the genuine raw root re-hashes to the committed
  // value under this block's `l2_transaction_count`. After this check the raw
  // root is as trustworthy as the header commitment.
  expect
    transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      transactions_phas_root,
      state_queue_datum.l2_transaction_count,
    ) == state_queue_datum.transactions_root

  // MPF membership check runs against the authenticated raw root.
  expect
    membership_check(
      transactions_phas_root,
      native_tx_id,
      native_tx_compact_cbor,
    )

  return(native_tx_id, state_queue_datum, native_tx_view)
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

// ## Internal Helpers

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
  let input, output <- singular_utxo_indexer.one_to_one(
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
  expect
      Input {
        output: Output {
          address: Address { payment_credential: Script(input_script_hash), .. },
          value: input_value,
          ..
        },
        ..
      }
    = input
  let (input_nft_policy_id, computation_thread_token_asset_name, input_nft_qty)
    = get_single_asset_from_value_apart_from_ada(input_value)
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
