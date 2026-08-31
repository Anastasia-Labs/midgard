use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash, blake2b_256, verify_ed25519_signature}
use aiken/interval.{between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, NoDatum, Output, OutputReference, Transaction,
  placeholder, resolve_input,
}
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/da_attestation_types.{
  AddSignatures, ApplyToStateQueue, BurnForRescue, BurnForStateQueue,
  DaAttestationDatum, DaParamsDatum, Init, MintRedeemer,
  RescueStrandedAttestation, SpendRedeemer, attestation_asset_name_prefix,
  attested_signer_bitmap_byte_count, da_params_asset_name,
  empty_attested_signer_bitmap, signature_byte_count,
  signature_witness_byte_count, state_queue_mint_reference_script_asset_name,
  verification_key_byte_count,
}
use midgard/ledger_state.{HeaderHash}
use midgard/state_queue

fn attestation_asset_name(header_hash: HeaderHash) -> AssetName {
  expect bytearray.length(header_hash) == 28
  attestation_asset_name_prefix |> bytearray.concat(header_hash)
}

fn bytearray_slice(bytes: ByteArray, offset: Int, byte_count: Int) -> ByteArray {
  builtin.slice_bytearray(offset, byte_count, bytes)
}

fn signer_bit_mask(signer_index: Int) -> Int {
  let byte_index = signer_index / 8
  let bit_in_byte = signer_index - byte_index * 8
  bytearray.at(#[128, 64, 32, 16, 8, 4, 2, 1], bit_in_byte)
}

fn signer_bit_is_clear(byte: Int, mask: Int) -> Bool {
  let high_bit_window = byte / mask
  let next_window = byte / ( mask * 2 )
  high_bit_window - next_window * 2 == 0
}

fn set_attested_signer(attested: ByteArray, signer_index: Int) -> ByteArray {
  expect bytearray.length(attested) == attested_signer_bitmap_byte_count

  let byte_index = signer_index / 8
  let mask = signer_bit_mask(signer_index)
  let old_byte = bytearray.at(attested, byte_index)
  expect signer_bit_is_clear(old_byte, mask)

  bytearray_slice(attested, 0, byte_index)
    |> bytearray.concat(builtin.cons_bytearray(old_byte + mask, #""))
    |> bytearray.concat(
        bytearray_slice(
          attested,
          byte_index + 1,
          attested_signer_bitmap_byte_count - byte_index - 1,
        ),
      )
}

fn get_da_params(
  reference_inputs: List<Input>,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
) -> DaParamsDatum {
  expect
      Some(Input {
        output: Output {
          address,
          value,
          datum: InlineDatum(datum_data),
          reference_script: None,
        },
        ..
      })
    = reference_inputs |> list.at(da_params_ref_input_index)

  expect Script(params_script_hash) = address.payment_credential
  expect params_script_hash == da_params_policy_id
  expect
    assets.quantity_of(value, da_params_policy_id, da_params_asset_name) == 1

  expect datum: DaParamsDatum = datum_data

  // W-C15 / decision row D-DA4. Until this line the committee hash was taken on
  // the datum's word: every consumer compared its own frozen
  // `committee_signers_hash` against a field that nothing here re-derived, so a
  // params datum carrying a rotated `committee` under the *pre-rotation* hash
  // would satisfy every such comparison while signatures verified against the
  // new keys. Re-deriving it once, here, is what makes `committee` and
  // `committee_signers_hash` a single fact for every caller. This is the sole
  // re-derivation site in this module, and it is paid once per params read.
  expect blake2b_256(datum.committee) == datum.committee_signers_hash

  datum
}

fn get_authenticated_state_queue_policy_id(
  reference_inputs: List<Input>,
  reference_script_auth_policy_id: PolicyId,
  state_queue_mint_ref_script_input_index: Int,
) -> PolicyId {
  expect
      Some(Input {
        output: Output {
          value,
          reference_script: Some(state_queue_policy_id),
          ..
        },
        ..
      })
    = reference_inputs |> list.at(state_queue_mint_ref_script_input_index)

  expect
    assets.has_nft_strict(
      value,
      reference_script_auth_policy_id,
      state_queue_mint_reference_script_asset_name,
    )

  state_queue_policy_id
}

fn output_at_index(outputs: List<Output>, output_index: Int) -> Output {
  outputs |> list.expect_at(output_index)
}

fn expect_script_hash(address: Address) -> PolicyId {
  expect Script(script_hash) = address.payment_credential
  script_hash
}

fn expect_script_hash_from_output(output: Output) -> PolicyId {
  let Output { address, .. } = output
  expect_script_hash(address)
}

fn validate_init_output(
  output: Output,
  own_policy_id: PolicyId,
  params: DaParamsDatum,
  availability_parameters: availability.ParametersV1,
  expected_header_hash: HeaderHash,
) -> AssetName {
  expect
      Output {
        address,
        value,
        datum: InlineDatum(datum_data),
        reference_script: None,
      }
    = output
  expect expect_script_hash(address) == own_policy_id

  expect datum: DaAttestationDatum = datum_data
  expect datum.header_hash == expected_header_hash
  expect datum.availability_commitment.header_hash == expected_header_hash
  expect
    availability.commitment_is_canonical_v1(
      datum.availability_commitment,
      availability_parameters,
    )
  expect datum.da_threshold == params.da_threshold
  expect datum.committee_signers_hash == params.committee_signers_hash
  expect datum.rescue_beneficiary.payment_credential != Script(own_policy_id)
  expect datum.attested_signers == empty_attested_signer_bitmap
  expect datum.attestation_count == 0

  let asset_name = attestation_asset_name(datum.header_hash)
  expect
    value == (
      assets.from_lovelace(availability_parameters.da_bond_lovelace)
        |> assets.add(own_policy_id, asset_name, 1)
    )

  asset_name
}

fn verify_indexed_signatures(
  signatures: ByteArray,
  message: ByteArray,
  committee_signers: ByteArray,
  input_attested: ByteArray,
  min_signer_index: Int,
  signature_cursor: Int,
  signatures_len: Int,
) -> ByteArray {
  if signature_cursor == signatures_len {
    input_attested
  } else {
    let signer_index = bytearray.at(signatures, signature_cursor)
    expect signer_index >= min_signer_index
    expect
      signer_index < bytearray.length(committee_signers) / verification_key_byte_count

    let verification_key = bytearray_slice(
      committee_signers,
      signer_index * verification_key_byte_count,
      verification_key_byte_count,
    )
    let signature = bytearray_slice(
      signatures,
      signature_cursor + 1,
      signature_byte_count,
    )
    expect verify_ed25519_signature(verification_key, message, signature)
    let attested_signers = set_attested_signer(input_attested, signer_index)

    verify_indexed_signatures(
      signatures,
      message,
      committee_signers,
      attested_signers,
      signer_index + 1,
      signature_cursor + signature_witness_byte_count,
      signatures_len,
    )
  }
}

/// Returns the attestation input's datum together with its value. The value is
/// what the rescue path refunds; `has_nft_strict` below is what makes it
/// unambiguous, since it admits exactly one token of this policy in the input.
fn get_attestation_input_datum(
  inputs: List<Input>,
  input_index: Int,
  own_policy_id: PolicyId,
) -> (DaAttestationDatum, Value) {
  expect
      Some(Input {
        output: Output {
          address,
          value,
          datum: InlineDatum(datum_data),
          reference_script: None,
        },
        ..
      })
    = inputs |> list.at(input_index)

  expect expect_script_hash(address) == own_policy_id
  expect datum: DaAttestationDatum = datum_data
  let attestation_asset = attestation_asset_name(datum.header_hash)
  expect assets.has_nft_strict(value, own_policy_id, attestation_asset)
  (datum, value)
}

/// The single burn of `attestation_asset` this transaction is allowed to make.
/// Requiring the policy's whole token map to be exactly one pair at quantity
/// `-1` is what the rescue path's duplicate and cross-attestation rejections
/// rest on: two attestations cannot be retired under one authorization, because
/// two burns of the same asset name collapse into a quantity of `-2` and two
/// burns of different names produce a second pair.
fn expect_sole_burn(
  tx: Transaction,
  own_policy_id: PolicyId,
  asset: AssetName,
) -> Bool {
  expect [Pair(minted_asset_name, minted_quantity)] = tx.mint
    |> assets.tokens(own_policy_id)
    |> dict.to_pairs
  expect minted_asset_name == asset
  expect minted_quantity == -1
  True
}

/// The rescue's value binding. The attestation's entire value, less the burnt
/// DAAT, must land on the single output the redeemer names.
fn validate_rescue_refund(
  outputs: List<Output>,
  refund_output_index: Int,
  attestation_value: Value,
  own_policy_id: PolicyId,
  attestation_asset: AssetName,
  rescue_beneficiary: Address,
) -> Bool {
  let Output { address: refund_address, value: refund_value, .. } = outputs
    |> output_at_index(refund_output_index)

  // Refunding back into this script would re-strand the ADA rather than
  // release it: every spend path requires the UTxO to carry its DAAT, and the
  // DAAT is being burnt here, so such an output could never be spent again.
  expect refund_address == rescue_beneficiary
  expect refund_address.payment_credential != Script(own_policy_id)

  expect
    refund_value == (
      attestation_value
        |> assets.merge(assets.from_asset(own_policy_id, attestation_asset, -1))
    )

  True
}

/// Resolves the mint redeemer that a burn-side spend redeemer defers to, and
/// requires it to name *this* very input. `expect_rescue` selects which mint
/// constructor is acceptable, so a `BurnForStateQueue` cannot be satisfied by a
/// rescue authorization and a `BurnForRescue` cannot be satisfied by an apply.
fn validate_burn_binding(
  tx: Transaction,
  own_ref: OutputReference,
  mint_redeemer_index: Int,
  expect_rescue: Bool,
) -> Bool {
  let own_policy_id = tx.inputs
    |> resolve_input(own_ref)
    |> expect_script_hash_from_output

  let mint_redeemer_data = tx.redeemers
    |> utils.get_redeemer_at(Mint(own_policy_id), mint_redeemer_index)
  expect mint_redeemer: MintRedeemer = mint_redeemer_data

  let bound_input_index = when mint_redeemer is {
    ApplyToStateQueue { da_attestation_input_index, .. } -> {
      expect !expect_rescue
      da_attestation_input_index
    }
    RescueStrandedAttestation { da_attestation_input_index, .. } -> {
      expect expect_rescue
      da_attestation_input_index
    }
    Init { .. } -> fail @"burn redeemer defers to a non-burning mint redeemer"
  }

  expect Some(Input { output_reference, .. }) = tx.inputs
    |> list.at(bound_input_index)
  expect output_reference == own_ref

  True
}

pub fn validate_add_signatures(
  datum: Option<DaAttestationDatum>,
  output_index: Int,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
  signatures: ByteArray,
  own_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  expect Some(input_datum) = datum

  expect
      Output {
        address: input_address,
        value: input_value,
        datum: InlineDatum(input_datum_data),
        reference_script: None,
      }
    = tx.inputs |> resolve_input(own_ref)
  let expected_input_datum_data: Data = input_datum
  expect input_datum_data == expected_input_datum_data

  let own_policy_id = expect_script_hash(input_address)
  let attestation_asset = attestation_asset_name(input_datum.header_hash)
  expect assets.has_nft_strict(input_value, own_policy_id, attestation_asset)

  expect
      Output {
        address: output_address,
        value: output_value,
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      }
    = tx.outputs |> output_at_index(output_index)
  expect output_address == input_address
  expect output_value == input_value

  expect output_datum: DaAttestationDatum = output_datum_data
  expect output_datum.header_hash == input_datum.header_hash
  expect
    output_datum.availability_commitment == input_datum.availability_commitment
  expect output_datum.da_threshold == input_datum.da_threshold
  expect
    output_datum.committee_signers_hash == input_datum.committee_signers_hash
  expect output_datum.rescue_beneficiary == input_datum.rescue_beneficiary

  let params = get_da_params(
    tx.reference_inputs,
    da_params_policy_id,
    da_params_ref_input_index,
  )
  expect params.committee_signers_hash == input_datum.committee_signers_hash

  let signatures_len = bytearray.length(signatures)
  expect signatures_len > 0
  expect signatures_len % signature_witness_byte_count == 0

  let attested_signers = verify_indexed_signatures(
    signatures,
    availability.attestation_message_v1(input_datum.availability_commitment),
    params.committee,
    input_datum.attested_signers,
    0,
    0,
    signatures_len,
  )
  let attestation_count = builtin.count_set_bits(attested_signers)
  expect output_datum.attested_signers == attested_signers
  expect output_datum.attestation_count == attestation_count
  expect attestation_count > input_datum.attestation_count

  True
}

validator da_attestation(
  da_params_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
  availability_policy_id: PolicyId,
  availability_parameters: availability.ParametersV1,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, tx: Transaction) {
    when redeemer is {
      Init {
        output_index,
        da_params_ref_input_index,
        state_queue_ref_input_index,
        state_queue_mint_ref_script_input_index,
      } -> {
        let params = get_da_params(
          tx.reference_inputs,
          da_params_policy_id,
          da_params_ref_input_index,
        )
        let state_queue_policy_id = get_authenticated_state_queue_policy_id(
          tx.reference_inputs,
          reference_script_auth_policy_id,
          state_queue_mint_ref_script_input_index,
        )

        let expected_header_hash = state_queue.get_state_queue_node(
          tx.reference_inputs,
          state_queue_policy_id,
          state_queue_ref_input_index,
          fn(state_queue_node, header_hash) {
            expect
              state_queue_node.da_attestation == state_queue.no_da_attestation
            header_hash
          },
        )

        let attestation_asset = tx.outputs
          |> output_at_index(output_index)
          |> validate_init_output(
              own_policy_id,
              params,
              availability_parameters,
              expected_header_hash,
            )

        expect [Pair(minted_asset_name, minted_quantity)] = tx.mint
          |> assets.tokens(own_policy_id)
          |> dict.to_pairs
        expect minted_asset_name == attestation_asset
        expect minted_quantity == 1

        True
      }
      ApplyToStateQueue {
        da_attestation_input_index,
        da_params_ref_input_index,
        state_queue_input_index,
        state_queue_output_index,
        state_queue_mint_ref_script_input_index,
        availability_mint_redeemer_index,
      } -> {
        let (attestation_datum, _attestation_value) = get_attestation_input_datum(
          tx.inputs,
          da_attestation_input_index,
          own_policy_id,
        )

        // Decision row D-DA4. This branch used to decide quorum entirely from
        // the attestation datum's own frozen `da_threshold`, never reading the
        // DA params at all, so committee rotation was not retroactive: a quorum
        // gathered under a rotated-out committee stayed applicable forever.
        // Reading the *current* governed params and requiring the frozen pair
        // to still equal them is what retires that quorum.
        //
        // Equality of the committee hash, not merely `count >= governed
        // threshold`: what is stale about such a quorum is *whose* signatures
        // it holds, and a threshold that happens to be satisfied must not
        // launder a rotated-out membership.
        let params = get_da_params(
          tx.reference_inputs,
          da_params_policy_id,
          da_params_ref_input_index,
        )
        expect
          attestation_datum.committee_signers_hash == params.committee_signers_hash
        expect attestation_datum.da_threshold == params.da_threshold

        expect
          attestation_datum.attestation_count >= attestation_datum.da_threshold

        let attestation_asset = attestation_asset_name(
          attestation_datum.header_hash,
        )
        expect expect_sole_burn(tx, own_policy_id, attestation_asset)

        expect Some(Input { output_reference: attestation_input_outref, .. }) = tx.inputs
          |> list.at(da_attestation_input_index)
        let da_bond_asset_name = availability.da_bond_asset_name_v1(
          attestation_input_outref,
        )
        let availability_mint_redeemer_data = tx.redeemers
          |> utils.get_redeemer_at(
              Mint(availability_policy_id),
              availability_mint_redeemer_index,
            )
        expect
            availability.MintBondFromAttestation {
              da_attestation_input_index: availability_attestation_input_index,
              state_queue_input_index: availability_state_queue_input_index,
              state_queue_output_index: availability_state_queue_output_index,
              ..
            }: availability.MintRedeemerV1
          = availability_mint_redeemer_data
        expect
          availability_attestation_input_index == da_attestation_input_index
        expect availability_state_queue_input_index == state_queue_input_index
        expect availability_state_queue_output_index == state_queue_output_index

        let state_queue_policy_id = get_authenticated_state_queue_policy_id(
          tx.reference_inputs,
          reference_script_auth_policy_id,
          state_queue_mint_ref_script_input_index,
        )

        state_queue.validate_da_attestation_attachment(
          tx.inputs,
          tx.outputs,
          state_queue_policy_id,
          state_queue_input_index,
          state_queue_output_index,
          attestation_datum.header_hash,
          da_bond_asset_name,
          tx.validity_range,
        )
      }
      RescueStrandedAttestation {
        da_attestation_input_index,
        da_params_ref_input_index,
        refund_output_index,
      } -> {
        let (attestation_datum, attestation_value) = get_attestation_input_datum(
          tx.inputs,
          da_attestation_input_index,
          own_policy_id,
        )
        let params = get_da_params(
          tx.reference_inputs,
          da_params_policy_id,
          da_params_ref_input_index,
        )

        // Decision row D-DA5 clause (c), and deliberately the whole of the
        // authorization: no deadline, no quorum of its own, no new governed
        // value to configure or get wrong. The state condition *is* the proof
        // of strandedness.
        //
        // It is the exact complement of the pair `ApplyToStateQueue` requires
        // above, and that is load-bearing rather than tidy. An attestation
        // freezes *both* governed values at Init, and apply demands both still
        // match; so an attestation is unrecoverable precisely when either has
        // moved. Inverting only the committee hash would leave a second, silent
        // strand: governance may change `da_threshold` over an unchanged
        // committee — the DA-params governor explicitly permits it — and such
        // an attestation could then never apply (the threshold no longer
        // matches) and never be rescued (the committee hash still matches),
        // while `AddSignatures` kept accepting signatures that could never
        // amount to anything. Its ADA would be locked for good, which is the
        // exact failure D-DA5 clause (c) exists to rule out.
        //
        // Widening to the complement cannot take value from an attestation
        // still in flight: whenever this condition holds, the apply gate above
        // is unsatisfiable no matter how many further signatures are gathered.
        // Rescuable and appliable are complements, never both.
        expect or {
            attestation_datum.committee_signers_hash != params.committee_signers_hash,
            attestation_datum.da_threshold != params.da_threshold,
          }

        let attestation_asset = attestation_asset_name(
          attestation_datum.header_hash,
        )
        expect expect_sole_burn(tx, own_policy_id, attestation_asset)

        validate_rescue_refund(
          tx.outputs,
          refund_output_index,
          attestation_value,
          own_policy_id,
          attestation_asset,
          attestation_datum.rescue_beneficiary,
        )
      }
    }
  }

  spend(
    datum: Option<DaAttestationDatum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      AddSignatures { output_index, da_params_ref_input_index, signatures } ->
        validate_add_signatures(
          datum,
          output_index,
          da_params_policy_id,
          da_params_ref_input_index,
          signatures,
          own_ref,
          tx,
        )
      BurnForStateQueue { mint_redeemer_index } ->
        validate_burn_binding(tx, own_ref, mint_redeemer_index, False)
      BurnForRescue { mint_redeemer_index } ->
        validate_burn_binding(tx, own_ref, mint_redeemer_index, True)
    }
  }

  else(_) {
    fail
  }
}

// midgard:test-fixtures-below
//
// Everything below this line is fixture and test code. The committee-hash
// re-derivation claim ("re-derived exactly once per params read") is scoped to
// the code *above* it, so fixtures — which legitimately call `blake2b_256` to
// build params — sit outside that count.

// ---------------------------------------------------------------------------
// Fixtures — Q62 (decision row D-DA4, non-retroactive committee rotation) and
// Q63 acceptance clause (c) (decision row D-DA5, partial-attestation rescue).
//
// Every transaction below is built from one base builder and mutated in
// exactly one field per negative, so a rejection can only be attributed to the
// bound it names. The one deliberate exception is documented at its test.
// ---------------------------------------------------------------------------

const test_da_policy: PolicyId =
  #"da11da11da11da11da11da11da11da11da11da11da11da11da11da11"

const test_availability_policy: PolicyId =
  #"ab11ab11ab11ab11ab11ab11ab11ab11ab11ab11ab11ab11ab11ab11"

const test_da_params_policy: PolicyId =
  #"cd22cd22cd22cd22cd22cd22cd22cd22cd22cd22cd22cd22cd22cd22"

const test_ref_script_auth_policy: PolicyId =
  #"ef33ef33ef33ef33ef33ef33ef33ef33ef33ef33ef33ef33ef33ef33"

const test_refund_key_hash: VerificationKeyHash =
  #"12341234123412341234123412341234123412341234123412341234"

const test_parent_header_hash: HeaderHash =
  #"aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"

fn test_availability_parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 4_096,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
    da_bond_lovelace: test_attestation_lovelace,
    challenger_bond_lovelace: test_attestation_lovelace,
    max_open_fee_lovelace: 1,
    max_publication_fee_lovelace: 1,
    max_settlement_fee_lovelace: 1,
    max_close_fee_lovelace: 1,
    max_timeout_fee_lovelace: 1,
  }
}

fn test_availability_commitment() -> availability.CommitmentV1 {
  availability.CommitmentV1 {
    version: availability.commitment_version_v1,
    deployment_identity: test_parent_header_hash,
    header_hash: test_header_hash(),
    payload_byte_length: 1,
    response_geometry: test_availability_parameters().response_geometry,
    tranche_descriptors: [
      availability.TrancheDescriptorV1 {
        tranche_index: 0,
        start_offset: 0,
        byte_length: 1,
        chunk_count: 1,
        chunk_commitment: #"acacacacacacacacacacacacacacacacacacacacacacacacacacacacacac",
        terminal_accumulator: #"abababababababababababababababababababababababababababababababab",
      },
    ],
    bond_owner: test_refund_key_hash,
  }
}

const test_attestation_ref: OutputReference =
  OutputReference {
    transaction_id: #"1111111111111111111111111111111111111111111111111111111111111111",
    output_index: 0,
  }

const test_second_attestation_ref: OutputReference =
  OutputReference {
    transaction_id: #"2222222222222222222222222222222222222222222222222222222222222222",
    output_index: 0,
  }

const test_params_ref: OutputReference =
  OutputReference {
    transaction_id: #"3333333333333333333333333333333333333333333333333333333333333333",
    output_index: 0,
  }

const test_state_queue_ref: OutputReference =
  OutputReference {
    transaction_id: #"4444444444444444444444444444444444444444444444444444444444444444",
    output_index: 0,
  }

const test_ref_script_ref: OutputReference =
  OutputReference {
    transaction_id: #"5555555555555555555555555555555555555555555555555555555555555555",
    output_index: 0,
  }

const test_state_queue_policy: PolicyId =
  #"5b115b115b115b115b115b115b115b115b115b115b115b115b115b11"

const test_attestation_lovelace: Int = 2_000_000

const test_params_lovelace: Int = 2_000_000

fn repeat_byte(byte: Int, count: Int, accumulated: ByteArray) -> ByteArray {
  if count == 0 {
    accumulated
  } else {
    repeat_byte(byte, count - 1, bytearray.push(accumulated, byte))
  }
}

fn test_signer_key(byte: Int) -> ByteArray {
  repeat_byte(byte, verification_key_byte_count, #"")
}

/// A two-member committee, packed as the on-chain format requires. The two
/// committees below are disjoint, so rotating from one to the other changes
/// `blake2b_256(committee)` and nothing else about the fixture.
fn test_committee(first_byte: Int, second_byte: Int) -> ByteArray {
  test_signer_key(first_byte) |> bytearray.concat(test_signer_key(second_byte))
}

fn test_committee_a() -> ByteArray {
  test_committee(0xa1, 0xa2)
}

fn test_committee_b() -> ByteArray {
  test_committee(0xb1, 0xb2)
}

fn test_owner_hash(byte: Int) -> VerificationKeyHash {
  repeat_byte(byte, 28, #"")
}

/// Params whose `committee_signers_hash` is the honest re-derivation, which is
/// what `get_da_params` now requires of every params read.
fn test_da_params(committee: ByteArray, da_threshold: Int) -> DaParamsDatum {
  DaParamsDatum {
    committee,
    committee_signers_hash: blake2b_256(committee),
    da_threshold,
    owners: [test_owner_hash(0x1), test_owner_hash(0x2)],
    update_threshold: 2,
  }
}

/// Params carrying an arbitrary `committee_signers_hash`, so a fixture can
/// state a hash that is *not* `blake2b_256(committee)`. Nothing but a forged
/// params datum can do this on-chain, which is exactly what the re-derivation
/// in `get_da_params` exists to refuse.
fn test_da_params_with_hash(
  committee: ByteArray,
  committee_signers_hash: ByteArray,
  da_threshold: Int,
) -> DaParamsDatum {
  DaParamsDatum {
    committee,
    committee_signers_hash,
    da_threshold,
    owners: [test_owner_hash(0x1), test_owner_hash(0x2)],
    update_threshold: 2,
  }
}

/// MSB-first bitmap with the first `set_bits` committee indexes attested.
fn test_attested_bitmap(set_bits: Int) -> ByteArray {
  let first_byte = bytearray.at(
    #[0, 128, 192, 224, 240, 248, 252, 254, 255],
    set_bits,
  )
  bytearray.push(
    repeat_byte(0, attested_signer_bitmap_byte_count - 1, #""),
    first_byte,
  )
}

fn test_attestation_datum(
  committee_signers_hash: ByteArray,
  da_threshold: Int,
  attestation_count: Int,
) -> DaAttestationDatum {
  DaAttestationDatum {
    header_hash: test_header_hash(),
    availability_commitment: test_availability_commitment(),
    da_threshold,
    committee_signers_hash,
    rescue_beneficiary: address.from_verification_key(test_refund_key_hash),
    attested_signers: test_attested_bitmap(attestation_count),
    attestation_count,
  }
}

fn test_attestation_asset() -> AssetName {
  attestation_asset_name(test_header_hash())
}

fn test_attestation_value() -> Value {
  assets.from_lovelace(test_attestation_lovelace)
    |> assets.add(test_da_policy, test_attestation_asset(), 1)
}

fn test_attestation_input(
  output_reference: OutputReference,
  datum: DaAttestationDatum,
) -> Input {
  let datum_data: Data = datum
  Input {
    output_reference,
    output: Output {
      address: address.from_script(test_da_policy),
      value: test_attestation_value(),
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
  }
}

fn test_params_reference_input_with_value(
  params: DaParamsDatum,
  value: Value,
) -> Input {
  let params_data: Data = params
  Input {
    output_reference: test_params_ref,
    output: Output {
      address: address.from_script(test_da_params_policy),
      value,
      datum: InlineDatum(params_data),
      reference_script: None,
    },
  }
}

fn test_authenticated_params_value() -> Value {
  assets.from_lovelace(test_params_lovelace)
    |> assets.add(test_da_params_policy, da_params_asset_name, 1)
}

fn test_params_reference_input(params: DaParamsDatum) -> Input {
  test_params_reference_input_with_value(
    params,
    test_authenticated_params_value(),
  )
}

fn test_rescue_mint_redeemer() -> MintRedeemer {
  RescueStrandedAttestation {
    da_attestation_input_index: 0,
    da_params_ref_input_index: 0,
    refund_output_index: 0,
  }
}

/// The rescue transaction under test. `attestation_datum` pins the committee
/// the attestation froze; `params` supplies the governed committee it is
/// compared against; the remaining knobs are the mutation points the negatives
/// below move, one at a time.
fn test_rescue_tx_to(
  attestation_datum: DaAttestationDatum,
  params: DaParamsDatum,
  burn_quantity: Int,
  refund_lovelace: Int,
  with_second_attestation: Bool,
  refund_address: Address,
) -> Transaction {
  let redeemer_data: Data = test_rescue_mint_redeemer()
  let first_input = test_attestation_input(
    test_attestation_ref,
    attestation_datum,
  )
  Transaction {
    ..placeholder,
    inputs: if with_second_attestation {
      [
        first_input,
        test_attestation_input(test_second_attestation_ref, attestation_datum),
      ]
    } else {
      [first_input]
    },
    reference_inputs: [test_params_reference_input(params)],
    outputs: [
      Output {
        address: refund_address,
        value: assets.from_lovelace(refund_lovelace),
        datum: NoDatum,
        reference_script: None,
      },
    ],
    mint: assets.from_asset(
      test_da_policy,
      test_attestation_asset(),
      burn_quantity,
    ),
    redeemers: [Pair(Mint(test_da_policy), redeemer_data)],
  }
}

fn test_rescue_tx(
  attestation_datum: DaAttestationDatum,
  params: DaParamsDatum,
  burn_quantity: Int,
  refund_lovelace: Int,
  with_second_attestation: Bool,
) -> Transaction {
  test_rescue_tx_to(
    attestation_datum,
    params,
    burn_quantity,
    refund_lovelace,
    with_second_attestation,
    attestation_datum.rescue_beneficiary,
  )
}

/// The fixture's stranded attestation: it froze committee A, gathered one of
/// the two signatures it needs, and governance has since rotated to committee
/// B. It can never take another signature, so it can never be applied.
fn test_stranded_attestation() -> DaAttestationDatum {
  test_attestation_datum(blake2b_256(test_committee_a()), 2, 1)
}

fn test_rotated_params() -> DaParamsDatum {
  test_da_params(test_committee_b(), 2)
}

fn test_rescue_mint(tx: Transaction) -> Bool {
  da_attestation.mint(
    test_da_params_policy,
    test_ref_script_auth_policy,
    test_availability_policy,
    test_availability_parameters(),
    test_rescue_mint_redeemer(),
    test_da_policy,
    tx,
  )
}

/// Runs the spend handler with an arbitrary burn-side redeemer, so the two
/// burn constructors can be exercised against either mint transaction.
fn test_apply_burn_spend(tx: Transaction, redeemer: SpendRedeemer) -> Bool {
  da_attestation.spend(
    test_da_params_policy,
    test_ref_script_auth_policy,
    test_availability_policy,
    test_availability_parameters(),
    None,
    redeemer,
    test_attestation_ref,
    tx,
  )
}

fn test_rescue_spend(tx: Transaction, own_ref: OutputReference) -> Bool {
  da_attestation.spend(
    test_da_params_policy,
    test_ref_script_auth_policy,
    test_availability_policy,
    test_availability_parameters(),
    None,
    BurnForRescue { mint_redeemer_index: 0 },
    own_ref,
    tx,
  )
}

/// The rescue transaction the negatives are all mutations of.
fn test_rescue_base_tx() -> Transaction {
  test_rescue_tx(
    test_stranded_attestation(),
    test_rotated_params(),
    -1,
    test_attestation_lovelace,
    False,
  )
}

/// The same rescue with a second stranded attestation added as an input — an
/// attacker bundling somebody else's UTxO into their rescue. The mint half is
/// unchanged and still authorises only input 0.
fn test_rescue_drain_tx() -> Transaction {
  test_rescue_tx(
    test_stranded_attestation(),
    test_rotated_params(),
    -1,
    test_attestation_lovelace,
    True,
  )
}

// ## Q63 acceptance clause (c) — partial-attestation rescue path
//
// The control has to carry both handlers, because a rescue transaction is only
// valid if both run: the mint half decides *whether* the attestation is
// stranded and where its value goes, and the spend half binds this particular
// UTxO to that decision. Pinning only the mint would leave the binding — the
// thing the replay rejection below attacks — unmeasured. The third conjunct
// establishes the same binding inside the drain transaction, which is what
// makes that rejection a one-field differential rather than a comparison
// against a different transaction.

/// Rescue control: a sub-threshold attestation pinned to a rotated-out
/// committee is refunded in full and its DAAT burnt. `attestation_count` is 1
/// against a `da_threshold` of 2, so this attestation never reached quorum and
/// no other redeemer could ever consume it.
test da_attestation_rescue_control_rotated_committee_refunds_partial_attestation() {
  and {
    test_rescue_mint(test_rescue_base_tx()),
    test_rescue_spend(test_rescue_base_tx(), test_attestation_ref),
    test_rescue_spend(test_rescue_drain_tx(), test_attestation_ref),
  }
}

/// Rescue theft rejection: the governed committee still equals the one the
/// attestation froze, so the attestation is not stranded — it can still be
/// signed and applied — and its value is not the rescuer's to take. The single
/// mutated field is the params committee, restored to committee A. Without the
/// state condition this path would be a burn-and-take backdoor over every
/// in-flight attestation in the system.
test da_attestation_rescue_rejects_unrotated_committee_attestation() fail {
  test_rescue_mint(
    test_rescue_tx(
      test_stranded_attestation(),
      test_da_params(test_committee_a(), 2),
      -1,
      test_attestation_lovelace,
      False,
    ),
  )
}

/// Rescue duplicate-refund rejection: the single mutated field is the burnt
/// quantity, `-2` rather than `-1`. Because this policy is stateless it cannot
/// prove per-header uniqueness, so two DAAT UTxOs may carry the same asset
/// name; retiring both under one authorization collapses to exactly this
/// quantity, and would refund one attestation's value while destroying the
/// other's claim to its own.
test da_attestation_rescue_rejects_duplicate_burn_quantity() fail {
  test_rescue_mint(
    test_rescue_tx(
      test_stranded_attestation(),
      test_rotated_params(),
      -2,
      test_attestation_lovelace,
      False,
    ),
  )
}

/// Rescue replay rejection: the same drain transaction the control validated at
/// input 0, spent instead at input 1. The single mutated field is `own_ref`.
/// The rescue mint redeemer names input 0, so its authorization cannot be
/// replayed onto the second attestation UTxO bundled into the transaction.
test da_attestation_rescue_rejects_replayed_mint_binding() fail {
  test_rescue_spend(test_rescue_drain_tx(), test_second_attestation_ref)
}

/// Rescue completeness control, and the regression test for the second strand
/// condition D-DA4 introduced. This is the *same* attestation and the *same*
/// governed params as `da_attestation_apply_rejects_governed_threshold_change`:
/// a full quorum whose frozen `da_threshold` no longer matches the governed one
/// over an unchanged committee. Apply refuses it; rescue must therefore accept
/// it, or its ADA is locked for good. The pair is what makes "rescuable and
/// appliable are complements" a measured claim rather than a comment.
///
/// It also covers the quorum-reached case: the rescue arm reads neither
/// `attestation_count` nor `da_threshold`, so a fully attested but stranded
/// attestation is rescuable, and that is correct — apply cannot consume it
/// either.
test da_attestation_rescue_control_threshold_change_refunds_quorum_attestation() {
  and {
    test_rescue_mint(
      test_rescue_tx(
        test_quorum_attestation(),
        test_da_params(test_committee_a(), 3),
        -1,
        test_attestation_lovelace,
        False,
      ),
    ),
    test_rescue_spend(
      test_rescue_tx(
        test_quorum_attestation(),
        test_da_params(test_committee_a(), 3),
        -1,
        test_attestation_lovelace,
        False,
      ),
      test_attestation_ref,
    ),
  }
}

// ## Burn-redeemer cross-binding
//
// `validate_burn_binding` discriminates on which mint constructor a burn-side
// spend redeemer is allowed to defer to. A *full* swap of its two dispositions
// would be caught by the controls above, but a one-sided regression — dropping
// either guard alone — would leave every other test in this module green while
// letting one burn redeemer be authorised by the other's mint. That matters:
// `ApplyToStateQueue` imposes no refund binding at all, so a `BurnForRescue`
// satisfied by an apply redeemer would release an attestation's value with
// nothing constraining where it went. Both directions are therefore pinned
// individually.

/// Cross-binding control: `BurnForStateQueue` against the apply transaction's
/// own mint redeemer is accepted, so the two rejections below are not a helper
/// that refuses every binding.
test da_attestation_burn_control_queue_binding_accepts_apply_redeemer() {
  test_apply_burn_spend(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input(test_da_params(test_committee_a(), 2)),
    ),
    BurnForStateQueue { mint_redeemer_index: 0 },
  )
}

/// Cross-binding rejection: a `BurnForRescue` spend deferring to an
/// `ApplyToStateQueue` mint redeemer. The single mutated field is the spend
/// redeemer's constructor.
test da_attestation_rejects_rescue_burn_bound_to_apply_redeemer() fail {
  test_apply_burn_spend(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input(test_da_params(test_committee_a(), 2)),
    ),
    BurnForRescue { mint_redeemer_index: 0 },
  )
}

/// Cross-binding rejection, the mirror direction: a `BurnForStateQueue` spend
/// deferring to a `RescueStrandedAttestation` mint redeemer. The single mutated
/// field is the spend redeemer's constructor, against the same rescue
/// transaction the control at the top of this family validated.
test da_attestation_rejects_queue_burn_bound_to_rescue_redeemer() fail {
  test_apply_burn_spend(
    test_rescue_base_tx(),
    BurnForStateQueue { mint_redeemer_index: 0 },
  )
}

/// Rescue value-binding rejection: the single mutated field is the refund
/// output's lovelace, one short of the attestation's. The rescue is
/// permissionless in who submits it, so the value binding is what stops a
/// rescuer skimming the deposit rather than returning it.
test da_attestation_rescue_rejects_refund_short_of_attestation_value() fail {
  test_rescue_mint(
    test_rescue_tx(
      test_stranded_attestation(),
      test_rotated_params(),
      -1,
      test_attestation_lovelace - 1,
      False,
    ),
  )
}

/// Rescue beneficiary binding: a permissionless rescuer may submit the burn,
/// but changing only the refund address cannot redirect the stranded value.
test da_attestation_rescue_rejects_redirected_beneficiary() fail {
  test_rescue_mint(
    test_rescue_tx_to(
      test_stranded_attestation(),
      test_rotated_params(),
      -1,
      test_attestation_lovelace,
      False,
      address.from_verification_key(test_owner_hash(0x8)),
    ),
  )
}

// ---------------------------------------------------------------------------
// ## Q62 / decision row D-DA4 — non-retroactive committee rotation
//
// Before this change `ApplyToStateQueue` read no DA params at all: it decided
// quorum from the attestation datum's own frozen `da_threshold` and burnt the
// DAAT. Rotation was therefore not retroactive — a quorum gathered under a
// committee that governance had since replaced stayed applicable forever. The
// control below is the unchanged-committee case that must keep working, and
// the five negatives are the mutation classes that must not.
// ---------------------------------------------------------------------------

fn test_header() -> ledger_state.HeaderV1 {
  let root32 = #"1010101010101010101010101010101010101010101010101010101010101010"
  ledger_state.HeaderV1 {
    prev_utxos_root: root32,
    utxos_root: root32,
    withdrawals_root: root32,
    forced_transactions_root: root32,
    transactions_root: root32,
    deposits_root: root32,
    transition_trace_root: root32,
    event_to_step_root: root32,
    validation_traces_root: root32,
    withdrawal_count: 1,
    forced_transaction_count: 1,
    l2_transaction_count: 1,
    deposit_count: 1,
    total_event_count: 4,
    transition_step_count: 4,
    validation_trace_count: 2,
    start_time: 1_000,
    end_time: 2_000,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: test_parent_header_hash,
    operator_vkey: test_parent_header_hash,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn test_header_hash() -> HeaderHash {
  let header_data: Data = test_header()
  utils.serialise_and_hash_28(header_data)
}

/// A state-queue linked-list node for `test_header_hash()`.
fn test_state_queue_node_output(
  da_attestation: availability.StateQueueStatusV1,
) -> Output {
  let node = state_queue.StateQueueNode {
    header: test_header(),
    da_attestation,
  }
  let element: linked_list.Element<Data, state_queue.StateQueueNode> = linked_list.Element {
    data: linked_list.Node { data: node },
    link: None,
  }
  let element_data: Data = element
  Output {
    address: address.from_script(test_state_queue_policy),
    value: assets.from_lovelace(test_attestation_lovelace)
      |> assets.add(
          test_state_queue_policy,
          state_queue.block_asset_name_prefix
            |> bytearray.concat(test_header_hash()),
          1,
        ),
    datum: InlineDatum(element_data),
    reference_script: None,
  }
}

/// The reference input carrying the state-queue minting script, authenticated
/// by its own reference-script NFT.
fn test_state_queue_ref_script_input() -> Input {
  Input {
    output_reference: test_ref_script_ref,
    output: Output {
      address: address.from_script(test_ref_script_auth_policy),
      value: assets.from_lovelace(test_params_lovelace)
        |> assets.add(
            test_ref_script_auth_policy,
            state_queue_mint_reference_script_asset_name,
            1,
          ),
      datum: NoDatum,
      reference_script: Some(test_state_queue_policy),
    },
  }
}

fn test_apply_mint_redeemer() -> MintRedeemer {
  ApplyToStateQueue {
    da_attestation_input_index: 0,
    da_params_ref_input_index: 0,
    state_queue_input_index: 1,
    state_queue_output_index: 0,
    state_queue_mint_ref_script_input_index: 1,
    availability_mint_redeemer_index: 1,
  }
}

fn test_availability_mint_redeemer() -> availability.MintRedeemerV1 {
  availability.MintBondFromAttestation {
    hub_oracle_ref_input_index: 2,
    da_attestation_input_index: 0,
    da_attestation_mint_redeemer_index: 0,
    bond_output_index: 1,
    state_queue_input_index: 1,
    state_queue_output_index: 0,
  }
}

/// The apply transaction under test. Only the attestation datum and the DA
/// params reference input vary across the family below; everything else — the
/// state-queue node, its attachment, the burn, the reference scripts — is held
/// fixed, so a rejection can only come from the governed-params comparison.
fn test_apply_tx(
  attestation_datum: DaAttestationDatum,
  params_reference_input: Input,
) -> Transaction {
  let redeemer_data: Data = test_apply_mint_redeemer()
  let availability_redeemer_data: Data = test_availability_mint_redeemer()
  let da_bond_asset_name = availability.da_bond_asset_name_v1(
    test_attestation_ref,
  )
  let attestation_deadline = test_header().end_time + state_queue.da_attestation_timeout_v1
  Transaction {
    ..placeholder,
    inputs: [
      test_attestation_input(test_attestation_ref, attestation_datum),
      Input {
        output_reference: test_state_queue_ref,
        output: test_state_queue_node_output(state_queue.no_da_attestation),
      },
    ],
    reference_inputs: [
      params_reference_input,
      test_state_queue_ref_script_input(),
    ],
    outputs: [
      test_state_queue_node_output(availability.Attested { da_bond_asset_name }),
    ],
    mint: assets.from_asset(test_da_policy, test_attestation_asset(), -1)
      |> assets.add(test_availability_policy, da_bond_asset_name, 1),
    redeemers: [
      Pair(Mint(test_da_policy), redeemer_data),
      Pair(Mint(test_availability_policy), availability_redeemer_data),
    ],
    validity_range: between(attestation_deadline - 60_000, attestation_deadline),
  }
}

fn test_apply_mint(tx: Transaction) -> Bool {
  da_attestation.mint(
    test_da_params_policy,
    test_ref_script_auth_policy,
    test_availability_policy,
    test_availability_parameters(),
    test_apply_mint_redeemer(),
    test_da_policy,
    tx,
  )
}

/// The attestation the apply family starts from: it froze committee A at a
/// threshold of 2 and has both signatures, so it is a genuine quorum.
fn test_quorum_attestation() -> DaAttestationDatum {
  test_attestation_datum(blake2b_256(test_committee_a()), 2, 2)
}

/// Apply control: the governed committee and threshold are still the ones the
/// attestation froze, so the quorum applies and the DAAT is burnt. This is the
/// test that establishes the five negatives below are not merely a handler that
/// rejects everything.
test da_attestation_apply_control_unchanged_committee_applies() {
  test_apply_mint(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input(test_da_params(test_committee_a(), 2)),
    ),
  )
}

/// Rotation rejection — the D-DA4 headline. The single mutated field is the
/// governed committee, rotated from A to B with its hash honestly re-derived.
/// The attestation still holds a full quorum under its frozen threshold, which
/// is exactly what used to be sufficient.
test da_attestation_apply_rejects_rotated_committee() fail {
  test_apply_mint(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input(test_da_params(test_committee_b(), 2)),
    ),
  )
}

/// Threshold-change rejection. The single mutated field is the governed
/// `da_threshold`, raised from 2 to 3 over the same committee. Governance
/// tightening the DA bar must retire in-flight quorums gathered under the old
/// one rather than let them apply at the bar they froze.
test da_attestation_apply_rejects_governed_threshold_change() fail {
  test_apply_mint(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input(test_da_params(test_committee_a(), 3)),
    ),
  )
}

/// Forged-committee-hash rejection, and the reason the rotation check alone is
/// not enough. The single mutated field is the params `committee`, rotated to
/// B while `committee_signers_hash` is left at A's. Every equality this
/// validator performs against that field would still pass; only re-deriving
/// `blake2b_256(committee)` inside `get_da_params` refuses it. Without that
/// re-derivation this transaction defeats the rotation rejection above.
test da_attestation_apply_rejects_forged_committee_hash() fail {
  test_apply_mint(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input(
        test_da_params_with_hash(
          test_committee_b(),
          blake2b_256(test_committee_a()),
          2,
        ),
      ),
    ),
  )
}

/// Stale-params-reference rejection. The single mutated field is the params
/// reference input's value, which no longer carries the governance NFT. The
/// datum it presents is byte-for-byte the control's, so this is an attacker
/// replaying an obsolete snapshot of the params to reinstate a rotated-out
/// committee; only the NFT authentication distinguishes it.
test da_attestation_apply_rejects_stale_params_reference_input() fail {
  test_apply_mint(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input_with_value(
        test_da_params(test_committee_a(), 2),
        assets.from_lovelace(test_params_lovelace),
      ),
    ),
  )
}

/// Old-quorum rejection. Declared deliberately as the family's one compound
/// mutation: the committee rotates to B *and* the governed threshold drops to
/// 1, so the frozen count of 2 clears the new bar with room to spare. It exists
/// to kill a specific weaker fix — re-reading the params but comparing
/// `attestation_count >= params.da_threshold` — which every one-field negative
/// above would still pass. What is stale about this quorum is whose signatures
/// it holds, and no satisfied threshold may launder that.
test da_attestation_apply_rejects_old_committee_quorum() fail {
  test_apply_mint(
    test_apply_tx(
      test_quorum_attestation(),
      test_params_reference_input(test_da_params(test_committee_b(), 1)),
    ),
  )
}
