use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{verify_ed25519_signature}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction, resolve_input,
}
use midgard/common/utils
use midgard/da_attestation_types.{
  AddSignatures, ApplyToStateQueue, BurnForStateQueue, DaAttestationDatum,
  DaParamsDatum, Init, MintRedeemer, SpendRedeemer,
  attestation_asset_name_prefix, da_params_asset_name,
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

fn attestation_message(header_hash: HeaderHash) -> ByteArray {
  bytearray.from_string(@"MidgardDAAttestationV1")
    |> bytearray.concat(header_hash)
}

fn bytearray_slice(bytes: ByteArray, offset: Int, byte_count: Int) -> ByteArray {
  builtin.slice_bytearray(offset, byte_count, bytes)
}

fn get_da_params(
  reference_inputs: List<Input>,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
) -> DaParamsDatum {
  expect Some(Input {
    output: Output {
      address,
      value,
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
    ..
  }) = reference_inputs |> list.at(da_params_ref_input_index)

  expect Script(params_script_hash) = address.payment_credential
  expect params_script_hash == da_params_policy_id
  expect
    assets.quantity_of(value, da_params_policy_id, da_params_asset_name) == 1

  expect datum: DaParamsDatum = datum_data
  datum
}

fn get_authenticated_state_queue_policy_id(
  reference_inputs: List<Input>,
  reference_script_auth_policy_id: PolicyId,
  state_queue_mint_ref_script_input_index: Int,
) -> PolicyId {
  expect Some(Input {
    output: Output { value, reference_script: Some(state_queue_policy_id), .. },
    ..
  }) = reference_inputs |> list.at(state_queue_mint_ref_script_input_index)

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
  expected_header_hash: HeaderHash,
) -> AssetName {
  expect Output {
    address,
    value,
    datum: InlineDatum(datum_data),
    reference_script: None,
  } = output
  expect expect_script_hash(address) == own_policy_id

  expect datum: DaAttestationDatum = datum_data
  expect datum.header_hash == expected_header_hash
  expect datum.da_threshold == params.da_threshold
  expect datum.committee_signers_hash == params.committee_signers_hash
  expect datum.attested_signers == empty_attested_signer_bitmap
  expect datum.attestation_count == 0

  let asset_name = attestation_asset_name(datum.header_hash)
  expect assets.has_nft_strict(value, own_policy_id, asset_name)

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
  attested_bit_indexes: List<Int>,
) -> ByteArray {
  if signature_cursor == signatures_len {
    builtin.write_bits(input_attested, attested_bit_indexes, True)
  } else {
    let signer_index = bytearray.at(signatures, signature_cursor)
    expect signer_index >= min_signer_index
    let signer_bit_index = 255 - signer_index

    let verification_key =
      bytearray_slice(
        committee_signers,
        signer_index * verification_key_byte_count,
        verification_key_byte_count,
      )
    let signature =
      bytearray_slice(signatures, signature_cursor + 1, signature_byte_count)
    expect verify_ed25519_signature(verification_key, message, signature)

    verify_indexed_signatures(
      signatures,
      message,
      committee_signers,
      input_attested,
      signer_index + 1,
      signature_cursor + signature_witness_byte_count,
      signatures_len,
      [signer_bit_index, ..attested_bit_indexes],
    )
  }
}

fn get_attestation_input_datum(
  inputs: List<Input>,
  input_index: Int,
  own_policy_id: PolicyId,
) -> DaAttestationDatum {
  expect Some(Input {
    output: Output {
      address,
      value,
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
    ..
  }) = inputs |> list.at(input_index)

  expect expect_script_hash(address) == own_policy_id
  expect datum: DaAttestationDatum = datum_data
  let attestation_asset = attestation_asset_name(datum.header_hash)
  expect assets.has_nft_strict(value, own_policy_id, attestation_asset)
  datum
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

  expect Output {
    address: input_address,
    value: input_value,
    datum: InlineDatum(input_datum_data),
    reference_script: None,
  } = tx.inputs |> resolve_input(own_ref)
  let expected_input_datum_data: Data = input_datum
  expect input_datum_data == expected_input_datum_data

  let own_policy_id = expect_script_hash(input_address)
  let attestation_asset = attestation_asset_name(input_datum.header_hash)
  expect assets.has_nft_strict(input_value, own_policy_id, attestation_asset)

  expect Output {
    address: output_address,
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  } = tx.outputs |> output_at_index(output_index)
  expect output_address == input_address
  expect output_value == input_value

  expect output_datum: DaAttestationDatum = output_datum_data
  expect output_datum.header_hash == input_datum.header_hash
  expect output_datum.da_threshold == input_datum.da_threshold
  expect
    output_datum.committee_signers_hash == input_datum.committee_signers_hash

  let params =
    get_da_params(
      tx.reference_inputs,
      da_params_policy_id,
      da_params_ref_input_index,
    )
  expect params.committee_signers_hash == input_datum.committee_signers_hash

  let signatures_len = bytearray.length(signatures)

  let attested_signers =
    verify_indexed_signatures(
      signatures,
      attestation_message(input_datum.header_hash),
      params.committee,
      input_datum.attested_signers,
      0,
      0,
      signatures_len,
      [],
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
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, tx: Transaction) {
    when redeemer is {
      Init {
        output_index,
        da_params_ref_input_index,
        state_queue_ref_input_index,
        state_queue_mint_ref_script_input_index,
      } -> {
        let params =
          get_da_params(
            tx.reference_inputs,
            da_params_policy_id,
            da_params_ref_input_index,
          )
        let state_queue_policy_id =
          get_authenticated_state_queue_policy_id(
            tx.reference_inputs,
            reference_script_auth_policy_id,
            state_queue_mint_ref_script_input_index,
          )

        let expected_header_hash =
          state_queue.get_state_queue_node(
            tx.reference_inputs,
            state_queue_policy_id,
            state_queue_ref_input_index,
            fn(state_queue_node, header_hash) {
              expect
                state_queue_node.da_attestation == state_queue.no_da_attestation
              header_hash
            },
          )

        let attestation_asset =
          tx.outputs
            |> output_at_index(output_index)
            |> validate_init_output(own_policy_id, params, expected_header_hash)

        expect [Pair(minted_asset_name, minted_quantity)] =
          tx.mint
            |> assets.tokens(own_policy_id)
            |> dict.to_pairs
        expect minted_asset_name == attestation_asset
        expect minted_quantity == 1

        True
      }
      ApplyToStateQueue {
        da_attestation_input_index,
        state_queue_input_index,
        state_queue_output_index,
        state_queue_mint_ref_script_input_index,
      } -> {
        let attestation_datum =
          get_attestation_input_datum(
            tx.inputs,
            da_attestation_input_index,
            own_policy_id,
          )
        expect
          attestation_datum.attestation_count >= attestation_datum.da_threshold

        let attestation_asset =
          attestation_asset_name(attestation_datum.header_hash)
        expect [Pair(minted_asset_name, minted_quantity)] =
          tx.mint
            |> assets.tokens(own_policy_id)
            |> dict.to_pairs
        expect minted_asset_name == attestation_asset
        expect minted_quantity == -1

        let state_queue_policy_id =
          get_authenticated_state_queue_policy_id(
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
          own_policy_id,
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
      BurnForStateQueue { mint_redeemer_index } -> {
        let own_policy_id =
          tx.inputs
            |> resolve_input(own_ref)
            |> expect_script_hash_from_output

        let mint_redeemer_data =
          tx.redeemers
            |> utils.get_redeemer_at(Mint(own_policy_id), mint_redeemer_index)
        expect ApplyToStateQueue { da_attestation_input_index, .. }: MintRedeemer =
          mint_redeemer_data
        expect Some(Input { output_reference, .. }) =
          tx.inputs |> list.at(da_attestation_input_index)
        expect output_reference == own_ref

        True
      }
    }
  }

  else(_) {
    fail
  }
}
