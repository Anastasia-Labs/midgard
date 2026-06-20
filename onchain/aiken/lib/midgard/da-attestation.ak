use aiken/builtin
use aiken/collection/list
use aiken/crypto.{verify_ed25519_signature}
use aiken/primitive/bytearray
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, resolve_input,
}
use midgard/common/utils
use midgard/da_params_governor as da_params_governor
use midgard/ledger_state.{HeaderHash}

/// DAAT tokens carry attestation certificates. This stateless policy cannot
/// prove per-header global uniqueness; downstream consumers must validate a
/// threshold-attested UTxO and must not rely on unique supply.
pub const asset_name_prefix: AssetName = "DAAT"

pub const state_queue_mint_reference_script_asset_name: AssetName =
  "StateQueueMint"

pub const signature_byte_count: Int = 64

pub const signature_witness_byte_count: Int = 65

pub const attested_signer_bitmap_byte_count: Int = 32

pub const empty_attested_signer_bitmap: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

pub type Datum {
  header_hash: HeaderHash,
  da_threshold: Int,
  /// Hash of the immutable sorted committee verification keys.
  committee_signers_hash: ByteArray,
  /// 256-bit MSB-first bitmap of committee indexes that have attested.
  attested_signers: ByteArray,
  attestation_count: Int,
}

pub type MintRedeemer {
  Init {
    output_index: Int,
    da_params_ref_input_index: Int,
    state_queue_ref_input_index: Int,
    state_queue_mint_ref_script_input_index: Int,
  }
  ApplyToStateQueue {
    da_attestation_input_index: Int,
    state_queue_input_index: Int,
    state_queue_output_index: Int,
    state_queue_mint_ref_script_input_index: Int,
  }
}

/// `AddSignatures.da_params_ref_input_index` points at the authenticated DA
/// params UTxO whose committee matches `Datum.committee_signers_hash`.
/// `AddSignatures.signatures` is a packed sequence of
/// `1-byte signer index || 64-byte Ed25519 signature` chunks. Indexes must be
/// strictly ascending and point to 32-byte verification key slots in the DA
/// params committee.
pub type SpendRedeemer {
  AddSignatures {
    output_index: Int,
    da_params_ref_input_index: Int,
    signatures: ByteArray,
  }
  BurnForStateQueue { mint_redeemer_index: Int }
}

pub fn asset_name(header_hash: HeaderHash) -> AssetName {
  expect bytearray.length(header_hash) == 28
  asset_name_prefix |> bytearray.concat(header_hash)
}

fn attestation_message(header_hash: HeaderHash) -> ByteArray {
  bytearray.from_string(@"MidgardDAAttestationV1")
    |> bytearray.concat(header_hash)
}

pub fn get_authenticated_state_queue_policy_id(
  reference_inputs: List<Input>,
  reference_script_auth_policy_id: PolicyId,
  state_queue_mint_ref_script_input_index: Int,
) -> PolicyId {
  expect Input {
    output: Output { value, reference_script: Some(state_queue_policy_id), .. },
    ..
  } =
    reference_inputs |> list.expect_at(state_queue_mint_ref_script_input_index)

  expect
    assets.has_nft_strict(
      value,
      reference_script_auth_policy_id,
      state_queue_mint_reference_script_asset_name,
    )

  state_queue_policy_id
}

pub fn validate_init_output(
  output: Output,
  own_policy_id: PolicyId,
  params: da_params_governor.Datum,
  expected_header_hash: HeaderHash,
) -> AssetName {
  expect Output {
    address,
    value,
    datum: InlineDatum(datum_data),
    reference_script: None,
  } = output
  expect utils.expect_script_hash(address) == own_policy_id

  let expected_datum_data: Data =
    Datum {
      header_hash: expected_header_hash,
      da_threshold: params.da_threshold,
      committee_signers_hash: params.committee_signers_hash,
      attested_signers: empty_attested_signer_bitmap,
      attestation_count: 0,
    }
  expect datum_data == expected_datum_data

  let attestation_asset = asset_name(expected_header_hash)
  expect assets.has_nft_strict(value, own_policy_id, attestation_asset)

  attestation_asset
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
      utils.bytearray_slice(
        committee_signers,
        signer_index * da_params_governor.verification_key_byte_count,
        da_params_governor.verification_key_byte_count,
      )
    let signature =
      utils.bytearray_slice(
        signatures,
        signature_cursor + 1,
        signature_byte_count,
      )
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

pub fn get_attestation_input_datum(
  inputs: List<Input>,
  input_index: Int,
  own_policy_id: PolicyId,
) -> Datum {
  expect Input {
    output: Output {
      address,
      value,
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
    ..
  } = inputs |> list.expect_at(input_index)

  expect utils.expect_script_hash(address) == own_policy_id
  expect datum: Datum = datum_data
  let attestation_asset = asset_name(datum.header_hash)
  expect assets.has_nft_strict(value, own_policy_id, attestation_asset)
  datum
}

pub fn validate_add_signatures(
  datum: Option<Datum>,
  output_index: Int,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
  signatures: ByteArray,
  own_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, reference_inputs, .. } = tx

  // 1. The spent datum must be present.
  expect Some(input_datum) = datum

  // 2. The script input must match the supplied datum exactly.
  expect Output {
    address: input_address,
    value: input_value,
    datum: InlineDatum(input_datum_data),
    reference_script: None,
  } = inputs |> resolve_input(own_ref)
  let expected_input_datum_data: Data = input_datum
  expect input_datum_data == expected_input_datum_data

  // 3. The script input must carry the exact header-specific attestation NFT.
  let own_policy_id = utils.expect_script_hash(input_address)
  let attestation_asset = asset_name(input_datum.header_hash)
  expect assets.has_nft_strict(input_value, own_policy_id, attestation_asset)

  // 4. The current DA params must authenticate the immutable committee for
  //    this attestation datum.
  let params =
    da_params_governor.get_da_params(
      reference_inputs,
      da_params_policy_id,
      da_params_ref_input_index,
    )
  expect params.committee_signers_hash == input_datum.committee_signers_hash

  // 5. The supplied signatures mark attested committee indexes in the bitmap.
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
  expect attestation_count > input_datum.attestation_count

  // 6. The continued output must preserve address and value exactly.
  expect Output {
    address: output_address,
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  } = outputs |> list.expect_at(output_index)
  expect output_address == input_address
  expect output_value == input_value

  // 7. The continued datum must preserve the stable fields and update only the
  //    attested signer bitmap and derived count.
  let expected_output_datum_data: Data =
    Datum {
      header_hash: input_datum.header_hash,
      da_threshold: input_datum.da_threshold,
      committee_signers_hash: input_datum.committee_signers_hash,
      attested_signers,
      attestation_count,
    }
  expect output_datum_data == expected_output_datum_data

  True
}
