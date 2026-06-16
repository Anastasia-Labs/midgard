use aiken/collection/list
use aiken/crypto.{verify_ed25519_signature}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Output, OutputReference, Transaction, resolve_input,
}
use midgard/da_params_governor as da_params_governor
use midgard/ledger_state.{HeaderHash}

/// DAAT tokens carry attestation certificates. This stateless policy cannot
/// prove per-header global uniqueness; downstream consumers must validate a
/// threshold-attested UTxO and must not rely on unique supply.
pub const asset_name_prefix: AssetName = "DAAT"

pub const signature_byte_count: Int = 64

pub const signature_witness_byte_count: Int = 65

pub type Datum {
  header_hash: HeaderHash,
  da_threshold: Int,
  /// Sorted verification keys, packed as 32-byte chunks.
  remaining_signers: ByteArray,
  attestation_count: Int,
}

pub type MintRedeemer {
  Init {
    output_index: Int,
    da_params_ref_input_index: Int,
    state_queue_ref_input_index: Int,
  }
  ApplyToStateQueue {
    da_attestation_input_index: Int,
    state_queue_input_index: Int,
    state_queue_output_index: Int,
  }
}

/// `AddSignatures.signatures` is a packed sequence of
/// `1-byte signer index || 64-byte Ed25519 signature` chunks. Indexes must be
/// strictly ascending and point to 32-byte verification key slots in the input
/// datum's `remaining_signers`.
pub type SpendRedeemer {
  AddSignatures { output_index: Int, signatures: ByteArray }
  BurnForStateQueue { mint_redeemer_index: Int }
}

pub fn asset_name(header_hash: HeaderHash) -> AssetName {
  expect bytearray.length(header_hash) == 28
  asset_name_prefix |> bytearray.concat(header_hash)
}

pub fn bytearray_slice(
  bytes: ByteArray,
  offset: Int,
  byte_count: Int,
) -> ByteArray {
  bytearray.slice(bytes, offset, offset + byte_count - 1)
}

pub fn output_at_index(outputs: List<Output>, output_index: Int) -> Output {
  outputs |> list.expect_at(output_index)
}

pub fn expect_script_hash(address: Address) -> PolicyId {
  expect Script(script_hash) = address.payment_credential
  script_hash
}

fn attestation_message(header_hash: HeaderHash) -> ByteArray {
  bytearray.from_string(@"MidgardDAAttestationV1")
    |> bytearray.concat(header_hash)
}

fn verify_indexed_signatures(
  signatures: ByteArray,
  message: ByteArray,
  input_remaining: ByteArray,
  output_remaining: ByteArray,
  min_signer_index: Int,
  input_cursor: Int,
  output_cursor: Int,
  signature_cursor: Int,
  input_len: Int,
  signatures_len: Int,
  count: Int,
) -> Int {
  if signature_cursor == signatures_len {
    expect
      bytearray.drop(input_remaining, input_cursor) == bytearray.drop(
        output_remaining,
        output_cursor,
      )
    count
  } else {
    expect signature_cursor + signature_witness_byte_count <= signatures_len
    let signer_index = bytearray.at(signatures, signature_cursor)
    expect signer_index >= min_signer_index
    let signer_offset =
      signer_index * da_params_governor.verification_key_byte_count
    let next_input_cursor =
      signer_offset + da_params_governor.verification_key_byte_count
    expect next_input_cursor <= input_len

    let copied_byte_count = signer_offset - input_cursor
    expect
      if copied_byte_count == 0 {
        True
      } else {
        bytearray_slice(input_remaining, input_cursor, copied_byte_count) == bytearray_slice(
          output_remaining,
          output_cursor,
          copied_byte_count,
        )
      }

    let verification_key =
      bytearray_slice(
        input_remaining,
        signer_offset,
        da_params_governor.verification_key_byte_count,
      )
    let signature =
      bytearray_slice(signatures, signature_cursor + 1, signature_byte_count)
    expect verify_ed25519_signature(verification_key, message, signature)

    verify_indexed_signatures(
      signatures,
      message,
      input_remaining,
      output_remaining,
      signer_index + 1,
      next_input_cursor,
      output_cursor + copied_byte_count,
      signature_cursor + signature_witness_byte_count,
      input_len,
      signatures_len,
      count + 1,
    )
  }
}

pub fn validate_add_signatures(
  datum: Option<Datum>,
  output_index: Int,
  signatures: ByteArray,
  own_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx

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
  let own_policy_id = expect_script_hash(input_address)
  let Datum {
    header_hash: input_header_hash,
    da_threshold: input_da_threshold,
    remaining_signers: input_remaining_signers,
    attestation_count: input_attestation_count,
  } = input_datum
  let attestation_asset = asset_name(input_header_hash)
  expect assets.has_nft_strict(input_value, own_policy_id, attestation_asset)

  // 4. The continued output must preserve address and value exactly.
  expect Output {
    address: output_address,
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  } = outputs |> output_at_index(output_index)
  expect output_address == input_address
  expect output_value == input_value

  // 5. The output remaining signer set feeds signature verification.
  expect output_datum: Datum = output_datum_data
  let Datum { remaining_signers: output_remaining_signers, .. } = output_datum

  // 6. The supplied signatures must remove exactly the signed committee slots.
  let verified_count =
    verify_indexed_signatures(
      signatures,
      attestation_message(input_header_hash),
      input_remaining_signers,
      output_remaining_signers,
      0,
      0,
      0,
      0,
      bytearray.length(input_remaining_signers),
      bytearray.length(signatures),
      0,
    )

  // 7. At least one signature must be added and the attestation count must
  //    increase by exactly the verified count.
  expect verified_count > 0

  // 8. The continued datum must preserve the stable fields and update only the
  //    signer set and attestation count according to verified signatures.
  let expected_output_datum_data: Data =
    Datum {
      header_hash: input_header_hash,
      da_threshold: input_da_threshold,
      remaining_signers: output_remaining_signers,
      attestation_count: input_attestation_count + verified_count,
    }
  expect output_datum_data == expected_output_datum_data

  True
}
