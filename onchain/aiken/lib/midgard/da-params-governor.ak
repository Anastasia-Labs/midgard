use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use cardano/address.{Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}

pub const asset_name: AssetName = "MIDGARD_DA_PARAMS"

pub const verification_key_byte_count: Int = 32

pub const verification_key_hash_byte_count: Int = 28

pub const max_indexed_signer_count: Int = 256

pub type Datum {
  /// Sorted verification keys, packed as 32-byte chunks. Governance owners
  /// remain key hashes because updates use transaction extra signatories.
  committee: ByteArray,
  /// Hash of `committee`, validated by the DA params governor.
  committee_signers_hash: ByteArray,
  da_threshold: Int,
  owners: List<VerificationKeyHash>,
  update_threshold: Int,
}

pub fn get_da_params(
  reference_inputs: List<Input>,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
) -> Datum {
  // 1. The referenced params input must exist and expose an inline datum.
  expect Input {
    output: Output {
      address,
      value,
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
    ..
  } = reference_inputs |> list.expect_at(da_params_ref_input_index)

  // 2. The params input must be locked by the expected governor policy.
  expect Script(params_script_hash) = address.payment_credential
  expect params_script_hash == da_params_policy_id

  // 3. The params input must carry exactly one params NFT.
  expect assets.quantity_of(value, da_params_policy_id, asset_name) == 1

  // 4. The inline datum must be a params datum. Its shape is enforced by the
  //    params governor mint/update paths.
  expect datum: Datum = datum_data
  datum
}
