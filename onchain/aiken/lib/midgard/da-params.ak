//// The governed DA params UTxO and its owner quorum, shared by every contract
//// that reads them: `validators/da-params-governor.ak`,
//// `validators/da-attestation.ak` and `validators/da-bond-pool.ak`. One copy
//// of each rule, so the three contracts cannot drift apart.

use aiken/collection/list
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/da_attestation_types.{DaParamsDatum, da_params_asset_name}

pub fn has_key(
  keys: List<VerificationKeyHash>,
  key: VerificationKeyHash,
) -> Bool {
  when keys is {
    [] -> False
    [current, ..rest] ->
      if current == key {
        True
      } else {
        has_key(rest, key)
      }
  }
}

/// Counts the owners that signed, walking `owners` rather than the signer list,
/// so a signer listed twice is still counted once. Owners are sorted and unique
/// because the governor validates every datum it writes.
pub fn owner_quorum_met(
  owners: List<VerificationKeyHash>,
  signers: List<VerificationKeyHash>,
  threshold: Int,
  count: Int,
) -> Bool {
  if count >= threshold {
    True
  } else {
    when owners is {
      [] -> False
      [owner, ..rest] -> {
        let next_count = if has_key(signers, owner) {
          count + 1
        } else {
          count
        }
        owner_quorum_met(rest, signers, threshold, next_count)
      }
    }
  }
}

/// The authenticated DA params datum: the reference input at
/// `da_params_ref_input_index` sits at `Script(da_params_policy_id)`, holds the
/// params NFT once, carries an inline datum and no reference script, and its
/// committee re-derives its committee hash.
pub fn get_da_params(
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
  // re-derivation site for a params read, and it is paid once per read.
  expect blake2b_256(datum.committee) == datum.committee_signers_hash

  datum
}
