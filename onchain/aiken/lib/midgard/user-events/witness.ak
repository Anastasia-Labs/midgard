use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/certificate.{RegisterCredential, UnregisterCredential}
use cardano/transaction.{Publish, Redeemer, ScriptPurpose}

pub type PublishRedeemer {
  MintOrBurn { target_policy: PolicyId }
  RegisterToProveNotRegistered { registration_certificate_index: Int }
  UnregisterToProveNotRegistered { registration_certificate_index: Int }
}

pub fn validate_witness_redeemer(
  expected_witness_script_hash: ScriptHash,
  witness_redeemer_index: Int,
  for_registration: Bool,
  event_policy_id: PolicyId,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  // Grab redeemer pair of witness script registration.
  expect Some(witness_redeemer_pair) =
    redeemers |> list.at(witness_redeemer_index)

  // Grab the included `Publish` script hash (no validation on `certificate`
  // index, i.e. `at` field under `Publish`, TODO?).
  let present_witness_script_hash =
    if for_registration {
      expect Publish {
        certificate: RegisterCredential {
          credential: Script(witness_script_hash),
          ..
        },
        ..
      } = witness_redeemer_pair.1st
      witness_script_hash
    } else {
      expect Publish {
        certificate: UnregisterCredential {
          credential: Script(witness_unregistration_script_hash),
          ..
        },
        ..
      } = witness_redeemer_pair.1st
      witness_unregistration_script_hash
    }

  // The only endpoint allowed for this validation is the `MintOrBurn` redeemer,
  // provided with a correct policy ID.
  let expected_witness_redeemer: Data =
    MintOrBurn { target_policy: event_policy_id }

  and {
    // 1. Registered script must match the expected witness.
    present_witness_script_hash == expected_witness_script_hash,
    // 2. Deposit / withdrawal / tx order policy ID, and the event identifier
    //    must be set correctly.
    witness_redeemer_pair.2nd == expected_witness_redeemer,
  }
}
