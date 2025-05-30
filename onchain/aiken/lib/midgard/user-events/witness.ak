use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/certificate.{RegisterCredential, UnregisterCredential}
use cardano/transaction.{Publish, Redeemer, ScriptPurpose}

pub type PublishRedeemer {
  MintOrBurn { target_policy: PolicyId }
  RegisterToProveNotRegistered { registration_redeemer_index: Int }
  UnregisterToProveNotRegistered { registration_redeemer_index: Int }
}

/// Witness script's prefix from its single CBOR encoded applied bytearray.
/// TODO: This should probably be moved to `env` with a better generation
///       process.
pub const witness_script_prefix: ByteArray =
  #"5903f201010033232323232323223225333004323232323253323300a3001300b3754004264664464646464a666022600800226464a66602c60320040082c6eb8c05c004c04cdd50050a999808980180089919299980b180c8010020b1bad3017001301337540142a66602266e1d2004001132325333016301900200416375a602e00260266ea802858c044dd5004899191919299980998030008991919299980b1804800898040010a99980b1804000899b8700248004528180b1baa0093253330153007301637540022900009bad301a3017375400264a66602a600e602c6ea80045300103d87a8000132330010013756603660306ea8008894ccc068004530103d87a8000132333222533301b337220320062a66603666e3c06400c4cdd2a40006603e6ea00092f5c02980103d87a8000133006006001375c60320026eb4c068004c078008c070004c8cc004004dd5980d180d980d980d980d980b9baa00f22533301900114c103d87a8000132333222533301a3372200e0062a66603466e3c01c00c4cdd2a40006603c6e980092f5c02980103d87a8000133006006001375c60300026eacc064004c074008c06c004dd7180c180a9baa00c153330133005001132323232323253330193010301a3754002264a666034601a60366ea80044c94ccc06ccdd78010078a99980d9806980e1baa004132325333020302300213232533301f301630203754002264a666040602460426ea800454ccc080cdd7803181298111baa001153330203370e900218109baa003132325333025302800213370e00201e2c6eb4c098004c088dd50018b0b0b1812181298109baa00116301f009301f00816375a6042002603a6ea80105858c07cc070dd50008b180f180f980d9baa0011630190023019001301b002301a301b0013300437566006602c6ea8038004dd6980c180a9baa00c1325333014300630153754010264646464a666030601e60326ea80044c94ccc064c030c068dd50008a99980c99baf301e301b375400200a2a666032601660346ea800c4c8c94ccc078c0840084cdc38040008b1bad301f001301b37540062c2c2c603a603c60346ea800458c060008c060004c068cc014dd59802180b9baa00f0023019301637540102c6eb4c060c054dd500618099baa00b230173018301830183018301830183018301830180013001001222533301233712002900008010a99980a8010a5eb7bdb1804ccc00c00cc060008cdc0000a40026e1d2002370e90001bad301000130103011001300c37540046e1d200616300d300e003300c002300b002300b0013006375400229309b2b1bae0015734aae7555cf2ab9f5740ae855d12601225820"

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
