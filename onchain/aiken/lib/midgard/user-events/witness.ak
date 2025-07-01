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

/// Witness script's prefix from its single CBOR encoded applied bytearray.
/// TODO: This should probably be moved to `env` with a better generation
///       process.
pub const witness_script_prefix: ByteArray =
  #"59030601010033232323232323223225333004323232323253330093370e900318051baa00113233223232323253330103003001132325333015301800200416375c602c00260246ea802454ccc040c0100044c8c94ccc054c06000801058dd6980b00098091baa009153330103370e900200089919299980a980c0010020b1bad3016001301237540122c60206ea80204c8c8c8c94ccc048c0140044c8c8c94ccc054c0200044c02400854ccc054c0240044cdc3801240022940c054dd500499299980a1804180a9baa0011480004dd6980c980b1baa001325333014300830153754002298103d87a80001323300100137566034602e6ea8008894ccc064004530103d87a8000132333222533301a337220300062a66603466e3c06000c4cdd2a40006603c6ea00092f5c02980103d87a8000133006006001375c60300026eb4c064004c074008c06c004c8cc004004dd5980c980d180d180d180d180b1baa00e22533301800114c103d87a800013233322253330193372200e0062a66603266e3c01c00c4cdd2a40006603a6e980092f5c02980103d87a8000133006006001375c602e0026eacc060004c070008c068004dd7180b980a1baa00b153330123006001132323253330153375e0040142a66602a6010602c6ea80084c94ccc058c028c05cdd5001099baf001301b301837540042c6034602e6ea80085858c064c068008c060004cc00cdd61801180a1baa00c375a602e60286ea802c4c94ccc04cc01cc050dd500409919299980a9804180b1baa00113375e6034602e6ea800400858c064cc014dd61802180b1baa00e0023018301537540102c6eb4c05cc050dd500598091baa00a23016301730173017301730170013001001222533301133712002900008010a99980a0010a5eb804ccc00c00cc05c008cdc0000a40026e1d2000370e90011bad300f001300f3010001300b37540022c601a601c006601800460160046016002600c6ea800452613656375c002ae6955ceaab9e5573eae815d0aba24c1225820"

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
