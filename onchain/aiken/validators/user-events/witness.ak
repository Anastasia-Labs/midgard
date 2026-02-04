//// This is the parameterized staking script that must be instantiated with a
//// given unique ID for each user event, which is assumed to be hashed with
//// Blake2b-256.
////
//// To get the actual prefix CBOR to be used by user events contracts, a mock
//// bytearray of 32 bytes must first be applied to the resulting compiled CBOR.
//// Next, the bytes leading up to the mock value must be extracted, ensuring it
//// is encoded in CBOR only once.

use aiken/collection/list
use aiken/crypto.{Blake2b_256, Hash}
use cardano/assets
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{Transaction}
use midgard/user_events/witness.{
  MintOrBurn, PublishRedeemer, RegisterToProveNotRegistered,
  UnregisterToProveNotRegistered,
}

validator main(nonce: Hash<Blake2b_256, ByteArray>) {
  publish(redeemer: PublishRedeemer, certificate: Certificate, tx: Transaction) {
    when redeemer is {
      MintOrBurn { target_policy } -> {
        let mint_qty = tx.mint |> assets.quantity_of(target_policy, nonce)

        when certificate is {
          RegisterCredential { .. } -> mint_qty == 1
          UnregisterCredential { .. } -> mint_qty == -1
          _ -> False
        }
      }
      RegisterToProveNotRegistered { registration_certificate_index } -> {
        // 1. The registration certificate must be immediately followed by the
        //    unregistration counterpart.
        expect [reg_cert,
          unreg_cert, ..] =
          tx.certificates |> list.drop(registration_certificate_index)

        // 2. Ensure correctness of the provided certificate index.
        expect reg_cert == certificate

        expect RegisterCredential { credential: reg_cred, .. } = reg_cert

        expect UnregisterCredential { credential: unreg_cred, .. } = unreg_cert

        // 3. The unregistration certificate must be of the same script.
        expect reg_cred == unreg_cred

        // Done.
        True
      }
      UnregisterToProveNotRegistered { registration_certificate_index } -> {
        // 1. Current certificate must be an `UnregisterCredential`.
        expect UnregisterCredential { credential: unreg_cred, .. } = cert

        // 2. The provided index must point to a registration certificate.
        expect [reg_cert, ..] =
          tx.certificates |> list.drop(registration_certificate_index)
        expect RegisterCredential { credential: reg_cred, .. } = reg_cert

        // 3. Credential from the registration certificate must match the
        //    current credential.
        expect reg_cred == unreg_cred

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
