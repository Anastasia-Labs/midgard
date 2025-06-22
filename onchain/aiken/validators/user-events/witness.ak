use aiken/collection/list
use aiken/crypto.{Blake2b_256, Hash}
use cardano/assets
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{Transaction}
use midgard/common/parameter_validation
use midgard/user_events/witness.{
  MintOrBurn, PublishRedeemer, RegisterToProveNotRegistered,
  UnregisterToProveNotRegistered,
}

validator main(nonce: Hash<Blake2b_256, ByteArray>) {
  publish(redeemer: PublishRedeemer, certificate: Certificate, tx: Transaction) {
    parameter_validation.prehashed_param_no_datum_wrapper(
      validator_function: fn(
        nonce: Hash<Blake2b_256, ByteArray>,
        publish_redeemer: PublishRedeemer,
        cert: Certificate,
        tx: Transaction,
      ) -> Bool {
        when publish_redeemer is {
          MintOrBurn { target_policy } -> {
            let mint_qty = tx.mint |> assets.quantity_of(target_policy, nonce)

            when cert is {
              RegisterCredential { .. } -> mint_qty == 1
              UnregisterCredential { .. } -> mint_qty == -1
              _ -> False
            }
          }
          RegisterToProveNotRegistered { registration_certificate_index } -> {
            // 1. The registration certificate must be immediately followed by
            //    the unregistration counterpart.
            expect [reg_cert,
              unreg_cert, ..] =
              tx.certificates |> list.drop(registration_certificate_index)

            // 2. Ensure correctness of the provided certificate index.
            expect reg_cert == cert

            expect RegisterCredential { credential: reg_cred, .. } = reg_cert

            expect UnregisterCredential { credential: unreg_cred, .. } =
              unreg_cert

            // 3. The unregistration certificate must be of the same script.
            reg_cred == unreg_cred
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
            reg_cred == unreg_cred
          }
        }
      },
      param: nonce,
      redeemer: redeemer,
      variable_arg: certificate,
      tx: tx,
    )
  }

  else(_) {
    fail
  }
}
