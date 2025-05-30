use aiken/collection/list
use aiken/crypto.{Blake2b_256, Hash}
use cardano/assets
use cardano/certificate.{Certificate, RegisterCredential, UnregisterCredential}
use cardano/transaction.{Publish, Transaction}
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
          RegisterToProveNotRegistered { registration_redeemer_index } -> {
            // 1. The registration redeemer must be immediately followed by the
            //    unregistration counterpart.
            expect [registration_pair,
              unregistration_pair, ..] =
              tx.redeemers |> list.drop(registration_redeemer_index)

            expect Pair(Publish {
              certificate: RegisterCredential { credential: reg_cred, .. } as reg_cert,
              ..
            },
            reg_redeemer_data) = registration_pair

            // 2. Ensure correctness of the provided redeemer index.
            expect reg_cert == cert
            expect RegisterToProveNotRegistered { .. } = reg_redeemer_data

            expect Pair(Publish {
              certificate: UnregisterCredential { credential: unreg_cred, .. },
              ..
            },
            unreg_redeemer_data) = unregistration_pair

            // 3. The unregistration redeemer must invoke the same script.
            expect reg_cred == unreg_cred

            // 4. The index carried in the unregistration redeemer must match
            //    the index from the registration redeemer.
            expect UnregisterToProveNotRegistered {
              registration_redeemer_index: index_from_unregistration,
            } = unreg_redeemer_data
            index_from_unregistration == registration_redeemer_index
          }
          UnregisterToProveNotRegistered { registration_redeemer_index } -> {
            // 1. Current certificate must be an `UnregisterCredential`.
            expect UnregisterCredential { credential: unreg_cred, .. } = cert

            // 2. The provided index must point to a registration endpoint.
            expect [registration_pair, ..] =
              tx.redeemers |> list.drop(registration_redeemer_index)
            expect Pair(Publish {
              certificate: RegisterCredential { credential: reg_cred, .. },
              ..
            },
            reg_redeemer_data) = registration_pair

            // 3. Credential from the registration endpoint must match the
            //    current credential.
            expect reg_cred == unreg_cred

            // 4. Index from the registration endpoint must be equal to the
            //    index from the current endpoint.
            expect RegisterToProveNotRegistered {
              registration_redeemer_index: index_from_registration,
            } = reg_redeemer_data
            registration_redeemer_index == index_from_registration
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
