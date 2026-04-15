use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/types.{MerkleRoot} as midgard_types
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/invalid_signature/step_01.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/invalid_signature/step_02
use midgard/ledger_state.{MidgardTxWitnessSetCompact}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: ScriptHash,
  fraud_proof_token_address: Address,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        bad_tx_wits,
        bad_addr_tx_wit_verification_key_hash,
        bad_addr_tx_wit,
        bad_addr_tx_wit_membership_proof,
        plutarch_phas_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
        <-
          finalize(
            computation_thread_token_policy_id,
            fraud_proof_token_policy_id,
            fraud_proof_token_address,
            step_datum,
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            own_out_ref,
            inputs,
            outputs,
            redeemers,
          )

        expect Some(State { bad_tx_body, bad_addr_tx_wits_root }) =
          m_input_state_data

        // 2. Prove that <VerificationKeyHash, (VerificationKey, Signature)>
        //    belongs to addr_tx_wits(tx)
        expect
          utils.plutarch_phas(
            bad_addr_tx_wits_root,
            bad_addr_tx_wit_verification_key_hash,
            bad_addr_tx_wit,
            bad_addr_tx_wit_membership_proof,
            redeemers,
            plutarch_phas_withdraw_redeemer_index,
          )

        let (verification_key, signature): (VerificationKey, Signature) =
          bad_addr_tx_wit

        // 3. Verification key hash must be wrong OR the signature is invalid
        expect or {
            blake2b_224(verification_key) != bad_addr_tx_wit_verification_key_hash,
            verify_ed25519_signature(
              verification_key,
              builtin.serialise_data(bad_tx_body),
              signature,
            ) == False,
          }

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

pub fn resolve_address_tx_wits_from_data(
  address_tx_wits_as_data: Data<
    Dict<VerificationKeyHash, (VerificationKey, Signature)>,
  >,
) -> Dict<VerificationKeyHash, (VerificationKey, Signature)> {
  // Return the resolved hash
  expect coerced: Pairs<VerificationKeyHash, (VerificationKey, Signature)> =
    address_tx_wits_as_data

  // Fails if not in ascending order
  coerced |> dict.from_ascending_pairs
}
