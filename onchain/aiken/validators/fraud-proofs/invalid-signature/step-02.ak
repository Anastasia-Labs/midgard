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
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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

        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
          _m_input_state_data,
          output_script_hash,
          output_state_data,
          _header,
          _bad_tx_id,
          bad_tx,
          bad_tx_body,
        <-
          pass_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
        <-
          continue(
            computation_thread_token_policy_id,
            step_datum,
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
          )

        // 2. Verify that the provided `tx_wits` correctly corresponds to the
        //    verified bad tx.
        expect verify_hash_32(bad_tx_wits, bad_tx.wits)

        // 3. Prove that <VerificationKeyHash, (VerificationKey, Signature)>
        //    belongs to addr_tx_wits(tx)
        expect
          utils.plutarch_phas(
            bad_tx_wits.addr_tx_wits,
            bad_addr_tx_wit_verification_key_hash,
            bad_addr_tx_wit,
            bad_addr_tx_wit_membership_proof,
            tx.redeemers,
            plutarch_phas_withdraw_redeemer_index,
          )

        let (verification_key, signature): (VerificationKey, Signature) =
          bad_addr_tx_wit

        // 4. The UTxO must be produced at next step's script hash
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            bad_tx_body,
            bad_addr_tx_wit_verification_key_hash,
            verification_key,
            signature,
          }
        expect output_state_data == expected_output_state

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

