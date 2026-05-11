use aiken/collection/dict
use aiken/crypto.{ScriptHash, verify_ed25519_signature}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/invalid_signature/step_02.{
  Args, Datum, SpendRedeemer, State,
}

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
        addr_tx_wits_preimage,
        bad_addr_tx_wit_verification_key,
        fraud_proof_mint_redeemer_index,
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

        expect Some(State { bad_tx_id, bad_addr_tx_wits_hash }) =
          m_input_state_data

        // 2. Provided address witnesses preimage must match the hash carried
        //    from the previous step.
        expect addr_tx_wits_preimage |> verify_hash_32(bad_addr_tx_wits_hash)

        // 3. Fetch the signature witnessed by the provided verification key.
        let bad_addr_tx_wit_signature =
          addr_tx_wits_preimage
            |> dict.expect_get(bad_addr_tx_wit_verification_key)

        // 4. The signature must be invalid.
        expect
          verify_ed25519_signature(
            bad_addr_tx_wit_verification_key,
            bad_tx_id,
            bad_addr_tx_wit_signature,
          ) == False

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
