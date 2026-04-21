use aiken/builtin
use aiken/crypto.{ScriptHash, verify_ed25519_signature}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
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
        bad_addr_tx_wit_verification_key,
        bad_addr_tx_wit_signature,
        bad_addr_tx_wit_membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
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

        expect Some(State { bad_tx_body, bad_addr_tx_wits_root }) =
          m_input_state_data

        // 2. Prove that <VerificationKey, Signature> belongs to
        //    addr_tx_wits(tx).
        expect
          utils.plutarch_phas(
            merkle_root: bad_addr_tx_wits_root,
            key: bad_addr_tx_wit_verification_key,
            value: bad_addr_tx_wit_signature,
            membership_proof: bad_addr_tx_wit_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
          )

        // 3. The signature must be invalid.
        expect
          verify_ed25519_signature(
            bad_addr_tx_wit_verification_key,
            builtin.serialise_data(bad_tx_body),
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
