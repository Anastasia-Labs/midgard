use aiken/collection/pairs
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/missing_native_script_tx/step_06.{
  Args, Datum, SpendRedeemer, State,
}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
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
        fraud_proof_mint_redeemer_index,
        script_tx_wits_preimage,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
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

        expect
            Some(State {
              expected_missing_script_hash,
              verified_script_wits_hash,
            })
          = m_input_state_data

        // 2. The provided script witnesses preimage must match the hash carried
        //    from the previous steps.
        expect
          script_tx_wits_preimage |> verify_hash_32(verified_script_wits_hash)

        // 3. The bad transaction's script witnesses must not contain the
        //    required native script.
        let required_script_is_present = script_tx_wits_preimage
          |> pairs.has_key(expected_missing_script_hash)
        expect required_script_is_present == False

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
