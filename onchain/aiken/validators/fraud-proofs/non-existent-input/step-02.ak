use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/non_existent_input/step_02.{
  Args, Datum, SpendRedeemer, State,
}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
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
        non_existent_input_key,
        non_existent_input_value,
        membership_proof_in_bad_tx,
        membership_proof_script_redeemer_index,
        non_membership_proof_in_ledger,
        non_membership_proof_script_redeemer_index,
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

        expect Some(State { bad_tx_inputs_root, blocks_prev_utxos_root }) =
          m_input_state_data

        let non_existent_input_key_data: Data = non_existent_input_key

        // 2. Target input must be included in the verified transaction inputs.
        expect
          utils.plutarch_phas(
            merkle_root: bad_tx_inputs_root,
            key: non_existent_input_key_data,
            value: non_existent_input_value,
            membership_proof: membership_proof_in_bad_tx,
            redeemers: redeemers,
            withdraw_redeemer_index: membership_proof_script_redeemer_index,
          )

        // 3. Target input must be absent from the verified block's prev ledger.
        expect
          utils.plutarch_pexcludes(
            merkle_root: blocks_prev_utxos_root,
            key: non_existent_input_key_data,
            non_membership_proof: non_membership_proof_in_ledger,
            redeemers: redeemers,
            withdraw_redeemer_index: non_membership_proof_script_redeemer_index,
          )

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
