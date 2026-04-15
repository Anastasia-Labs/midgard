use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/no_reference_input/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/no_reference_input/step_03

validator main(
  step_03_validator_script_hash: ScriptHash,
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
        no_reference_input_id,
        no_reference_input_resolved,
        membership_proof_in_bad_tx,
        membership_proof_script_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
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

        expect Some(State {
          bad_tx_reference_inputs_root,
          blocks_prev_utxos_root,
          blocks_withdrawals_root,
        }) = m_input_state_data

        // 2. Target input must be included in the verified transaction inputs.
        expect
          utils.plutarch_phas(
            merkle_root: bad_tx_reference_inputs_root,
            key: no_reference_input_id,
            value: no_reference_input_resolved,
            membership_proof: membership_proof_in_bad_tx,
            redeemers: redeemers,
            withdraw_redeemer_index: membership_proof_script_redeemer_index,
          )

        // 3. 
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data =
          step_03.State {
            missing_output_reference: no_reference_input_id
              |> ledger_state.midgard_input_id_to_out_ref,
            blocks_prev_utxos_root,
            blocks_transactions_root,
            blocks_withdrawals_root,
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
