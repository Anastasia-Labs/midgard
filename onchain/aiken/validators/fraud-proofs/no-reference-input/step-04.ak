use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/no_reference_input/step_04.{
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
        non_membership_proof_in_txs,
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
              missing_reference_input_tx_id,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. The missing reference input must not have been produced in the
        //    same block. The transactions MPF is keyed by the raw 32-byte
        //    native tx id, so the non-membership key is the tx id bytes
        //    directly.
        expect
          utils.plutarch_pexcludes_raw(
            merkle_root: blocks_transactions_root,
            key_bytes: missing_reference_input_tx_id,
            non_membership_proof: non_membership_proof_in_txs,
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
