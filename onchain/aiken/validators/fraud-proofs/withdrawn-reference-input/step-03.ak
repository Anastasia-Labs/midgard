use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/withdrawn_reference_input/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/ledger_state.{WithdrawalBody, WithdrawalInfo, WithdrawalIsValid}

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
        withdrawal_id,
        withdrawal_info,
        membership_proof,
        membership_proof_script_redeemer_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

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

        expect Some(input_state) = m_input_state_data

        let State { missing_output_reference, blocks_withdrawals_root } =
          input_state

        // 4. The provided withdrawal event must be valid, and it must spend the
        //    input that was referenced in the bad transaction.
        expect WithdrawalInfo {
          body: WithdrawalBody { l2_outref, .. },
          validity: WithdrawalIsValid,
          ..
        } = withdrawal_info
        expect l2_outref == missing_output_reference

        // 5. The withdrawal event must be present in the withdrawals root of
        //    the block.
        expect
          utils.plutarch_phas(
            merkle_root: blocks_withdrawals_root,
            key: withdrawal_id,
            value: withdrawal_info,
            membership_proof: membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: membership_proof_script_redeemer_index,
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
