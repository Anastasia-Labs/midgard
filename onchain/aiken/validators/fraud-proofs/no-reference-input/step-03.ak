use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/no_reference_input/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/no_reference_input/step_04

validator main(
  step_04_validator_script_hash: ScriptHash,
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
        non_membership_proof_in_ledger,
        non_membership_proof_script_redeemer_index,
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
          missing_output_reference,
          blocks_prev_utxos_root,
          blocks_transactions_root,
        }) = m_input_state_data

        // 3. The block's transactions root must be passed to next step's
        //    address.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data =
          step_04.State {
            missing_ref_input_tx_id: missing_output_reference.transaction_id,
            blocks_transactions_root,
          }
        expect output_state_data == expected_output_state

        // 4. Target input must be absent from the verified block's prev ledger.
        expect
          utils.plutarch_pexcludes(
            merkle_root: blocks_prev_utxos_root,
            key: missing_output_reference,
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
