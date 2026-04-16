//// ## No Reference Input & Withdrawn Reference Input Fraud Proofs (Section 5.1.16)
////
//// Proves a given transaction refers to an input that was either missing from
//// the block's starting ledger (i.e. `prev_utxos_root`), or had been spent in
//// a withdrawal event.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxBodyProvided, cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/no_reference_input/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/no_reference_input/step_02
use midgard/ledger_state.{Header}

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
      ct.Continue(tx_inclusion_args) -> {
        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          _m_input_state_data,
          output_script_hash,
          output_state_data,
          header,
          _bad_tx_id,
          _bad_tx,
          bad_tx_field_preimage,
        <-
          pass_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        // 2. Bad transaction's body must be provided.
        expect TxBodyProvided(bad_tx_body) = bad_tx_field_preimage

        // 3. The UTxO must be produced at next step's script hash with relevant
        //    roots.
        let Header { prev_utxos_root, withdrawals_root, transactions_root, .. } =
          header
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            bad_tx_reference_inputs_root: bad_tx_body.reference_inputs,
            blocks_prev_utxos_root: prev_utxos_root,
            blocks_transactions_root: transactions_root,
            blocks_withdrawals_root: withdrawals_root,
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
