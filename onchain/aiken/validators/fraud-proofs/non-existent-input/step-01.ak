//// ## Non-Existent Input UTxO Fraud Proof (Section 5.1.1)
////
//// Proves a given transaction spends an input that does not exist in the
//// initial ledger (i.e. `prev_utxos_root`) of its containing block.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/non_existent_input/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/non_existent_input/step_02

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

        // 2. The UTxO must be produced at next step's script hash with inputs
        //    root of the bad transaction, and block's prev UTxOs root.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            bad_tx_inputs_root: bad_tx_body.spend_inputs,
            blocks_prev_utxos_root: header.prev_utxos_root,
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
