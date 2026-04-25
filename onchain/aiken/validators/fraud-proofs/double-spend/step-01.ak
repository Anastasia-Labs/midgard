//// ## Double Spend Fraud Proof (Section 5.1.1)
////
//// Proves 2 unique transactions exist such that both spend the same UTxO.
////
//// First step's contract is responsible for verifying inclusion of the two
//// transactions in a Midgard block, and passing them to the second step's
//// contract.
////
//// This is meant to take place in two transactions:
//// 1. First transaction spends the initialized fraud proof UTxO (which has no
////    `data`) and reproduces the UTxO at own address with `data` containing
////    the first verified bad transaction's body.
//// 2. Second transaction spends the UTxO produced in the first transaction,
////    and produces the UTxO at the next step's script address with `data`
////    containing both verified bad transactions' bodies.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/double_spend/step_01.{Datum, SpendRedeemer, State}
use midgard/fraud_proofs/double_spend/step_02

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
          own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
          _header,
          bad_tx_id,
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

        when m_input_state_data is {
          None -> {
            // 2a. The UTxO must be produced at own script hash with the
            //     verified first bad transaction ID and its inputs included in
            //     the `data` field of its `StepDatum`.
            expect output_script_hash == own_script_hash
            let expected_output_state: Data =
              State {
                verified_tx1_id: bad_tx_id,
                verified_tx1_inputs_root: bad_tx_body.spend_inputs,
              }
            expect output_state_data == expected_output_state

            // Done.
            True
          }
          Some(step_1_state) -> {
            // 2b. The UTxO must be produced at next step's script hash with the
            //     verified bad transactions' inputs included in its datum.
            expect output_script_hash == step_02_validator_script_hash
            let State { verified_tx1_id, verified_tx1_inputs_root } =
              step_1_state
            let expected_output_state: Data =
              step_02.State {
                verified_tx1_inputs_root,
                verified_tx2_inputs_root: bad_tx_body.spend_inputs,
              }
            expect output_state_data == expected_output_state

            // 3b. The two transactions must be different.
            expect verified_tx1_id != bad_tx_id

            // Done.
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
