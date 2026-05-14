//// ## Double Spend Fraud Proof (Section 5.1.1)
////
//// Proves 2 unique transactions exist such that both spend the same UTxO.
////
//// First step's contract is responsible for verifying inclusion of the first
//// transaction in a Midgard block, and passing it to the second step's
//// contract. The second step verifies inclusion of the second transaction.
////
//// This is meant to take place in four transactions:
//// 1. First transaction spends the initialized fraud proof UTxO (which has no
////    `data`) and reproduces the UTxO at the next step's script address with
////    `data` containing the first verified bad transaction's ID and inputs
////    hash.
//// 2. Second transaction spends the UTxO produced in the first transaction and
////    produces the UTxO at the next step's script address with `data`
////    containing both verified bad transactions' inputs hashes.
//// 3. Third transaction provides the first bad transaction's inputs preimage and
////    carries the double-spent output reference to the next step.
//// 4. Fourth transaction provides the second bad transaction's inputs preimage
////    and finalizes the fraud proof.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, pass_native_tx_to_next_step,
}
use midgard/fraud_proofs/double_spend/step_01.{Datum, SpendRedeemer}
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
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
          _header,
          bad_tx_id,
          bad_tx_view,
        <-
          pass_native_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        expect None = m_input_state_data

        // 2. The UTxO must be produced at next step's script hash with the
        //    verified first bad transaction ID and native spend-input hash in
        //    the `data` field of its `StepDatum`.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            verified_tx1_id: bad_tx_id,
            verified_tx1_spend_inputs_hash: bad_tx_view.spend_inputs_hash,
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
