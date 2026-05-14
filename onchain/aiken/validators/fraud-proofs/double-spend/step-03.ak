use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/double_spend/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/double_spend/step_04
use midgard/fraud_proofs/native_tx

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
        tx1_spend_input_cbors,
        double_spent_input_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

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
          verified_tx1_spend_inputs_hash,
          verified_tx2_spend_inputs_hash,
        }) =
          m_input_state_data

        // 2. Provided native spend-inputs preimage for tx1 must be valid.
        expect
          native_tx.verify_spend_inputs_preimage(
            tx1_spend_input_cbors,
            verified_tx1_spend_inputs_hash,
          )

        // 3. The provided index must correspond to an input in the given list.
        let tx1_double_spent_input =
          native_tx.spend_input_cbor_at(
            tx1_spend_input_cbors,
            double_spent_input_index,
          )

        // 4. The fetched input, along with the unused second transaction inputs
        //    hash, must be carried over in the datum of the next step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data =
          step_04.State {
            verified_tx2_spend_inputs_hash,
            double_spent_input_cbor: tx1_double_spent_input,
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
