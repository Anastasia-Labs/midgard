use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, verify_tx_and_its_body}
use midgard/fraud_proofs/non_existent_input_no_index/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/non_existent_input_no_index/step_04
use midgard/ledger_state.{MidgardInputId}

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
        previous_tx,
        previous_tx_body,
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

        expect Some(State{verified_non_existent_input_key: MidgardInputId {
          transaction_id: non_existent_input_tx_id,
          output_index: non_existent_input_output_index,
          ..
        }}) = m_input_state_data

        // 2. The provided transaction must match the transaction hash of the
        //    picked input (i.e. the one that is supposedly non-existent).
        expect
          verify_tx_and_its_body(
            non_existent_input_tx_id,
            previous_tx,
            previous_tx_body,
          )

        // 3. The UTxO must be produced at next step's script hash with the
        //    verified transaction's outputs root included in its datum, the
        //    non-existent input's output index (which is expected to be larger
        //    than the last output in the provided trie), and output index `0`
        //    which is the first output to be deleted from the trie.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data =
          step_04.State {
            verified_outputs_root: previous_tx_body.outputs,
            non_existent_input_output_index,
            output_index_to_delete: 0,
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
