use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_03
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
        bad_input_index,
        spend_inputs_opening,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect
            Some(State { bad_tx_id, bad_tx_witness_set_hash, prev_utxos_root })
          = m_input_state_data
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let input_with_missing_script = spend_input_at(
          spend_inputs_view,
          bad_input_index,
        )
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          input_with_missing_script,
          bad_tx_id,
          bad_tx_witness_set_hash,
          prev_utxos_root,
        }
        expect output_state_data == expected_output_state
        True
      }
    }
  }

  else(_) {
    fail
  }
}
