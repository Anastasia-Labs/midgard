use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/native_script_invalid/step_01.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/native_script_invalid/step_02

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
      ct.Continue(Args { carriage }) -> {
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            bad_tx_id,
            bad_tx_view
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          carriage,
          own_out_ref,
          tx,
        )
        expect None = m_input_state_data
        expect bad_tx_view.tx_compact.validity_code == 0
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_id,
          bad_tx_witness_set_hash: bad_tx_view.tx_compact.witness_set_hash,
          validity_interval_start: bad_tx_view.tx_compact.body.validity_interval_start,
          validity_interval_end: bad_tx_view.tx_compact.body.validity_interval_end,
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
