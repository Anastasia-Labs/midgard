use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/missing_native_script_tx/step_03.{
  Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_tx/step_04

validator main(
  step_04_validator_script_hash: ScriptHash,
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
        // 1. Generic validations for passing the producing tx to the next step
        //    must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            producing_tx_id,
            producing_tx_view
          <- pass_native_tx_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        expect
            Some(State { input_with_missing_script, verified_script_wits_hash })
          = m_input_state_data

        // 2. The producing tx must correspond to the bad input's transaction
        //    ID.
        expect producing_tx_id == input_with_missing_script.tx_id

        // 3. The UTxO must be produced at next step's script hash with the
        //    producing transaction's native outputs hash and the bad input's
        //    output index.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          producing_tx_outputs_hash: producing_tx_view.tx_compact.body.outputs_hash,
          bad_input_output_index: input_with_missing_script.output_index,
          verified_script_wits_hash,
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
