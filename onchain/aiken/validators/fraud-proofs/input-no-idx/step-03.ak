use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/input_no_idx/step_03.{Datum, SpendRedeemer, State}
use midgard/fraud_proofs/input_no_idx/step_04

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
        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        //
        //    This function already validates this new tx is included in the
        //    same block as the first tx via the computation thread token's
        //    asset name.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
          _header,
          producing_tx_id,
          producing_tx_view,
        <-
          pass_native_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        expect Some(State { bad_input_tx_id, bad_input_output_index }) =
          m_input_state_data

        // 2. The verified producing tx ID must match the bad transaction's
        //    input.
        expect producing_tx_id == bad_input_tx_id

        // 3. The UTxO must be produced at next step's script hash with the
        //    producing transaction's native outputs hash included in its datum,
        //    plus the bad input's output index (expected to be larger than the
        //    last output in the producing transaction).
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data =
          step_04.State {
            producing_tx_outputs_hash: producing_tx_view.tx_compact.body.outputs_hash,
            bad_input_output_index,
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
