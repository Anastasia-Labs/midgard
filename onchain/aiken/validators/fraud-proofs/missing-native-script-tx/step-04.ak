use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_native_script_tx/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_tx/step_05

validator main(
  step_05_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, outputs_preimage }) -> {
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
          producing_tx_outputs_hash,
          bad_input_output_index,
          verified_script_wits_hash,
        }) = m_input_state_data

        // 2. Provided outputs preimage must match the producing transaction
        //    outputs hash.
        expect outputs_preimage |> verify_hash_32(producing_tx_outputs_hash)
        let output_with_missing_script =
          outputs_preimage |> list.expect_at(bad_input_output_index)

        // 3. The spent output must be locked by a script credential. The next
        //    step proves this script hash corresponds to a native script.
        expect Script(expected_missing_script_hash) =
          output_with_missing_script.address

        // 4. The UTxO must be produced at next step's script hash with a proper
        //    state.
        expect output_script_hash == step_05_validator_script_hash
        let expected_output_state: Data =
          step_05.State {
            expected_missing_script_hash,
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
