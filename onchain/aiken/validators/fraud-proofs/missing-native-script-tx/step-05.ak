use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_native_script_tx/step_05.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_tx/step_06
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript,
}
use midgard/script_proof_v1

validator main(
  step_06_validator_script_hash: ScriptHash,
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
        missing_native_script_bytes,
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
            Some(State {
              expected_missing_script_hash,
              bad_tx_id,
              bad_tx_witness_set_hash,
            })
          = m_input_state_data

        // 2. The missing script hash must correspond to a native Cardano
        //    script under the canonical native versioned-script hash
        //    (language tag 0 prepended to the script bytes).
        expect
          expected_missing_script_hash == script_proof_v1.versioned_script_hash(
            MidgardVersionedScript {
              language: NativeCardanoScript,
              script_bytes: missing_native_script_bytes,
            },
          )

        // 3. The UTxO must be produced at next step's script hash with a proper
        //    state.
        expect output_script_hash == step_06_validator_script_hash
        let expected_output_state: Data = step_06.State {
          expected_missing_script_hash,
          bad_tx_id,
          bad_tx_witness_set_hash,
          phase: step_06.Ready,
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
