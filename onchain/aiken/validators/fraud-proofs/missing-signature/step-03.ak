use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_signature/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_signature/step_04

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
        missing_required_signer_vkey,
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
              missing_required_signer_hash,
              verified_addr_tx_wits_hash,
            })
          = m_input_state_data

        // 2. The provided verification key must correspond correctly to the
        //    required signer hash passed down from the previous step.
        expect
          utils.get_verification_key_hash(missing_required_signer_vkey) == missing_required_signer_hash

        // 3. The UTxO must be produced at next step's script hash with a proper
        //    state.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          missing_required_signer_vkey,
          verified_addr_tx_wits_hash,
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
