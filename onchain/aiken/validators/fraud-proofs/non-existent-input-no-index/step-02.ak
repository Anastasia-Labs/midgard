use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{plutarch_phas}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/non_existent_input_no_index/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/non_existent_input_no_index/step_03

validator main(
  step_03_validator_script_hash: ScriptHash,
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
        non_existent_input_key,
        non_existent_input_value,
        non_existent_input_membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

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

        expect Some(input_state) = m_input_state_data

        // 2. Provided input must be a member of the verified transaction's
        //    inputs.
        expect
          plutarch_phas(
            merkle_root: input_state.verified_tx_inputs_root,
            key: non_existent_input_key,
            value: non_existent_input_value,
            membership_proof: non_existent_input_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
          )

        // 3. The proven inputs key must be passed to the next step so that its
        //    producing transaction can be verified against the included hash.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data =
          step_03.State {
            verified_non_existent_input_key: non_existent_input_key,
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
