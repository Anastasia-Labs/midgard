use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/input_no_idx/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/input_no_idx/step_03

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
        inputs_preimage,
        bad_inputs_index,
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

        expect Some(State { verified_tx_inputs_hash }) = m_input_state_data

        // 2. Provided inputs preimage must correspond correctly to the passed
        //    along hash.
        expect inputs_preimage |> verify_hash_32(verified_tx_inputs_hash)

        let bad_inputs_outref =
          inputs_preimage |> list.expect_at(bad_inputs_index)

        // 3. The fetched inputs tx id and output index must be passed to the
        //    next step so that its producing transaction can be verified
        //    against the included hash.
        expect output_script_hash == step_03_validator_script_hash
        let OutputReference { transaction_id, output_index } = bad_inputs_outref
        let expected_output_state: Data =
          step_03.State {
            bad_input_tx_id: transaction_id,
            bad_input_output_index: output_index,
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
