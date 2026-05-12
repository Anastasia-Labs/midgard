use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/input_no_idx/step_04.{
  Args, Datum, SpendRedeemer, State,
}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        fraud_proof_mint_redeemer_index,
        outputs_preimage,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
        <-
          finalize(
            computation_thread_token_policy_id,
            fraud_proof_token_policy_id,
            fraud_proof_token_address,
            step_datum,
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            own_out_ref,
            inputs,
            outputs,
            redeemers,
          )

        expect Some(State { producing_tx_outputs_hash, bad_input_output_index }) =
          m_input_state_data

        // 2. Provided outputs preimage must match the producing transaction
        //    outputs hash.
        expect outputs_preimage |> verify_hash_32(producing_tx_outputs_hash)

        // 3. With preimage of outputs verified, it must be shown its length is
        //    smaller than the output index passed down from previous step.
        expect bad_input_output_index >= list.length(outputs_preimage)

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
