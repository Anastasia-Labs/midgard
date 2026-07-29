use aiken/collection/list
use aiken/crypto.{blake2b_256}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/input_no_idx/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_tx/codec

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

        // 2. Provided outputs preimage (raw per-output CBOR byte list) must
        //    reproduce the producing transaction's native outputs hash once
        //    wrapped with `encode_native_byte_list`.
        expect
          blake2b_256(codec.encode_native_byte_list(outputs_preimage)) == producing_tx_outputs_hash

        // 3. With the preimage of outputs verified, the bad input's output index
        //    must be out of range of the producing transaction's outputs.
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
