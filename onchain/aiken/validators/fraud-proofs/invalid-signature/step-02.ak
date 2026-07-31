use aiken/collection/list
use aiken/crypto.{blake2b_256, verify_ed25519_signature}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/invalid_signature/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_tx/compact
use midgard/fraud_proofs/native_tx/preimages
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, NativeTxWitnessSetCompact,
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
        witness_set_preimage,
        addr_tx_wits_preimage,
        bad_address_witness_index,
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

        expect Some(State { bad_tx_id, bad_tx_wits_hash }) = m_input_state_data

        // 2. Provided witness set preimage must reproduce the witness set hash
        //    carried from the previous step, which in turn exposes the
        //    authentic address witnesses hash.
        expect
          blake2b_256(
            compact.encode_native_tx_witness_set_compact(witness_set_preimage),
          ) == bad_tx_wits_hash
        let NativeTxWitnessSetCompact { addr_tx_wits_hash, .. } =
          witness_set_preimage

        // 3. Provided address witnesses preimage must in turn reproduce that
        //    hash. `encode_address_witness_preimage` is length prefixed, so a
        //    matching preimage fixes the witness count and every witness's
        //    position, making the accused index unambiguous.
        expect
          blake2b_256(
            preimages.encode_address_witness_preimage(addr_tx_wits_preimage),
          ) == addr_tx_wits_hash

        // 4. The witness at the accused index must carry a signature that does
        //    not verify against the bad transaction's ID.
        let MidgardAddressWitness { verification_key, signature } =
          addr_tx_wits_preimage |> list.expect_at(bad_address_witness_index)
        expect
          verify_ed25519_signature(verification_key, bad_tx_id, signature) == False

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
