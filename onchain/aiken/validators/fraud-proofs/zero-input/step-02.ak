//// ## Zero Input Fraud Proof (Section 5.1.1)
////
//// Concludes the proof that the transaction carried through step-01 spends no
//// inputs.

use aiken/crypto.{blake2b_256}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_tx/codec.{encode_native_byte_list}
use midgard/fraud_proofs/native_tx/preimages.{verify_spend_inputs_preimage}
use midgard/fraud_proofs/zero_input/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/ledger_state.{MidgardInputsHash}

/// The `spend_inputs_hash` a native transaction body carries when it spends
/// nothing. Derived from the same encoder the native codec hashes real input
/// lists with, so it cannot drift from `verify_spend_inputs_preimage`.
///
/// > [!NOTE]
/// > This is *not* `env.empty_list_hash`, which hashes the PlutusData encoding
/// > of an empty list. Native bodies are committed under the native codec.
const empty_spend_inputs_hash: MidgardInputsHash =
  [] |> encode_native_byte_list |> blake2b_256

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        fraud_proof_mint_redeemer_index,
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

        expect Some(State { bad_tx_spend_inputs_hash }) = m_input_state_data

        // 2. The spend inputs list must be empty.
        expect bad_tx_spend_inputs_hash == empty_spend_inputs_hash

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

test empty_spend_inputs_hash_matches_the_native_preimage_encoding() {
  verify_spend_inputs_preimage([], empty_spend_inputs_hash)
}

/// Pins the constant to the exact value the offchain codec derives, so the two
/// encoders cannot drift apart silently. The SDK asserts the same literal in
/// `EMPTY_SPEND_INPUTS_HASH`.
test empty_spend_inputs_hash_matches_the_offchain_constant() {
  empty_spend_inputs_hash == #"45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
}

test empty_spend_inputs_hash_rejects_a_non_empty_input_list() {
  !verify_spend_inputs_preimage(
    [#"d8799f581c00000000000000000000000000000000000000000000000000000000ff"],
    empty_spend_inputs_hash,
  )
}
