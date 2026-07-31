//// ## Invalid Signature Fraud Proof (Section 5.1.6)
////
//// Proves that a transaction committed by a block carries an address witness
//// whose Ed25519 signature does not verify against the transaction's ID.
////
//// The ledger rule is unconditional over every transaction in the block
//// (`∀t ∈ Ledger, ∀(v, s, h) ∈ addr_tx_wits(t): is_valid_signature(v, s, h)`),
//// so this proof deliberately does not consult the transaction's
//// `validity_code`. A block may not excuse an invalid signature by declaring
//// the offending transaction invalid.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/invalid_signature/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/invalid_signature/step_02

validator main(
  step_02_validator_script_hash: ScriptHash,
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
        //    step must pass. `pass_native_tx_to_next_step` authenticates the raw
        //    native tx against the header's counted `transactions_root`.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          _m_input_state_data,
          output_script_hash,
          output_state_data,
          _header,
          bad_tx_id,
          bad_tx_view,
        <-
          pass_native_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        // 2. The UTxO must be produced at next step's script hash with the bad
        //    transaction's ID and the hash of its witness set.
        //
        //    The tx ID doubles as the signed message: `native_tx_id` is the
        //    blake2b-256 of the compact body CBOR, which is exactly what an
        //    address witness signs.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            bad_tx_id,
            bad_tx_wits_hash: bad_tx_view.tx_compact.witness_set_hash,
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
