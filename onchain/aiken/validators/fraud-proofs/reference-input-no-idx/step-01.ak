//// ## Reference Input with No Index Fraud Proof (Section 5.1.1)
////
//// The reference-input mirror of `input-no-idx`. Proves a given transaction
//// *reads* a reference input, such that the preimage of its transaction ID
//// exists within the block (i.e. the UTxO was produced by another transaction
//// in the block), yet its output index is larger than the last output of its
//// producing transaction.
////
//// Only this first step differs from `input-no-idx`: it commits the bad
//// transaction's native **reference**-inputs hash instead of its spend-inputs
//// hash. Steps 02-04 are structurally identical (they only ever see an input
//// list preimage and a producing transaction), so they compile to the same
//// UPLC as their `input-no-idx` counterparts.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/reference_input_no_idx/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/reference_input_no_idx/step_02

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
          _bad_tx_id,
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

        // 2. The UTxO must be produced at next step's script hash with the
        //    native reference-inputs hash of the bad transaction included in its
        //    datum.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            verified_tx_reference_inputs_hash: bad_tx_view.tx_compact.body.reference_inputs_hash,
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
