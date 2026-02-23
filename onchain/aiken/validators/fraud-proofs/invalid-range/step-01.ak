//// ## Invalid Range Fraud Proof (Section 5.1.2)
////
//// Proves a given transaction, which has a validity range with one or both
//// ends bounded, is included in a Midgard block whose validity range does not
//// cover the transaction's validity range. Or if the transaction has an
//// invalid time range (i.e. reversed interval).

use aiken/crypto.{ScriptHash}
use aiken_design_patterns/validity_range_normalization as vrn
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/invalid_range/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/invalid_range/step_02
use midgard/ledger_state.{Header}

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
        //    step must pass.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          _m_input_state_data,
          output_script_hash,
          output_state_data,
          header,
          _bad_tx_id,
          _bad_tx,
          bad_tx_body,
        <-
          pass_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        let Header {
          start_time: block_valid_from,
          end_time: block_valid_to,
          ..
        } = header

        let bad_tx_normalized_validity_range =
          vrn.normalize_time_range(bad_tx_body.validity_interval)

        // 2. The UTxO must be produced at next step's script hash with validity
        //    range of the bad tx, and block's time range provided.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            block_valid_from,
            block_valid_to,
            bad_tx_normalized_validity_range,
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
