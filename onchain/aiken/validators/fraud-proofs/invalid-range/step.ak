//// ## Invalid Range Fraud Proof (Section 5.1.2)
////
//// Proves a given transaction, which has a validity range with one or both
//// ends bounded, is included in a Midgard block whose validity range does not
//// cover the transaction's validity range.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/unordered as unordered
use aiken_design_patterns/validity_range_normalization.{
  Always, ClosedRange, FromNegInf, ToPosInf,
} as vrn
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, finalize, verify_bad_tx_in_state_queue_node_and_get_header,
}
use midgard/fraud_proofs/invalid_range/step.{Args, Datum, SpendRedeemer}
use midgard/hub_oracle as hub
use midgard/ledger_state.{Header}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(Args {
        input_index,
        output_index,
        hub_ref_input_index,
        bad_state_queue_node_ref_input_index,
        bad_tx_id,
        bad_tx,
        bad_tx_body,
        bad_tx_membership_proof,
        fraud_proof_mint_redeemer_index,
        inclusion_proof_script_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } =
          tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
          _own_script_hash,
          computation_thread_token_asset_name,
          _fraud_prover,
          _m_input_state_data,
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

        // 2. The provided transaction must be valid, and must be included in
        //    the specified state queue node.
        let Header {
          start_time: block_valid_from,
          end_time: block_valid_to,
          ..
        } =
          verify_bad_tx_in_state_queue_node_and_get_header(
            bad_tx_id,
            bad_tx,
            bad_tx_body,
            bad_tx_membership_proof,
            computation_thread_token_asset_name,
            hub_oracle,
            hub_ref_input_index,
            bad_state_queue_node_ref_input_index,
            inclusion_proof_script_withdraw_redeemer_index,
            reference_inputs,
            redeemers,
          )

        let bad_tx_normalized_validity_range =
          vrn.normalize_time_range(bad_tx_body.validity_interval)

        // 3. One or more **bounds** of the bad transaction's validity interval
        //    must fall out of the block's. Meaning unbounded ends are not
        //    validated against the block's time range.
        //
        //    > [!NOTE]
        //    > We are assuming inclusive lower bound and exclusive upper bound
        //    > in the block. TODO: This needs clarification in specs.
        expect
          when bad_tx_normalized_validity_range is {
            ClosedRange { lower, upper } -> or {
                lower < block_valid_from,
                upper >= block_valid_to,
              }
            FromNegInf { upper } -> upper >= block_valid_to
            ToPosInf { lower } -> lower < block_valid_from
            Always -> fail @"The tx does not have an invalid time range"
          }

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

pub fn get_validity_range_validate_block_hash(
  tx: Transaction,
  state_queue_node_ref_input_index: Int,
) -> (Int, Int) {
  // Reference input with block_hash 
  expect Some(state_queue_node_input) =
    list.at(tx.reference_inputs, state_queue_node_ref_input_index)

  // Extract tx_root via the state queue block hash
  expect InlineDatum(state_queue_datum_data) =
    state_queue_node_input.output.datum
  expect state_queue_node_datum: unordered.NodeDatum = state_queue_datum_data
  expect parsed_state_queue_datum: Header = state_queue_node_datum.data
  (parsed_state_queue_datum.start_time, parsed_state_queue_datum.end_time)
}
