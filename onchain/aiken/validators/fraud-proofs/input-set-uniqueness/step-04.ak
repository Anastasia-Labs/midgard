//// Forced `DuplicateInput` contradiction — one authenticated union item per
//// resume, then finalize only after the complete union is strictly ordered.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, reference_inputs_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/input_set_uniqueness/rule
use midgard/fraud_proofs/input_set_uniqueness/step_04.{
  Advance, Datum, Finalize, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{
  field_item_count, fixed_stride_item_batch,
}

validator main(
  fraud_policy: PolicyId,
  fraud_address: Address,
  thread_policy: PolicyId,
  field_certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Advance { input_index, output_index, field_opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect rule.scan_state_is_authentic_v1(state)
        expect state.next_expected_script_hash == own_hash
        let reading_spend = state.cursor < state.spend_count
        let field_index = if reading_spend {
          spend_inputs_field_index
        } else {
          reference_inputs_field_index
        }
        let item_index = if reading_spend {
          state.cursor
        } else {
          state.cursor - state.spend_count
        }
        let expected_count = if reading_spend {
          state.spend_count
        } else {
          state.reference_count
        }
        let view = opened_field_view(
          field_opening,
          BodyAnchor { tx_id: state.bound.subject.transaction_id },
          field_index,
          reference_inputs,
          field_certificate_policy,
        )
        expect field_item_count(view) == expected_count
        let remaining = expected_count - item_index
        let batch_count = if remaining < 128 {
          remaining
        } else {
          128
        }
        expect batch_count > 0
        let expected = rule.advance_unique_batch_v1(
          state,
          fixed_stride_item_batch(view, item_index, batch_count),
          own_hash,
        )
        let expected_data: Data = expected
        and {
          output_hash == own_hash,
          output_state == expected_data,
        }
      }
      ct.Continue(Finalize {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state <- finalize(
          thread_policy,
          fraud_policy,
          fraud_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_ref,
          inputs,
          outputs,
          redeemers,
        )
        expect Some(state) = input_state
        rule.unique_scan_is_complete_v1(state)
      }
    }
  }

  else(_) {
    fail
  }
}
