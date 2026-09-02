//// Forced `DuplicateInput` contradiction — authenticate both complete sets
//// and seed the bounded strict-order scan.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyTxOpening, anchored_field_view, anchored_native_tx,
  reference_inputs_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/input_set_uniqueness/rule
use midgard/fraud_proofs/input_set_uniqueness/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/native_tx_field_access_v1.{field_item_count}

validator main(
  step_04_hash: ScriptHash,
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
      ct.Continue(Args {
        input_index,
        output_index,
        native_tx_compact_cbor,
        spend_inputs_carriage,
        reference_inputs_carriage,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(State { bound }) = input_state
        let anchored = anchored_native_tx(
          BodyTxOpening { native_tx_compact_cbor },
          BodyAnchor { tx_id: bound.subject.transaction_id },
        )
        let spend_view = anchored_field_view(
          anchored,
          spend_inputs_field_index,
          spend_inputs_carriage,
          reference_inputs,
          field_certificate_policy,
        )
        let reference_view = anchored_field_view(
          anchored,
          reference_inputs_field_index,
          reference_inputs_carriage,
          reference_inputs,
          field_certificate_policy,
        )
        let expected = rule.initial_scan_state_v1(
          bound,
          field_item_count(spend_view),
          field_item_count(reference_view),
          step_04_hash,
        )
        let expected_data: Data = expected
        and {
          output_hash == step_04_hash,
          output_state == expected_data,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
