use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyFieldOpening, opened_field_view,
  required_observers_field_index,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/fraud_proofs/observers_forbidden_on_untagged_network/rule.{
  terminal_contradiction_v1,
}
use midgard/fraud_proofs/observers_forbidden_on_untagged_network/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{field_item_count}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
        observer_opening,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state <- finalize(
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
        expect Some(state) = input_state
        expect BodyFieldOpening { native_tx_compact_cbor, .. } = observer_opening
        let verified = verify_native_tx_compact_cbor_v1(
          state.subject.transaction_id,
          native_tx_compact_cbor,
        )
        expect verified.tx_compact.body.network_id == state.network_id
        let view = opened_field_view(
          observer_opening,
          BodyAnchor { tx_id: state.subject.transaction_id },
          required_observers_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        terminal_contradiction_v1(state, field_item_count(view))
      }
    }
  }

  else(_) {
    fail
  }
}
