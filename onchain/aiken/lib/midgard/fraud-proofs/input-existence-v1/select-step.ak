use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, opened_field_view}
use midgard/fraud_proofs/input_existence_v1/select.{
  claimed_input_v1, selected_index_v1, selected_input_v1,
}
use midgard/fraud_proofs/input_existence_v1/types.{
  SelectArgsV1, SelectedInputStateV1, SourceStateV1,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

pub fn select_step_v1(
  field_index: Int,
  next_hash: ByteArray,
  policy: PolicyId,
  certificate_policy: PolicyId,
  datum: Option<ct.StepDatum<SourceStateV1>>,
  redeemer: ct.StepRedeemer<SelectArgsV1>,
  own_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  when redeemer is {
    ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
      cancel(
        policy,
        datum,
        input_index,
        own_ref,
        computation_thread_mint_redeemer_index,
        tx,
      )
    ct.Continue(SelectArgsV1 {
      input_index,
      output_index,
      inputs_opening,
      selected_input_index,
      claimed_inputs_opening,
      event_membership,
    }) -> {
      expect Some(step_datum) = datum
      let _, _, _, input_state, output_hash, output_state <- continue(
        policy,
        step_datum,
        input_index,
        output_index,
        own_ref,
        tx.inputs,
        tx.outputs,
        tx.extra_signatories,
      )
      expect Some(SourceStateV1 { subject, header, compact }) = input_state
      let index = selected_index_v1(subject, field_index, selected_input_index)
      let view = opened_field_view(
        inputs_opening,
        BodyAnchor {
          tx_id: subject.transaction_id,
          source_kind: subject.source_kind,
        },
        field_index,
        tx.reference_inputs,
        certificate_policy,
      )
      let expected: Data = SelectedInputStateV1 {
        subject,
        header,
        compact,
        source_kind: field_index,
        input_index: index,
        claimed_input: claimed_input_v1(
          subject,
          field_index,
          view,
          claimed_inputs_opening,
          tx.reference_inputs,
          certificate_policy,
        ),
        selected_input: selected_input_v1(subject, field_index, view, index),
        step_index: substrate.subject_event_step_index_v1(
          header,
          subject,
          event_membership,
          tx.reference_inputs,
          tx.redeemers,
        ),
      }
      output_hash == next_hash && output_state == expected
    }
  }
}
