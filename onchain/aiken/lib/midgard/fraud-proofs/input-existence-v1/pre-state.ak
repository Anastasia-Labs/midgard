use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/first_fault_v1 as order
use midgard/fraud_proofs/input_existence_v1/types.{
  LedgerStateV1, PreStateArgsV1, SelectedInputStateV1,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

pub fn pre_state_step_v1(
  next_hash: ByteArray,
  policy: PolicyId,
  datum: Option<ct.StepDatum<SelectedInputStateV1>>,
  redeemer: ct.StepRedeemer<PreStateArgsV1>,
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
    ct.Continue(PreStateArgsV1 {
      input_index,
      output_index,
      transition_membership,
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
      )
      expect Some(state) = input_state
      let pre_state = substrate.subject_selected_event_pre_state_v1(
        state.header,
        state.subject,
        state.step_index,
        transition_membership,
        tx.reference_inputs,
        tx.redeemers,
      )
      let expected: Data = LedgerStateV1 {
        subject: state.subject,
        compact: state.compact,
        network_order_context: order.network_order_context_v1(
          state.compact,
          state.header,
        ),
        source_kind: state.source_kind,
        input_index: state.input_index,
        claimed_input: state.claimed_input,
        selected_input: state.selected_input,
        pre_utxos_root: pre_state.pre_root,
      }
      output_hash == next_hash && output_state == expected
    }
  }
}
