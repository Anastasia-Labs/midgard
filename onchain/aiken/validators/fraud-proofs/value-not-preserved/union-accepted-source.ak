use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  NativeTxInclusionCarriage, cancel, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/value_not_preserved/step_01.{
  claimed_asset_is_well_formed_v1,
}
use midgard/fraud_proofs/value_not_preserved/union.{
  AcceptedImbalance, ConservationClaim,
}
use midgard/fraud_proofs/value_not_preserved/union_state.{SourceState}
use midgard/ledger_state

validator main(
  event_hash: ByteArray,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ByteArray,
) {
  spend(
    datum: Option<ct.StepDatum<ConservationClaim>>,
    redeemer: ct.StepRedeemer<NativeTxInclusionCarriage>,
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
      ct.Continue(carriage) -> {
        let
            _,
            _,
            _,
            input_state,
            output_hash,
            output_state,
            header,
            transaction_id,
            verified
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          carriage,
          own_out_ref,
          tx,
        )
        expect Some(claim) = input_state
        expect AcceptedImbalance { asset, .. } = claim
        expect claimed_asset_is_well_formed_v1(asset)
        expect verified.tx_compact.validity_code == Some(0)
        let expected: Data = SourceState {
          transaction_id,
          claim,
          fee: verified.tx_compact.body.fee,
          event_key: ledger_state.L2TransactionEventKey {
            tx_id: transaction_id,
          },
          event_root: header.event_to_step_root,
          event_count: header.total_event_count,
          trace_root: header.transition_trace_root,
          trace_count: header.transition_step_count,
        }
        output_hash == event_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
