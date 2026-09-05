use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/value_not_preserved/union.{
  ConservationClaim, ForcedConservation,
}
use midgard/fraud_proofs/value_not_preserved/union_state.{SourceState}
use midgard/ledger_state
use midgard/rejection_reason_v1.{ValueNotPreserved}
use midgard/transition_trace

pub type Args {
  input_index: Int,
  output_index: Int,
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
}

validator main(
  event_hash: ByteArray,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<ConservationClaim>>,
    redeemer: ct.StepRedeemer<Args>,
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
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let _own_hash, thread_name, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect state == ForcedConservation
        let subject = substrate.bind_forced_subject_to_thread_v1(
          thread_name,
          args.header,
          args.membership,
          1,
        )
        expect ValueNotPreserved = substrate.rejection_reason_of_v1(subject)
        let source = args.membership.value.source
        let Pair(verified, _) = verify_native_tx_proof_source_v1(
          subject.transaction_id,
          source.compact_cbor,
          source.witness_set_compact_cbor,
          source.field_preimage_lengths_cbor,
        )
        let expected: Data = SourceState {
          transaction_id: subject.transaction_id,
          claim: state,
          fee: verified.tx_compact.body.fee,
          event_key: ledger_state.ForcedTransactionEventKey {
            tx_order_id: args.membership.key,
          },
          event_root: args.header.event_to_step_root,
          event_count: args.header.total_event_count,
          trace_root: args.header.transition_trace_root,
          trace_count: args.header.transition_step_count,
        }
        output_hash == event_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
