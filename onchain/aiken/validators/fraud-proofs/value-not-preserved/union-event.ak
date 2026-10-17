use aiken/cbor
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/value_not_preserved/union_state.{
  EventState, SourceState,
}
use midgard/ledger_state
use midgard/transition_trace

pub type Args {
  input_index: Int,
  output_index: Int,
  membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
}

validator main(
  pre_state_hash: ByteArray,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<SourceState>>,
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
        let _own_hash, _thread_name, _, input_state, output_hash, output_state
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        let membership = args.membership
        let phase = when state.event_key is {
          ledger_state.L2TransactionEventKey { .. } ->
            ledger_state.L2Transaction
          ledger_state.ForcedTransactionEventKey { .. } ->
            ledger_state.ForcedTransaction
          _ -> fail
        }
        expect
          transition_trace.verify_root_membership_with_bytes(
            membership,
            transition_trace.EventToStepRootDomain,
            state.event_root,
            state.event_count,
            cbor.serialise(membership.key),
            cbor.serialise(membership.value),
          )
        expect membership.key == state.event_key
        expect membership.value.phase == phase
        expect
          membership.value.step_index >= 0 && membership.value.step_index < state.trace_count
        let expected: Data = EventState {
          transaction_id: state.transaction_id,
          claim: state.claim,
          fee: state.fee,
          event_key: state.event_key,
          trace_root: state.trace_root,
          trace_count: state.trace_count,
          step_index: membership.value.step_index,
        }
        output_hash == pre_state_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
