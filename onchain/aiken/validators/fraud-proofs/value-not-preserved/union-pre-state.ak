use aiken/cbor
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/value_not_preserved/union.{
  BalanceState, FoldState, empty_delta_root,
}
use midgard/fraud_proofs/value_not_preserved/union_state.{EventState, Inputs}
use midgard/ledger_state
use midgard/transition_trace

pub type Args {
  input_index: Int,
  output_index: Int,
  membership: transition_trace.IndexedTraceProof,
}

validator main(
  inputs_hash: ByteArray,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<EventState>>,
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
        let transition = membership.value
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
            transition_trace.TransitionTraceRootDomain,
            state.trace_root,
            state.trace_count,
            cbor.serialise(membership.key),
            cbor.serialise(transition),
          )
        expect membership.key == state.step_index
        expect transition.step_index == state.step_index
        expect
          transition.schema_version == ledger_state.transition_step_schema_version_v1
        expect
          transition.event_key == state.event_key && transition.phase == phase
        let context: Data = Inputs { cursor: 0 }
        let expected: Data = FoldState {
          balance: BalanceState {
            transaction_id: state.transaction_id,
            claim: state.claim,
            pre_utxos_root: transition.pre_utxos_root,
            lovelace_delta: 0 - state.fee,
            asset_delta_root: empty_delta_root(),
          },
          continuation: context,
        }
        output_hash == inputs_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
