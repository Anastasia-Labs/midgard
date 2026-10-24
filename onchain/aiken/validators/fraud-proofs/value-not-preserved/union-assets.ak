use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/value_not_preserved/union.{
  FoldState, PendingContribution,
}
use midgard/fraud_proofs/value_not_preserved/union_state.{AssetCursor}
use midgard/ledger_output_commitment_v1
use midgard/validation_merkle_v1.{FrontierPeak} as frontier

pub type Action {
  Select {
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    peaks: List<FrontierPeak>,
    siblings: List<ByteArray>,
  }
  Finish
}

pub type Args {
  input_index: Int,
  output_index: Int,
  action: Action,
}

validator main(
  union_update_hash: ByteArray,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<FoldState>>,
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
        let own_hash, _thread_name, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect cursor: AssetCursor = state.continuation
        when args.action is {
          Select { policy_id, asset_name, quantity, peaks, siblings } -> {
            expect cursor.cursor < cursor.count
            expect
              frontier.frontier_commitment(cursor.count, peaks) == cursor.frontier_commitment
            expect
              frontier.verify_membership(
                cursor.count,
                peaks,
                cursor.cursor,
                ledger_output_commitment_v1.asset_leaf_hash(
                  policy_id,
                  asset_name,
                  quantity,
                ),
                siblings,
              )
            let next: Data = AssetCursor { ..cursor, cursor: cursor.cursor + 1 }
            let pending: Data = PendingContribution {
              unit: bytearray.concat(policy_id, asset_name),
              quantity: cursor.quantity_sign * quantity,
              return_script_hash: own_hash,
              next_continuation: next,
            }
            let expected: Data = FoldState { ..state, continuation: pending }
            output_hash == union_update_hash && output_state == expected
          }
          Finish -> {
            expect cursor.cursor == cursor.count
            let expected: Data = FoldState {
              ..state,
              continuation: cursor.next_continuation,
            }
            output_hash == cursor.next_script_hash && output_state == expected
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
