use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/value_not_preserved/union.{
  BalanceState, FoldState, PendingContribution, apply_contribution,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  old_delta: Int,
  proof: mpf.Proof,
}

validator main(computation_thread_token_policy_id: PolicyId) {
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
        expect pending: PendingContribution = state.continuation
        let root = apply_contribution(
          state.balance.asset_delta_root,
          pending.unit,
          pending.quantity,
          args.old_delta,
          args.proof,
        )
        let expected: Data = FoldState {
          balance: BalanceState { ..state.balance, asset_delta_root: root },
          continuation: pending.next_continuation,
        }
        output_hash == pending.return_script_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
