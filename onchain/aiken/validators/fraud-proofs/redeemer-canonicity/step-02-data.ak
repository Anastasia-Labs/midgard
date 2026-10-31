use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/redeemer_canonicity/rule.{
  ContinueData, FinishData, advance_data_v1,
}
use midgard/fraud_proofs/redeemer_canonicity/step_02_data.{
  Args, Datum, SpendRedeemer,
}

validator main(step_03_hash: ScriptHash, computation_thread_policy: PolicyId) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        window,
        revealed,
        tail_root,
        node_budget,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(scan) = input_state
        when
          advance_data_v1(scan, window, revealed, tail_root, node_budget)
        is {
          ContinueData { scan: next } -> {
            let value: Data = next
            and {
              next_hash == own_hash,
              next_state == value,
            }
          }
          FinishData { terminal } -> {
            let value: Data = terminal
            and {
              next_hash == step_03_hash,
              next_state == value,
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
