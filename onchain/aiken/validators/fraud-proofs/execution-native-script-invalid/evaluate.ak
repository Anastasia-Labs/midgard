use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/evaluate

validator main(terminal_hash: ScriptHash, thread_policy: PolicyId) {
  spend(
    datum: Option<evaluate.Datum>,
    redeemer: evaluate.SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(page) -> {
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          page.input_index,
          page.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        let Pair(next_hash, expected) = when evaluate.page_v1(state, page) is {
          evaluate.Paged { state: next_state } -> {
            let next: Data = next_state
            Pair(own_hash, next)
          }
          evaluate.Finished { state: next_state } -> {
            let next: Data = next_state
            Pair(terminal_hash, next)
          }
        }
        and {
          output_hash == next_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
