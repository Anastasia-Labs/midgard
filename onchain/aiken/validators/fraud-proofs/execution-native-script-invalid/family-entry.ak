use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/entry

validator main(
  purpose_select_hash: ScriptHash,
  spend_key_hash: ScriptHash,
  terminal_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<entry.Datum>,
    redeemer: entry.SpendRedeemer,
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
      ct.Continue(action) -> {
        let Pair(input_index, output_index) = entry.indices_v1(action)
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        let Pair(next_hash, expected) = when action is {
          entry.Observe { .. } -> {
            let next: Data = state
            Pair(purpose_select_hash, next)
          }
          entry.Contradict { .. } -> {
            let next: Data = entry.contradiction_v1(state)
            Pair(terminal_hash, next)
          }
          entry.ObserveAbsent { opening, .. } -> {
            let next: Data = entry.observe_absent_v1(
              state,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(terminal_hash, next)
          }
          entry.SpendAbsent { opening, .. } -> {
            let next: Data = entry.spend_absent_v1(
              state,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(terminal_hash, next)
          }
          entry.SpendKey { .. } -> {
            let next: Data = state
            Pair(spend_key_hash, next)
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
