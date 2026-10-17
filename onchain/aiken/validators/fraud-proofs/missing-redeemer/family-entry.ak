use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_redeemer/entry

validator main(
  purpose_select_hash: ScriptHash,
  claim_start_hash: ScriptHash,
  thread_policy: PolicyId,
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
        let _, _, fraud_prover, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        // The route is a free choice that would otherwise strand the owner's
        // thread, so only the owner may make it.
        expect list.has(tx.extra_signatories, fraud_prover)
        when action is {
          entry.Observe { .. } -> {
            let expected: Data = state
            and {
              output_hash == purpose_select_hash,
              output_state == expected,
            }
          }
          entry.Claim { .. } -> {
            let expected: Data = entry.claim_context_v1(state)
            and {
              output_hash == claim_start_hash,
              output_state == expected,
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
