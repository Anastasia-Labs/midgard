use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_redeemer/adjudicate

validator main(
  terminal_hash: ScriptHash,
  claimed_redeemer_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<adjudicate.Datum>,
    redeemer: adjudicate.SpendRedeemer,
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
        let Pair(input_index, output_index) = adjudicate.indices_v1(action)
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        let Pair(next_hash, expected) = when action is {
          adjudicate.Resolve { .. } -> {
            let next: Data = adjudicate.resolve_v1(state)
            Pair(terminal_hash, next)
          }
          adjudicate.Lookup { source, proof, .. } -> {
            let next: Data = adjudicate.lookup_v1(state, source, proof)
            Pair(terminal_hash, next)
          }
          adjudicate.LookupScan { source, proof, opening, .. } -> {
            let next: Data = adjudicate.lookup_scan_v1(
              state,
              source,
              proof,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(claimed_redeemer_grammar_hash, next)
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
