use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate.{
  Lookup, LookupScan, Resolve, indices_v1,
}
use midgard/fraud_proofs/receive_purpose_language/lookup

validator main(
  terminal_hash: ScriptHash,
  claimed_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<lookup.Datum>,
    redeemer: lookup.SpendRedeemer,
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
        let Pair(input_index, output_index) = indices_v1(action)
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
          Resolve { .. } -> {
            let next: Data = lookup.resolve_v1(state)
            Pair(terminal_hash, next)
          }
          Lookup { source, proof, .. } -> {
            let next: Data = lookup.lookup_v1(state, source, proof)
            Pair(terminal_hash, next)
          }
          LookupScan { source, proof, opening, .. } -> {
            let next: Data = lookup.lookup_scan_v1(
              state,
              source,
              proof,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(claimed_grammar_hash, next)
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
