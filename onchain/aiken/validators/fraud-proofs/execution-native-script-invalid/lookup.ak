use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate.{
  Lookup, LookupScan, Resolve, indices_v1,
}
use midgard/fraud_proofs/execution_native_script_invalid/lookup

validator main(
  signers_hash: ScriptHash,
  terminal_hash: ScriptHash,
  thread_policy: PolicyId,
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
          Lookup { source, proof, .. } ->
            when lookup.lookup_v1(state, source, proof) is {
              lookup.NativeSource { state: next_state } -> {
                let next: Data = next_state
                Pair(signers_hash, next)
              }
              lookup.NoNativeSource { state: next_state } -> {
                let next: Data = next_state
                Pair(terminal_hash, next)
              }
            }
          LookupScan { .. } -> fail
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
