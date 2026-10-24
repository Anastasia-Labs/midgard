use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/script_integrity_hash_mismatch/rule.{
  decide_integrity_v1, fold_next_language_v1,
}
use midgard/fraud_proofs/script_integrity_hash_mismatch/step_04.{
  Args, Datum, SpendRedeemer,
}

validator main(step_05_hash: ScriptHash, thread_policy: PolicyId) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
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
      ct.Continue(args) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        let next = fold_next_language_v1(state)
        if next.cursor == 2 {
          let expected: Data = decide_integrity_v1(next)
          and {
            output_hash == step_05_hash,
            output_state == expected,
          }
        } else {
          let expected: Data = next
          and {
            output_hash == own_hash,
            output_state == expected,
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
