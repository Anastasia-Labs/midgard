use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/execution_native_script_invalid/spend_rank

validator main(
  terminal_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<spend_rank.Datum>,
    redeemer: spend_rank.SpendRedeemer,
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
        let Pair(input_index, output_index) = when action is {
          workers.Scan { args } -> Pair(args.input_index, args.output_index)
          workers.Finish { input_index, output_index } ->
            Pair(input_index, output_index)
        }
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
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
          workers.Scan { args } -> {
            let next: Data = spend_rank.advance_v1(
              state,
              args,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(own_hash, next)
          }
          workers.Finish { .. } -> {
            let next: Data = spend_rank.finish_v1(state)
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
