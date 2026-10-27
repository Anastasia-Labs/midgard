use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{
  FieldDone, FieldWalked,
}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  Datum, FieldRedeemer,
}

/// Counts outputs with a bounded budget of work. A crossing goes to the
/// terminal; an exhausted output field goes to the mint field's grammar door.
validator main(
  mint_grammar_hash: ScriptHash,
  terminal_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: FieldRedeemer,
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
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        when
          fold.advance_outputs_v1(
            state,
            args.opening,
            args.checkpoint_bytes,
            args.budget,
            tx.reference_inputs,
            certificate_policy,
          )
        is {
          FieldDone(done) -> {
            let expected: Data = done
            output_hash == terminal_hash && output_state == expected
          }
          FieldWalked(walked) -> {
            let expected: Data = walked
            and {
              output_hash == if walked.complete {
                mint_grammar_hash
              } else {
                own_hash
              },
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
