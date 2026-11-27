use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  EntryDatum, EntryRedeemer,
}

/// Opens the spend field over the bound transaction with no entry counted.
/// An empty spend field is complete at opening and goes straight to the
/// output field's grammar door.
validator main(
  inputs_hash: ScriptHash,
  outputs_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<EntryDatum>,
    redeemer: EntryRedeemer,
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
        let _, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(bound) = input_state
        let opened = fold.initial_v1(
          bound,
          args.opening,
          tx.reference_inputs,
          certificate_policy,
        )
        let expected: Data = opened
        and {
          output_hash == if opened.complete {
            outputs_grammar_hash
          } else {
            inputs_hash
          },
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
