use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/unused_script_witness/walk

/// Takes the completed all-purposes walk and opens field 6 for the witness
/// walk, which starts with grammar certification.
validator main(
  witness_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<walk.EntryDatum>,
    redeemer: walk.EntryRedeemer,
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
        expect Some(state) = input_state
        let expected: Data = walk.open_v1(
          state,
          args,
          tx.reference_inputs,
          certificate_policy,
        )
        and {
          output_hash == witness_grammar_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
