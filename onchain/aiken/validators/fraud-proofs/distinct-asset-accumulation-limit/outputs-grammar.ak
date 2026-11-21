use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as grammar
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  Datum, GrammarRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{
  outputs_field_index, spend_inputs_field_index,
}

/// Opens the output field once the spend field is exhausted and certifies
/// its grammar. A certified nonempty field goes to the output count; an
/// empty one goes to the mint field's grammar door.
validator main(
  outputs_hash: ScriptHash,
  mint_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: GrammarRedeemer,
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
        let Pair(input_index, output_index) = grammar.grammar_indices_v1(action)
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
        let advanced = when action is {
          grammar.Open { opening, .. } ->
            fold.open_next_v1(
              state,
              spend_inputs_field_index,
              outputs_field_index,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
          grammar.Certify { opening, checkpoint_bytes, budget, .. } ->
            fold.certify_v1(
              state,
              outputs_field_index,
              opening,
              checkpoint_bytes,
              budget,
              tx.reference_inputs,
              certificate_policy,
            )
        }
        let expected: Data = advanced
        and {
          output_hash == if advanced.cursor.phase == cursor.Certifying {
            own_hash
          } else if advanced.complete {
            mint_grammar_hash
          } else {
            outputs_hash
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
