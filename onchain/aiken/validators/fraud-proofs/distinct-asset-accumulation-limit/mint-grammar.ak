use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as grammar
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{TerminalV1}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/workers.{
  Datum, GrammarRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{
  mint_field_index, outputs_field_index,
}

/// Opens the mint field once the output field is exhausted and certifies its
/// grammar. A certified nonempty field goes to the mint count; an empty one
/// ends the derivation without a crossing.
validator main(
  mint_hash: ScriptHash,
  terminal_hash: ScriptHash,
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
              outputs_field_index,
              mint_field_index,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
          grammar.Certify { opening, checkpoint_bytes, budget, .. } ->
            fold.certify_v1(
              state,
              mint_field_index,
              opening,
              checkpoint_bytes,
              budget,
              tx.reference_inputs,
              certificate_policy,
            )
        }
        if advanced.cursor.phase == cursor.Certifying {
          let expected: Data = advanced
          output_hash == own_hash && output_state == expected
        } else if advanced.complete {
          let expected: Data = TerminalV1 {
            bound: advanced.cursor.bound,
            crossing: None,
          }
          output_hash == terminal_hash && output_state == expected
        } else {
          let expected: Data = advanced
          output_hash == mint_hash && output_state == expected
        }
      }
    }
  }

  else(_) {
    fail
  }
}
