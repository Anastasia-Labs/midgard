use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as worker
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/field_opening_v1.{
  mint_field_index, spend_inputs_field_index,
}

/// Opens the mint field once the spend field is exhausted and certifies its
/// grammar. A certified nonempty field goes to the mint walk; an empty one
/// goes straight to the observer field's
/// opening.
validator main(
  mint_scan_script_hash: ScriptHash,
  observe_open_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<worker.Datum>,
    redeemer: worker.GrammarRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) -> {
        let Pair(input_index, output_index) = worker.grammar_indices_v1(action)
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        let advanced = when action is {
          worker.Open { opening, .. } ->
            scan.open_next_v1(
              state,
              spend_inputs_field_index,
              mint_field_index,
              opening,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
          worker.Certify { opening, checkpoint_bytes, budget, .. } ->
            scan.certify_v1(
              state,
              mint_field_index,
              opening,
              checkpoint_bytes,
              budget,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
        }
        let expected: Data = advanced
        and {
          output_hash == if advanced.cursor.phase == cursor.Certifying {
            own_hash
          } else if advanced.complete {
            observe_open_script_hash
          } else {
            mint_scan_script_hash
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
