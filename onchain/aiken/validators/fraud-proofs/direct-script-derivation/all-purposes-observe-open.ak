use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as worker
use midgard/fraud_proofs/field_opening_v1.{
  mint_field_index, required_observers_field_index,
}

/// Opens the observer field once the mint field is exhausted. The field has a
/// fixed stride, so it walks at once; an empty field goes straight to the
/// receive door.
validator main(
  observe_scan_script_hash: ScriptHash,
  receive_grammar_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<worker.Datum>,
    redeemer: worker.OpenRedeemer,
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
      ct.Continue(worker.OpenArgsV1 { input_index, output_index, opening }) -> {
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
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
        let advanced = scan.open_next_v1(
          state,
          mint_field_index,
          required_observers_field_index,
          opening,
          tx.reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let expected: Data = advanced
        and {
          output_hash == if advanced.complete {
            receive_grammar_script_hash
          } else {
            observe_scan_script_hash
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
