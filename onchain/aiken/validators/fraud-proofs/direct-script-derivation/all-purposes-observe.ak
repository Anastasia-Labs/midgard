use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as worker

/// Walks the observer field in nonempty batches, then hands the exhausted walk
/// to the receive door.
validator main(
  receive_grammar_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<worker.Datum>,
    redeemer: worker.ExecutionRedeemer,
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
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        let advanced = scan.advance_observers_v1(
          state,
          args.opening,
          args.checkpoint_bytes,
          args.witnesses,
          tx.reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let expected: Data = advanced
        and {
          output_hash == if advanced.complete {
            receive_grammar_script_hash
          } else {
            own_hash
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
