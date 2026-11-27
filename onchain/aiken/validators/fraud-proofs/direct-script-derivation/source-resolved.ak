use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/source_scan as scan
use midgard/fraud_proofs/direct_script_derivation/source_workers as worker

validator main(
  authenticate_script_hash: ScriptHash,
  input_finish_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<scan.StateV1>>,
    redeemer: ct.StepRedeemer<worker.ResolveAction>,
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
        let Pair(input_index, output_index) = worker.resolve_indices(args)
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
        when args is {
          worker.ResolveScan { opening, checkpoint, budget, .. } -> {
            let resolved = scan.select_resolved_inputs_v1(
              state,
              opening,
              checkpoint,
              budget,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let expected: Data = worker.ResolvedStateV1 {
              resume_script_hash: own_hash,
              resolved,
            }
            output_hash == authenticate_script_hash && output_state == expected
          }
          worker.Advance { .. } -> {
            expect state.complete
            let expected: Data = worker.ResumeStateV1 {
              resume_script_hash: own_hash,
              state,
            }
            output_hash == input_finish_script_hash && output_state == expected
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
