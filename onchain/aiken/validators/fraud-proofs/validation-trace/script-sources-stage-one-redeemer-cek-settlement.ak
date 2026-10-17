use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_redeemer_normalization_v1 as envelope

pub type Datum =
  ct.StepDatum<envelope.ScriptSourcesRedeemerExecutionAttestedStateV1>

pub type Action {
  ReturnItem {
    input_index: Int,
    output_index: Int,
    original_envelope: envelope.PreparedScriptSourcesRedeemerEnvelopeV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

validator main(
  deployment_id: ByteArray,
  expected_traversal_normalizer_script_hash: ScriptHash,
  expected_outer_normalizer_script_hash: ScriptHash,
  expected_executor_script_hashes: List<ScriptHash>,
  return_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(ReturnItem { input_index, output_index, original_envelope }) -> {
        expect Some(step_datum) = datum
        continue(
          computation_thread_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(
            input_script_hash,
            _token_name,
            _prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            expect original_envelope.carrier == envelope.cek_context_carrier
            expect
              original_envelope.action_family <= envelope.finish_data_family
            expect pending: envelope.CekContextItemStepPending = original_envelope.base
            let next_data: Data = pending.claimed_next
            expect
              normalized.raw_data_hash(next_data) == original_envelope.expected_next_control_data_hash
            let expected: Data = envelope.CekContextItemStepVerified {
              staged: pending.staged,
              next: pending.claimed_next,
            }
            and {
              envelope.execution_attestation_is_bound_to_envelope_v1(
                state,
                original_envelope,
                deployment_id,
                state.envelope_binder_script_hash,
                expected_traversal_normalizer_script_hash,
                expected_outer_normalizer_script_hash,
                expected_executor_script_hashes,
                input_script_hash,
              ),
              output_script_hash == return_script_hash,
              output_state == expected,
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
