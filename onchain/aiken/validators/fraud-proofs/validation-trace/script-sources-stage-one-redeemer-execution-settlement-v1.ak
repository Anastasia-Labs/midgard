use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_sources_redeemer_normalization_v1 as normalization

pub type Datum =
  ct.StepDatum<normalization.ScriptSourcesRedeemerExecutionAttestedStateV1>

pub type ActionV1 {
  SettleExecution {
    input_index: Int,
    output_index: Int,
    envelope: normalization.PreparedScriptSourcesRedeemerEnvelopeV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  deployment_id: ByteArray,
  expected_envelope_binder_script_hash: ScriptHash,
  expected_traversal_normalizer_script_hash: ScriptHash,
  expected_outer_normalizer_script_hash: ScriptHash,
  expected_fold_map_executor_script_hash: ScriptHash,
  expected_finalize_frame_executor_script_hash: ScriptHash,
  expected_award_script_hash: ScriptHash,
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
      ct.Continue(SettleExecution { input_index, output_index, envelope }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        continue(
          computation_thread_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          fn(
            input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            and {
              bytearray.length(deployment_id) == 32,
              bytearray.length(expected_envelope_binder_script_hash) == 28,
              bytearray.length(expected_traversal_normalizer_script_hash) == 28,
              bytearray.length(expected_outer_normalizer_script_hash) == 28,
              bytearray.length(expected_fold_map_executor_script_hash) == 28,
              bytearray.length(expected_finalize_frame_executor_script_hash) == 28,
              normalization.execution_attestation_settlement_is_exact_v1(
                state,
                envelope,
                deployment_id,
                expected_envelope_binder_script_hash,
                expected_traversal_normalizer_script_hash,
                expected_outer_normalizer_script_hash,
                expected_fold_map_executor_script_hash,
                expected_finalize_frame_executor_script_hash,
                input_script_hash,
                expected_award_script_hash,
                output_script_hash,
                output_state,
              ),
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
