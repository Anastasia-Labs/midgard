use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_redeemer_normalization_v1 as envelope

pub type Datum =
  ct.StepDatum<envelope.CekContextItemStepPending>

pub type Action {
  BindItem {
    input_index: Int,
    output_index: Int,
    witness: item.RedeemerItemProofWitnessV1,
    current_control_hash: ByteArray,
    next_control_hash: ByteArray,
    family: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

validator main(
  deployment_id: ByteArray,
  traversal_normalizer_script_hash: ScriptHash,
  outer_normalizer_script_hash: ScriptHash,
  semantic_executor_script_hashes: List<ScriptHash>,
  settlement_script_hash: ScriptHash,
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
      ct.Continue(BindItem {
        input_index,
        output_index,
        witness,
        current_control_hash,
        next_control_hash,
        family,
      }) -> {
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
            expect Some(pending) = input_state
            expect
              family >= envelope.fold_map_family && family <= envelope.finish_data_family
            let current: Data = pending.item_control
            let witness_data: Data = witness
            expect
              normalized.raw_data_hash(witness_data) == pending.item_witness_hash
            let next: Data = pending.claimed_next
            let base: Data = pending
            let auxiliary = builtin.constr_data(
              18,
              [builtin.constr_data(1, []), current, witness_data],
            )
            let canonical_auxiliary_hash = envelope.canonical_auxiliary_hash_v1(
              auxiliary,
            )
            let canonical_action_hash = envelope.canonical_item_action_hash(
              current,
              witness_data,
              family,
            )
            let carrier_identity = envelope.carrier_identity(base)
            let next_data_hash = normalized.raw_data_hash(next)
            expect Some(executor) = list.at(
              semantic_executor_script_hashes,
              normalized.executor_index(current, witness_data, family),
            )
            let commitment = envelope.envelope_commitment_v1(
              deployment_id,
              envelope.cek_context_carrier,
              carrier_identity,
              family,
              canonical_auxiliary_hash,
              canonical_action_hash,
              current_control_hash,
              next_control_hash,
              pending.item_control.item_index,
              pending.item_control.item_count,
              input_script_hash,
              traversal_normalizer_script_hash,
              outer_normalizer_script_hash,
              executor,
              settlement_script_hash,
              next_data_hash,
            )
            let expected: Data = envelope.PreparedScriptSourcesRedeemerEnvelopeV1 {
              version: envelope.version,
              domain: envelope.envelope_domain,
              deployment_id,
              carrier: envelope.cek_context_carrier,
              base,
              carrier_identity,
              expected_next_control_data_hash: next_data_hash,
              action_family: family,
              canonical_auxiliary_hash,
              canonical_action_hash,
              current_pending_item_control_hash: current_control_hash,
              expected_next_item_control_hash: next_control_hash,
              redeemer_count: pending.item_control.item_index,
              redeemer_total_count: pending.item_control.item_count,
              envelope_binder_script_hash: input_script_hash,
              traversal_normalizer_script_hash,
              outer_normalizer_script_hash,
              semantic_executor_script_hash: executor,
              settlement_script_hash,
              envelope_commitment: commitment,
            }
            and {
              output_script_hash == traversal_normalizer_script_hash,
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
