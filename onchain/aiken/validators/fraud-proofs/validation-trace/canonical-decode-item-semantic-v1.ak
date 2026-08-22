use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/canonical_decode_item_staging_v1 as staging
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine_v1.{
  NoAuxiliaryWitness, ValidationOneStepWitnessV1,
}
use midgard/validation_resolution_v1.{hash_one_step_evidence}
use midgard/validation_semantic_v1.{Datum}

pub type ActionV1 {
  Verify {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  source_binder_script_hash: ScriptHash,
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
      ct.Continue(Verify { input_index, output_index, transition }) ->
        bind_item_source(
          source_binder_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          own_out_ref,
          tx,
        )
    }
  }

  else(_) {
    fail
  }
}

/// Option B (#620): the commitment this stage re-checks is transition-only —
/// `hash_one_step_evidence(transition, NoAuxiliaryWitness)` — so no carriage
/// reaches this stage in any form and it is item-size-independent on every
/// route. Content is proven later, once, at the observe stage's §8.8 field
/// door; what this stage contributes is installing the transition into the
/// typed staging chain under its own script hash, which is the provenance the
/// downstream stages rely on.
fn bind_item_source(
  source_binder_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  transition: ValidationOneStepWitnessV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let transition_data: Data = transition
  let auxiliary_data: Data = NoAuxiliaryWitness
  continue(
    computation_thread_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
    fn(
      _input_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      input_state,
      output_script_hash,
      output_state,
    ) {
      expect Some(base) = input_state
      let expected_output_state: Data = staging.authenticate(base, transition)
      and {
        hash_one_step_evidence(transition_data, auxiliary_data) == base.evidence_hash,
        output_script_hash == source_binder_script_hash,
        output_state == expected_output_state,
      }
    },
  )
}
