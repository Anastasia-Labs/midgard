use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine_v1.{
  NoAuxiliaryWitness, ValidationOneStepWitnessV1, structural_transition_is_valid,
}
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, hash_one_step_evidence,
  prepared_resolution_version, state_is_well_formed,
}
use midgard/validation_resolver_v1.{Datum}
use midgard/validation_trace_v1.{CanonicalDecode, hash_machine_state}

pub type ActionV1 {
  PrepareSelected {
    input_index: Int,
    output_index: Int,
    semantic_resolver_index: Int,
    transition: ValidationOneStepWitnessV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

/// Option B (#620): the canonical-decode preparation commits to the transition
/// alone — `hash_one_step_evidence(transition, NoAuxiliaryWitness)`, computed
/// on-chain. The delivery wrapper is no longer part of the committed preimage,
/// so the carriage tier stays free until the observe stage dereferences it and
/// the §8.8 field door there is the sole content gate. The retired
/// `PrepareSelectedByEvidenceHash` arm existed only to keep auxiliary bytes out
/// of the prepare transaction; with the transition never carrying field
/// preimage bytes, the on-chain hash is cheap and a second arm would be a
/// dead-arm hazard (9f191e9a).
fn prepare_selected(
  semantic_resolver_script_hashes: List<ScriptHash>,
  computation_thread_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  semantic_resolver_index: Int,
  transition: ValidationOneStepWitnessV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  expect Some(semantic_resolver_script_hash) = list.at(
    semantic_resolver_script_hashes,
    semantic_resolver_index,
  )
  let transition_data: Data = transition
  let auxiliary_data: Data = NoAuxiliaryWitness
  let evidence_hash = hash_one_step_evidence(transition_data, auxiliary_data)
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
      expect Some(state) = input_state
      expect state_is_well_formed(state)
      expect
        hash_machine_state(transition.claimed_successor) == state.challenger_successor_hash
      expect structural_transition_is_valid(state.pre_state, transition)
      let expected_output_state: Data = PreparedValidationResolutionStateV1 {
        version: prepared_resolution_version,
        resolution: state,
        evidence_hash,
      }
      and {
        list.length(semantic_resolver_script_hashes) == 2,
        semantic_resolver_index >= 0,
        semantic_resolver_index < 2,
        state.pre_state.phase == CanonicalDecode,
        output_script_hash == semantic_resolver_script_hash,
        output_state == expected_output_state,
      }
    },
  )
}

validator main(
  semantic_resolver_script_hashes: List<ScriptHash>,
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
      ct.Continue(PrepareSelected {
        input_index,
        output_index,
        semantic_resolver_index,
        transition,
      }) ->
        prepare_selected(
          semantic_resolver_script_hashes,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          semantic_resolver_index,
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
