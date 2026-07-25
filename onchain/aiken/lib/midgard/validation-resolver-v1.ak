use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/validation_machine_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_resolution_v1.ValidationResolutionStateV1>

pub type ResolveActionV1 {
  Resolve {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    challenger_evidence: validation_machine_v1.ValidationOneStepEvidenceV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ResolveActionV1>

pub type PrepareActionV1 {
  Prepare {
    input_index: Int,
    output_index: Int,
    transition: validation_machine_v1.ValidationOneStepWitnessV1,
    auxiliary: Data,
  }
}

pub type PrepareSpendRedeemer =
  ct.StepRedeemer<PrepareActionV1>

pub type PrepareSelectedActionV1 {
  PrepareSelected {
    input_index: Int,
    output_index: Int,
    semantic_resolver_index: Int,
    transition: validation_machine_v1.ValidationOneStepWitnessV1,
    auxiliary: Data,
  }
}

pub type PrepareSelectedSpendRedeemer =
  ct.StepRedeemer<PrepareSelectedActionV1>

pub fn prepare(
  expected_phase: validation_trace_v1.ValidationPhase,
  semantic_resolver_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  redeemer: PrepareSpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
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
    ct.Continue(Prepare { input_index, output_index, transition, auxiliary }) -> {
      let Transaction { inputs, outputs, .. } = tx
      expect Some(step_datum) = datum
      continue(
        computation_thread_token_policy_id,
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
          let expected_output_state: Data = validation_resolution_v1.prepare_semantic_resolution(
            state,
            transition,
            auxiliary,
          )
          and {
            state.pre_state.phase == expected_phase,
            output_script_hash == semantic_resolver_script_hash,
            output_state == expected_output_state,
          }
        },
      )
    }
  }
}

/// Routes one authenticated phase-level boundary to exactly one narrow
/// semantic proof family. The selector is intentionally not part of the
/// evidence hash: the selected validator reconstructs and authenticates the
/// complete transition/auxiliary evidence, so choosing the wrong family
/// cannot produce a winning resolution.
pub fn prepare_selected(
  expected_phase: validation_trace_v1.ValidationPhase,
  semantic_resolver_script_hashes: List<ScriptHash>,
  expected_semantic_resolver_count: Int,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  redeemer: PrepareSelectedSpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
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
    ct.Continue(PrepareSelected {
      input_index,
      output_index,
      semantic_resolver_index,
      transition,
      auxiliary,
    }) -> {
      let Transaction { inputs, outputs, .. } = tx
      expect Some(step_datum) = datum
      expect Some(semantic_resolver_script_hash) = list.at(
        semantic_resolver_script_hashes,
        semantic_resolver_index,
      )
      continue(
        computation_thread_token_policy_id,
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
          let expected_output_state: Data = validation_resolution_v1.prepare_semantic_resolution(
            state,
            transition,
            auxiliary,
          )
          and {
            expected_semantic_resolver_count > 0,
            list.length(semantic_resolver_script_hashes) == expected_semantic_resolver_count,
            semantic_resolver_index >= 0,
            semantic_resolver_index < expected_semantic_resolver_count,
            state.pre_state.phase == expected_phase,
            output_script_hash == semantic_resolver_script_hash,
            output_state == expected_output_state,
          }
        },
      )
    }
  }
}

pub fn validate(
  expected_phase: validation_trace_v1.ValidationPhase,
  verify_one_step: fn(
    validation_trace_v1.ValidationMachineStateV1,
    validation_machine_v1.ValidationOneStepEvidenceV1,
  ) ->
    Bool,
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  datum: Option<Datum>,
  redeemer: SpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
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
    ct.Continue(Resolve {
      input_index,
      output_index,
      fraud_proof_mint_redeemer_index,
      challenger_evidence,
    }) -> {
      let Transaction { inputs, outputs, redeemers, .. } = tx
      expect Some(step_datum) = datum
      finalize(
        computation_thread_token_policy_id,
        fraud_proof_token_policy_id,
        fraud_proof_token_address,
        step_datum,
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        own_out_ref,
        inputs,
        outputs,
        redeemers,
        fn(
          _input_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          input_state,
        ) {
          expect Some(state) = input_state
          and {
            state.pre_state.phase == expected_phase,
            validation_resolution_v1.challenger_wins_with_valid_successor(
              state,
              challenger_evidence.transition.claimed_successor,
              verify_one_step(state.pre_state, challenger_evidence),
            ),
          }
        },
      )
    }
  }
}
