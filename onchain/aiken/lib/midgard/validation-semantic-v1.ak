use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{continue}
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_resolution_v1.PreparedValidationResolutionStateV1>

pub fn continue_winning(
  expected_phase: validation_trace_v1.ValidationPhase,
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  transition: ValidationOneStepWitnessV1,
  auxiliary: Data,
  semantic_transition_is_valid: Bool,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let transition_data: Data = transition
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
      let expected_output_state: Data = validation_resolution_v1.winning_resolution()
      and {
        validation_resolution_v1.prepared_resolution_is_well_formed(state),
        state.resolution.pre_state.phase == expected_phase,
        validation_resolution_v1.hash_one_step_evidence(
          transition_data,
          auxiliary,
        ) == state.evidence_hash,
        semantic_transition_is_valid,
        output_script_hash == award_script_hash,
        output_state == expected_output_state,
      }
    },
  )
}

/// `continue_winning`, with the semantic predicate taken as a function of the
/// prepared resolution's pre-state instead of as an already-computed `Bool`.
///
/// The two forms decide exactly the same thing. `continue`'s `input_state` is
/// the validator's own `datum` — `validate_input_and_provide_output`
/// destructures the very `step_datum` argument this function was handed — so
/// `state.resolution.pre_state` here is the same value a caller of
/// `continue_winning` obtains by re-matching `datum` itself before the call.
/// The predicate is still evaluated in the same conjunct, in the same
/// position, after the same three guards.
///
/// What changes is cost: a caller of `continue_winning` decodes the step datum
/// twice, once to reach the pre-state for its predicate and once inside
/// `continue`. Taking the predicate as a continuation drops the first decode.
/// The saving is small per validator and it is publication margin on a
/// consensus limit, which is why the near-limit validation-trace resolvers use
/// this form.
pub fn continue_winning_at(
  expected_phase: validation_trace_v1.ValidationPhase,
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  transition: ValidationOneStepWitnessV1,
  auxiliary: Data,
  semantic_transition_is_valid: fn(validation_trace_v1.ValidationMachineStateV1) ->
    Bool,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let transition_data: Data = transition
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
      let expected_output_state: Data = validation_resolution_v1.winning_resolution()
      and {
        validation_resolution_v1.prepared_resolution_is_well_formed(state),
        state.resolution.pre_state.phase == expected_phase,
        validation_resolution_v1.hash_one_step_evidence(
          transition_data,
          auxiliary,
        ) == state.evidence_hash,
        semantic_transition_is_valid(state.resolution.pre_state),
        output_script_hash == award_script_hash,
        output_state == expected_output_state,
      }
    },
  )
}
