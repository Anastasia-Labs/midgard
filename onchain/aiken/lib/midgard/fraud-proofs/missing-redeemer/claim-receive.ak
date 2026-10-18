//// Receive claims keep the leaf ordinal through authenticated output discovery
//// and the sorted unique credential declaration. Counts are never output indices.

use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_declaration as declaration
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, outputs_field_index}
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ClaimPurposeStateV1, ExistingPurpose, ImpossiblePurpose,
}
use midgard/fraud_proofs/missing_redeemer/claim_start.{FinishOpeningArgsV1}

/// `target_ordinal` is the leaf's receive ordinal, derived once at the claim
/// start and checked against the leaf again by `finish_declaration_v1`.
pub type OutputStateV1 {
  context: ClaimContextV1,
  target_ordinal: Int,
  scan: receive.OutputStateV1,
}

pub type DeclarationStateV1 {
  context: ClaimContextV1,
  scan: receive.DeclarationStateV1,
}

pub type OutputActionV1 {
  Scan {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    witnesses: List<receive.OutputWitnessV1>,
  }
  /// Hands the completed scan to the receive finish role unchanged.
  Finish { input_index: Int, output_index: Int }
}

pub type OutputDatum =
  ct.StepDatum<OutputStateV1>

pub type DeclarationDatum =
  ct.StepDatum<DeclarationStateV1>

pub type GrammarRedeemer =
  ct.StepRedeemer<workers.ProgressArgsV1>

pub type OutputRedeemer =
  ct.StepRedeemer<OutputActionV1>

pub type FinishRedeemer =
  ct.StepRedeemer<FinishOpeningArgsV1>

pub type DeclarationRedeemer =
  ct.StepRedeemer<declaration.Action>

pub fn output_indices_v1(action: OutputActionV1) -> Pair<Int, Int> {
  when action is {
    Scan { input_index, output_index, .. } -> Pair(input_index, output_index)
    Finish { input_index, output_index } -> Pair(input_index, output_index)
  }
}

pub fn initial_v1(
  context: ClaimContextV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> OutputStateV1 {
  let target_ordinal = claim_start.distinct_coordinate_v1(
    context,
    purpose.receive,
  ).index
  let Pair(scan, _) = receive.initial_outputs_v1(
    context.bound,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  OutputStateV1 { context, target_ordinal, scan }
}

pub fn certify_v1(
  state: OutputStateV1,
  args: workers.ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> OutputStateV1 {
  expect and {
      !state.scan.complete,
      state.scan.credential_count == 0,
      state.scan.credential_root == env.empty_merkle_tree_root,
      args.budget <= workers.scan_batch,
    }
  let (field_cursor, _, complete) = cursor.certify_with_completion_at_v1(
    state.scan.cursor,
    outputs_field_index,
    args.opening,
    args.checkpoint_bytes,
    args.budget,
    reference_inputs,
    certificate_policy_id,
  )
  OutputStateV1 {
    ..state,
    scan: receive.OutputStateV1 { ..state.scan, cursor: field_cursor, complete },
  }
}

pub fn advance_outputs_v1(
  state: OutputStateV1,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<receive.OutputWitnessV1>,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> OutputStateV1 {
  // Every witness consumes one authenticated output, so a page is bounded by
  // the field itself and the step budget, as in the derivation's own scan.
  let Pair(scan, _) = receive.advance_outputs_v1(
    state.scan,
    opening,
    checkpoint_bytes,
    witnesses,
    reference_inputs,
    certificate_policy_id,
  )
  OutputStateV1 { ..state, scan }
}

/// The declaration state copies the completed bound and source index, which
/// `finish_declaration_v1` compares with the claim context through
/// `claim_start.finish_v1`.
pub fn finish_outputs_v1(
  state: OutputStateV1,
  args: FinishOpeningArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> DeclarationStateV1 {
  let completed = receive.finish_outputs_v1(
    state.scan,
    args.opening,
    args.checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  DeclarationStateV1 {
    context: state.context,
    scan: receive.initial_declaration_v1(completed, Some(state.target_ordinal)),
  }
}

pub fn advance_declaration_v1(
  state: DeclarationStateV1,
  entries: List<receive.DeclaredCredentialV1>,
) -> DeclarationStateV1 {
  expect list.length(entries) <= workers.scan_batch
  DeclarationStateV1 {
    ..state,
    scan: receive.advance_declaration_v1(state.scan, entries),
  }
}

pub fn finish_declaration_v1(state: DeclarationStateV1) -> ClaimPurposeStateV1 {
  let coordinate = claim_start.distinct_coordinate_v1(
    state.context,
    purpose.receive,
  )
  expect state.scan.target_ordinal == Some(coordinate.index)
  let selected = receive.selected_purpose_v1(state.scan)
  let receipt = when selected is {
    Some(actual) -> {
      expect and {
          actual.purpose.coordinate == coordinate,
          coordinate.index < state.scan.declared_count,
        }
      ExistingPurpose { purpose: actual.purpose }
    }
    None -> {
      expect coordinate.index >= state.scan.declared_count
      ImpossiblePurpose { coordinate }
    }
  }
  claim_start.finish_v1(
    state.context,
    BoundIndexV1 {
      bound: state.scan.bound,
      source_index: state.scan.source_index,
    },
    receipt,
  )
}
