//// Family entry for `executionNativeScriptInvalid`. The shared source graph
//// hands over its exact binding. The prover either selects one purpose
//// through the shared purpose route, or proves that the coordinate a leaf
//// records as `ExecutionNativeScriptFalse` has no native execution: an
//// invalid coordinate, an observer or spend ordinal past the end of its
//// field, or a key-locked spend (opened and ranked by `spend_rank`). A mint or receive
//// ordinal past its domain is decided by the shared route's own exhaustion.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1} as dd_types
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  FinalV1, NoExecution,
}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, opened_field_view, required_observers_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/first_fault_v1.{
  PurposeCoordinateV1, purpose_coordinate_is_valid_v1,
}
use midgard/native_tx_field_access_v1.{field_item_count}
use midgard/rejection_reason_v1.{ExecutionNativeScriptFalse}

pub type ActionV1 {
  Observe { input_index: Int, output_index: Int }
  Contradict { input_index: Int, output_index: Int }
  ObserveAbsent { input_index: Int, output_index: Int, opening: FieldOpeningV1 }
  SpendAbsent { input_index: Int, output_index: Int, opening: FieldOpeningV1 }
  SpendKey { input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<BoundIndexV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

pub fn indices_v1(action: ActionV1) -> Pair<Int, Int> {
  when action is {
    Observe { input_index, output_index } -> Pair(input_index, output_index)
    Contradict { input_index, output_index } -> Pair(input_index, output_index)
    ObserveAbsent { input_index, output_index, .. } ->
      Pair(input_index, output_index)
    SpendAbsent { input_index, output_index, .. } ->
      Pair(input_index, output_index)
    SpendKey { input_index, output_index } -> Pair(input_index, output_index)
  }
}

/// The coordinate the leaf records. Every route that proves an absence
/// starts here, so it only ever speaks about the recorded coordinate.
pub fn recorded_coordinate_v1(bound: BoundV1) -> PurposeCoordinateV1 {
  expect Some(ExecutionNativeScriptFalse { purpose_kind, purpose_index }) = bound.subject.rejection_reason
  PurposeCoordinateV1 { kind: purpose_kind, index: purpose_index }
}

fn absent_v1(bound: BoundV1, coordinate: PurposeCoordinateV1) -> FinalV1 {
  FinalV1 { bound, fact: NoExecution { coordinate } }
}

/// A recorded coordinate that is no purpose coordinate at all.
pub fn contradiction_v1(state: BoundIndexV1) -> FinalV1 {
  let coordinate = recorded_coordinate_v1(state.bound)
  expect !purpose_coordinate_is_valid_v1(coordinate)
  absent_v1(state.bound, coordinate)
}

/// Field 3 has a fixed stride, so its authenticated view carries the
/// complete observer count.
pub fn observe_absent_v1(
  state: BoundIndexV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FinalV1 {
  let coordinate = recorded_coordinate_v1(state.bound)
  let view = opened_field_view(
    opening,
    cursor.anchor_v1(state.bound, required_observers_field_index),
    required_observers_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  expect and {
      coordinate.kind == purpose.observe,
      coordinate.index >= field_item_count(view),
    }
  absent_v1(state.bound, coordinate)
}

/// Field 0 has a fixed stride, so its authenticated view carries the
/// complete spend count.
pub fn spend_absent_v1(
  state: BoundIndexV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> FinalV1 {
  let coordinate = recorded_coordinate_v1(state.bound)
  let view = opened_field_view(
    opening,
    cursor.anchor_v1(state.bound, spend_inputs_field_index),
    spend_inputs_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  expect and {
      coordinate.kind == purpose.spend,
      coordinate.index >= field_item_count(view),
    }
  absent_v1(state.bound, coordinate)
}
