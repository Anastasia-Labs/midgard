//// Claim custody start for `missingRedeemer`. The leaf's own purpose
//// coordinate is reopened in its authenticated domain. Leaf integers select
//// evidence; only authenticated domain facts create a purpose fact.

use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  PurposeCoordinateV1,
} as seam
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ClaimPurposeStateV1, ClaimedPurposeFactV1, ExistingPurpose, ImpossiblePurpose,
  NotRequired,
}
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}

pub type StartActionV1 {
  Complete { input_index: Int, output_index: Int }
  Spend {
    input_index: Int,
    output_index: Int,
    raw_item_index: Int,
    opening: opening.FieldOpeningV1,
    descriptor_cbor: ByteArray,
    membership: MembershipCarriage,
  }
  SpendOutOfRange {
    input_index: Int,
    output_index: Int,
    opening: opening.FieldOpeningV1,
  }
  Observe {
    input_index: Int,
    output_index: Int,
    opening: opening.FieldOpeningV1,
  }
  Mint { input_index: Int, output_index: Int, opening: opening.FieldOpeningV1 }
  Receive {
    input_index: Int,
    output_index: Int,
    opening: opening.FieldOpeningV1,
  }
}

pub type FinishOpeningArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: opening.FieldOpeningV1,
  checkpoint_bytes: ByteArray,
}

pub type StartDatum =
  ct.StepDatum<ClaimContextV1>

pub type StartRedeemer =
  ct.StepRedeemer<StartActionV1>

pub fn start_indices_v1(action: StartActionV1) -> Pair<Int, Int> {
  when action is {
    Complete { input_index, output_index } -> Pair(input_index, output_index)
    Spend { input_index, output_index, .. } -> Pair(input_index, output_index)
    SpendOutOfRange { input_index, output_index, .. } ->
      Pair(input_index, output_index)
    Observe { input_index, output_index, .. } -> Pair(input_index, output_index)
    Mint { input_index, output_index, .. } -> Pair(input_index, output_index)
    Receive { input_index, output_index, .. } -> Pair(input_index, output_index)
  }
}

/// The coordinate named by the authenticated leaf, if its arm names one.
pub fn coordinate_v1(context: ClaimContextV1) -> Option<PurposeCoordinateV1> {
  when context.bound.bound.subject.rejection_reason is {
    None -> None
    Some(reason) -> seam.purpose_coordinate_of_reason_v1(reason)
  }
}

/// A valid coordinate of `kind` that differs from the observed route's own
/// coordinate. The observed coordinate is reused through `complete_v1`.
pub fn distinct_coordinate_v1(
  context: ClaimContextV1,
  kind: Int,
) -> PurposeCoordinateV1 {
  expect Some(coordinate) = coordinate_v1(context)
  expect and {
      seam.purpose_coordinate_is_valid_v1(coordinate),
      coordinate.kind == kind,
      when context.observed is {
        None -> True
        Some(observed) ->
          coordinate != observed.authenticated.selection.coordinate
      },
    }
  coordinate
}

/// Callers are the domain finish functions, never a redeemer-supplied fact.
/// The complete Bound comparison includes source identity and event pre-root.
pub fn finish_v1(
  context: ClaimContextV1,
  reopened: BoundIndexV1,
  fact: ClaimedPurposeFactV1,
) -> ClaimPurposeStateV1 {
  expect reopened == context.bound
  expect Some(claimed) = coordinate_v1(context)
  let coordinate = when fact is {
    ExistingPurpose { purpose } -> {
      expect seam.purpose_coordinate_is_valid_v1(purpose.coordinate)
      purpose.coordinate
    }
    ImpossiblePurpose { coordinate } -> coordinate
    NotRequired -> fail
  }
  expect coordinate == claimed
  ClaimPurposeStateV1 { context, purpose: fact }
}

/// No coordinate, an invalid signed coordinate, or the observed route's own
/// coordinate. A claimed-only context has no observation to reuse.
pub fn complete_v1(context: ClaimContextV1) -> ClaimPurposeStateV1 {
  when coordinate_v1(context) is {
    None -> ClaimPurposeStateV1 { context, purpose: NotRequired }
    Some(coordinate) -> {
      let fact = if !seam.purpose_coordinate_is_valid_v1(coordinate) {
        ImpossiblePurpose { coordinate }
      } else {
        expect Some(observed) = context.observed
        let selection = observed.authenticated.selection
        expect Some(actual) = selection.purpose
        expect and {
            coordinate == selection.coordinate,
            actual.coordinate == coordinate,
          }
        ExistingPurpose { purpose: actual }
      }
      finish_v1(context, context.bound, fact)
    }
  }
}

pub fn spend_out_of_range_v1(
  context: ClaimContextV1,
  field_opening: opening.FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ClaimPurposeStateV1 {
  let coordinate = distinct_coordinate_v1(context, purpose.spend)
  let view = opening.opened_field_view(
    field_opening,
    cursor.anchor_v1(context.bound.bound, opening.spend_inputs_field_index),
    opening.spend_inputs_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  expect coordinate.index >= field_item_count(view)
  finish_v1(context, context.bound, ImpossiblePurpose { coordinate })
}

/// Field 3 has a fixed stride: its authenticated full view carries the complete
/// item count, so even empty-domain absence needs no invented scan checkpoint.
pub fn observe_v1(
  context: ClaimContextV1,
  field_opening: opening.FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ClaimPurposeStateV1 {
  let coordinate = distinct_coordinate_v1(context, purpose.observe)
  let view = opening.opened_field_view(
    field_opening,
    cursor.anchor_v1(
      context.bound.bound,
      opening.required_observers_field_index,
    ),
    opening.required_observers_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  let fact = if coordinate.index >= field_item_count(view) {
    ImpossiblePurpose { coordinate }
  } else {
    let script_hash = field_item_at(view, coordinate.index)
    expect bytearray.length(script_hash) == 28
    ExistingPurpose {
      purpose: PurposeV1 { coordinate, script_hash, subject: script_hash },
    }
  }
  finish_v1(context, context.bound, fact)
}
