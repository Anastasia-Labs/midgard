use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/first_fault_v1 as order
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1 as reason

fn rejected(r: reason.RejectionReasonV1) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 1,
    source_kind: 1,
    transaction_id: #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    source_key: #"00",
    rejection_reason: Some(r),
  }
}

fn point(
  kind: Int,
  raw_index: Int,
  index: Int,
  subcheck: Int,
) -> order.ResolutionPositionV1 {
  order.ResolutionPositionV1 {
    source_kind: kind,
    input_index: raw_index,
    out_ref: #"825820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f19"
      |> bytearray.concat(bytearray.from_int_big_endian(index, 2)),
    subcheck,
  }
}

test canonical_item_precedes_same_field_terminal_length_check() {
  !order.leaf_admits_fault_v1(
    rejected(reason.FieldPreimageLengthMismatch { field_index: 4 }),
    None,
    order.CanonicalFault(
      order.CanonicalItem { field_index: 4, item_index: 200 },
    ),
    order.NotRequired,
  )
}

test canonical_earlier_field_end_precedes_later_field_item() {
  order.leaf_admits_fault_v1(
    rejected(reason.FieldPreimageLengthMismatch { field_index: 3 }),
    None,
    order.CanonicalFault(order.CanonicalItem { field_index: 4, item_index: 0 }),
    order.NotRequired,
  )
}

test resolve_earlier_input_later_subcheck_precedes_later_input_absence() {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 0 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(0, 7, 1, order.spend_signer_subcheck)),
    order.ResolutionReopened(point(1, 0, 2, order.input_absence_subcheck)),
  )
}

test resolve_later_input_earlier_subcheck_cannot_refute_earlier_input() {
  order.leaf_admits_fault_v1(
    rejected(
      reason.ResolvedReferenceScriptDepthLimit {
        source_kind: 1,
        input_index: 7,
      },
    ),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(0, 0, 2, order.input_absence_subcheck)),
    order.ResolutionReopened(
      point(1, 7, 1, order.reference_script_depth_subcheck),
    ),
  )
}

test resolve_same_input_absence_precedes_signer_check() {
  !order.leaf_admits_fault_v1(
    rejected(reason.SpendInputSignerMissing { input_index: 9 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(0, 9, 2, order.input_absence_subcheck)),
    order.ResolutionReopened(point(0, 9, 2, order.spend_signer_subcheck)),
  )
}

test resolve_true_first_fault_is_admitted() {
  let observed = point(1, 8, 3, order.input_absence_subcheck)
  order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 8 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(observed),
    order.ResolutionReopened(observed),
  )
}

test resolve_reference_kind_does_not_override_smaller_spend_outref() {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 0 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(0, 20, 1, order.input_absence_subcheck)),
    order.ResolutionReopened(point(1, 0, 2, order.input_absence_subcheck)),
  )
}

test resolve_raw_index_does_not_override_actual_outref() {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 0 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(1, 200, 1, order.input_absence_subcheck)),
    order.ResolutionReopened(point(0, 0, 2, order.input_absence_subcheck)),
  )
}

test resolve_wrong_claimed_raw_coordinate_is_refused() fail {
  order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 3 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(1, 0, 1, order.input_absence_subcheck)),
    order.ResolutionReopened(point(0, 4, 2, order.input_absence_subcheck)),
  )
}

test resolve_wrong_claimed_subcheck_is_refused() fail {
  order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 3 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(1, 0, 1, order.input_absence_subcheck)),
    order.ResolutionReopened(point(0, 3, 2, order.spend_signer_subcheck)),
  )
}

test resolve_equal_outref_at_distinct_raw_positions_is_refused() fail {
  order.resolution_position_precedes_or_equals_v1(
    point(0, 0, 1, order.input_absence_subcheck),
    point(1, 0, 1, order.output_canonicity_subcheck),
  )
}

test static_rules_precede_input_sets_despite_wire_tag_order() {
  order.phase_of_reason_v1(
    reason.NetworkIdMismatch,
    order.NetworkOrderContextV1 { body_network_mismatch: True },
  ) < order.phase_of_reason_v1(
    reason.EmptyInputs,
    order.NetworkOrderContextV1 { body_network_mismatch: True },
  )
}

test resolve_validity_precedes_any_input_without_claimed_item() {
  order.leaf_admits_fault_v1(
    rejected(reason.ValidityIntervalExcludesBlockSlot),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.NotRequired,
  )
}

test resolve_earlier_phase_is_admitted_without_claimed_item() {
  order.leaf_admits_fault_v1(
    rejected(reason.RequiredSignerUnsigned { signer_index: 0 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.NotRequired,
  )
}

test resolve_later_phase_does_not_admit_earlier_input_fault() {
  !order.leaf_admits_fault_v1(
    rejected(reason.OutputNonCanonical { output_index: 0 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.NotRequired,
  )
}

fn no_network_mismatch() -> Option<order.NetworkOrderContextV1> {
  Some(order.NetworkOrderContextV1 { body_network_mismatch: False })
}

// Callers negate the result, so these rows do too: evidence that is missing,
// unused or for another coordinate must abort rather than return False.
test n7_resolution_leaf_without_claimed_receipt_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 0 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.NotRequired,
  )
}

test n7_unused_resolution_receipt_on_earlier_phase_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.RequiredSignerUnsigned { signer_index: 0 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.ResolutionReopened(point(0, 0, 1, order.input_absence_subcheck)),
  )
}

test n7_out_of_domain_receipt_does_not_admit() {
  !order.leaf_admits_fault_v1(
    rejected(
      reason.ResolvedReferenceScriptNodeLimit { source_kind: 1, input_index: 5 },
    ),
    no_network_mismatch(),
    order.ResolutionFault(point(1, 0, 9, order.spend_signer_subcheck)),
    order.ResolutionOutOfDomain { source_kind: 1, input_index: 5 },
  )
}

test n7_out_of_domain_receipt_for_other_coordinate_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 5 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.ResolutionOutOfDomain { source_kind: 1, input_index: 4 },
  )
}

test n7_out_of_domain_receipt_for_other_field_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.SpendInputSignerMissing { input_index: 5 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.ResolutionOutOfDomain { source_kind: 1, input_index: 5 },
  )
}

test n7_out_of_domain_receipt_on_validity_leaf_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.ValidityIntervalExcludesBlockSlot),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.ResolutionOutOfDomain { source_kind: 0, input_index: 0 },
  )
}

test n7_out_of_domain_receipt_on_later_phase_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.OutputNonCanonical { output_index: 0 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.ResolutionOutOfDomain { source_kind: 0, input_index: 0 },
  )
}

fn purpose(
  kind: Int,
  index: Int,
  subcheck: order.PurposeSubcheckV1,
) -> order.PurposeFaultPositionV1 {
  order.PurposeFaultPositionV1 {
    coordinate: order.PurposeCoordinateV1 { kind, index },
    subcheck,
  }
}

fn existing(kind: Int, index: Int) -> order.ClaimedFaultOrderV1 {
  order.PurposeReopened(
    order.ExistingPurpose {
      coordinate: order.PurposeCoordinateV1 { kind, index },
    },
  )
}

fn purpose_admits(
  r: reason.RejectionReasonV1,
  observed: order.PurposeFaultPositionV1,
  claimed: order.ClaimedFaultOrderV1,
) -> Bool {
  order.leaf_admits_fault_v1(
    rejected(r),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.PurposeFault(observed),
    claimed,
  )
}

test n7_purpose_order_is_per_pointer() {
  and {
    !purpose_admits(
      reason.ScriptSourceMissing { purpose_kind: 0, purpose_index: 1 },
      purpose(0, 0, order.RedeemerRequired),
      existing(0, 1),
    ),
    purpose_admits(
      reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
      purpose(0, 1, order.SourceRequired),
      existing(0, 0),
    ),
  }
}

test n7_purpose_same_pointer_subcheck() {
  and {
    !purpose_admits(
      reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
      purpose(0, 0, order.SourceRequired),
      existing(0, 0),
    ),
    purpose_admits(
      reason.ScriptSourceMissing { purpose_kind: 0, purpose_index: 0 },
      purpose(0, 0, order.RedeemerRequired),
      existing(0, 0),
    ),
    purpose_admits(
      reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
      purpose(0, 0, order.RedeemerRequired),
      existing(0, 0),
    ),
  }
}

test n7_purpose_kind_precedes_index() {
  and {
    order.purpose_position_precedes_or_equals_v1(
      purpose(0, 99, order.RedeemerRequired),
      purpose(1, 0, order.SourceRequired),
    ),
    order.purpose_position_precedes_or_equals_v1(
      purpose(1, 99, order.RedeemerRequired),
      purpose(2, 0, order.SourceRequired),
    ),
    order.purpose_position_precedes_or_equals_v1(
      purpose(2, 99, order.RedeemerRequired),
      purpose(3, 0, order.SourceRequired),
    ),
    !order.purpose_position_precedes_or_equals_v1(
      purpose(3, 0, order.SourceRequired),
      purpose(2, 99, order.RedeemerRequired),
    ),
  }
}

fn impossible_invalid(kind: Int, index: Int) -> Bool {
  let coordinate = order.PurposeCoordinateV1 { kind, index }
  !order.purpose_coordinate_is_valid_v1(coordinate) && !purpose_admits(
    reason.ScriptSourceMissing { purpose_kind: kind, purpose_index: index },
    purpose(0, 0, order.SourceRequired),
    order.PurposeReopened(order.ImpossiblePurpose { coordinate }),
  )
}

test n7_signed_impossible_coordinates_are_preserved() {
  and {
    impossible_invalid(-1, 0),
    impossible_invalid(4, 0),
    impossible_invalid(0, -1),
  }
}

test n7_existing_negative_kind_refuses() fail {
  purpose_admits(
    reason.ScriptSourceMissing { purpose_kind: -1, purpose_index: 0 },
    purpose(0, 0, order.SourceRequired),
    existing(-1, 0),
  )
}

test n7_existing_unknown_kind_refuses() fail {
  purpose_admits(
    reason.ScriptSourceMissing { purpose_kind: 4, purpose_index: 0 },
    purpose(0, 0, order.SourceRequired),
    existing(4, 0),
  )
}

test n7_existing_negative_index_refuses() fail {
  purpose_admits(
    reason.RedeemerMissing { purpose_kind: 0, purpose_index: -1 },
    purpose(0, 0, order.RedeemerRequired),
    existing(0, -1),
  )
}

test n7_invalid_observed_coordinate_refuses_even_for_earlier_leaf() fail {
  purpose_admits(
    reason.EmptyInputs,
    purpose(4, 0, order.SourceRequired),
    order.NotRequired,
  )
}

test n7_missing_claimed_purpose_receipt_refuses() fail {
  purpose_admits(
    reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
    purpose(0, 0, order.SourceRequired),
    order.NotRequired,
  )
}

test n7_resolution_receipt_for_purpose_refuses() fail {
  purpose_admits(
    reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
    purpose(0, 0, order.SourceRequired),
    order.ResolutionReopened(point(0, 0, 1, 0)),
  )
}

test n7_substituted_claimed_purpose_receipt_refuses() fail {
  purpose_admits(
    reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
    purpose(0, 0, order.SourceRequired),
    existing(0, 1),
  )
}

test n7_substituted_impossible_coordinate_refuses() fail {
  purpose_admits(
    reason.ScriptSourceMissing { purpose_kind: -1, purpose_index: 0 },
    purpose(0, 0, order.SourceRequired),
    order.PurposeReopened(
      order.ImpossiblePurpose {
        coordinate: order.PurposeCoordinateV1 { kind: 4, index: 0 },
      },
    ),
  )
}

test n7_unused_claimed_purpose_receipt_refuses() fail {
  purpose_admits(
    reason.EmptyInputs,
    purpose(0, 0, order.SourceRequired),
    existing(0, 0),
  )
}

test n7_accepted_leaf_refuses_unused_receipt() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..rejected(reason.EmptyInputs),
    direction: 0,
    rejection_reason: None,
  }
  order.leaf_admits_fault_v1(
    subject,
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.PurposeFault(purpose(0, 0, order.SourceRequired)),
    existing(0, 0),
  )
}

test n7_accepted_leaf_does_not_admit_purpose_fault() {
  let subject = substrate.VerdictSubjectV1 {
    ..rejected(reason.EmptyInputs),
    direction: 0,
    rejection_reason: None,
  }
  !order.leaf_admits_fault_v1(
    subject,
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.PurposeFault(purpose(0, 0, order.SourceRequired)),
    order.NotRequired,
  )
}

test n7_missing_purpose_context_refuses() fail {
  order.leaf_admits_fault_v1(
    rejected(reason.EmptyInputs),
    None,
    order.PurposeFault(purpose(0, 0, order.SourceRequired)),
    order.NotRequired,
  )
}

test n7_missing_resolution_context_refuses() fail {
  order.leaf_admits_fault_v1(
    rejected(reason.EmptyInputs),
    None,
    order.ResolutionFault(point(0, 0, 1, 0)),
    order.NotRequired,
  )
}

test n7_canonical_context_is_unused_and_refuses() fail {
  order.leaf_admits_fault_v1(
    rejected(reason.FieldItemWidthIllegal { field_index: 0, item_index: 0 }),
    Some(order.NetworkOrderContextV1 { body_network_mismatch: False }),
    order.CanonicalFault(order.CanonicalItem { field_index: 0, item_index: 0 }),
    order.NotRequired,
  )
}

test n7_network_context_distinguishes_body_and_output() {
  let body = order.NetworkOrderContextV1 { body_network_mismatch: True }
  let output = order.NetworkOrderContextV1 { body_network_mismatch: False }
  and {
    order.phase_of_reason_v1(reason.NetworkIdMismatch, body) == 2,
    order.phase_of_reason_v1(reason.NetworkIdMismatch, output) == 8,
    order.leaf_admits_fault_v1(
      rejected(reason.NetworkIdMismatch),
      Some(body),
      order.ResolutionFault(point(0, 0, 1, 0)),
      order.NotRequired,
    ),
    !order.leaf_admits_fault_v1(
      rejected(reason.NetworkIdMismatch),
      Some(output),
      order.ResolutionFault(point(0, 0, 1, 0)),
      order.NotRequired,
    ),
    order.leaf_admits_fault_v1(
      rejected(reason.NetworkIdMismatch),
      Some(body),
      order.PurposeFault(purpose(0, 0, order.SourceRequired)),
      order.NotRequired,
    ),
    purpose_admits(
      reason.NetworkIdMismatch,
      purpose(0, 0, order.SourceRequired),
      order.NotRequired,
    ),
  }
}

test n7_pre_discovery_and_unused_brackets() {
  let observed = purpose(0, 0, order.SourceRequired)
  and {
    purpose_admits(
      reason.RedeemerMalformed { redeemer_index: 0 },
      observed,
      order.NotRequired,
    ),
    purpose_admits(
      reason.OutputNonCanonical { output_index: 0 },
      observed,
      order.NotRequired,
    ),
    purpose_admits(
      reason.OutputReferenceScriptMalformed { output_index: 0 },
      observed,
      order.NotRequired,
    ),
    purpose_admits(
      reason.OutputReferenceScriptNodeLimit { output_index: 0 },
      observed,
      order.NotRequired,
    ),
    purpose_admits(
      reason.OutputReferenceScriptDepthLimit { output_index: 0 },
      observed,
      order.NotRequired,
    ),
    purpose_admits(
      reason.ProtectedOutputSignerMissing { output_index: 0 },
      observed,
      order.NotRequired,
    ),
    purpose_admits(
      reason.MintDeclaredAssetLimit { policy_index: 0 },
      observed,
      order.NotRequired,
    ),
    purpose_admits(
      reason.MintItemMalformed { policy_index: 0 },
      observed,
      order.NotRequired,
    ),
    !purpose_admits(
      reason.UnusedScriptWitness { script_index: 0 },
      observed,
      order.NotRequired,
    ),
    !purpose_admits(
      reason.UnusedRedeemer { redeemer_index: 0 },
      observed,
      order.NotRequired,
    ),
    !purpose_admits(reason.ValueNotPreserved, observed, order.NotRequired),
  }
}

// The same encodings are pinned off-chain in
// demo/midgard-sdk/tests/first-fault-wire.test.ts.
test purpose_coordinate_encodes_as_off_chain() {
  cbor.serialise(order.PurposeCoordinateV1 { kind: 3, index: 7 }) == #"d8799f0307ff"
}

test network_order_context_encodes_as_off_chain() {
  cbor.serialise(order.NetworkOrderContextV1 { body_network_mismatch: True }) == #"d8799fd87a80ff"
}

test n7_out_of_domain_receipt_for_purpose_refuses() fail {
  !purpose_admits(
    reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
    purpose(0, 0, order.SourceRequired),
    order.ResolutionOutOfDomain { source_kind: 0, input_index: 0 },
  )
}

// Callers negate the result, so these rows do too. An out-ref equal to the
// observed one at another raw position is not an earlier position: it aborts.
test n7_equal_outref_at_another_index_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 1 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 10, order.input_absence_subcheck)),
    order.ResolutionReopened(point(0, 1, 10, order.input_absence_subcheck)),
  )
}

test n7_equal_outref_in_another_field_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 0 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 10, order.input_absence_subcheck)),
    order.ResolutionReopened(point(1, 0, 10, order.input_absence_subcheck)),
  )
}

// The reopened point must name the leaf's own sub-check, so a receipt for
// another sub-check of the same input cannot stand in for it.
test n7_reopened_receipt_for_another_subcheck_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 3 }),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.ResolutionReopened(point(0, 3, 2, order.spend_signer_subcheck)),
  )
}

fn script_item(kind: Int) -> reason.RejectionReasonV1 {
  reason.PlutusScriptUndecodable { item_kind: kind, item_index: 0, class: 1 }
}

// A witness script item (kind 0) is decoded in phase 5 and an output reference
// script item (kind 1) in phase 8. Read through the callers' negation, a False
// row convicts the leaf.
test n7_witness_script_item_follows_a_canonical_fault() {
  !order.leaf_admits_fault_v1(
    rejected(script_item(0)),
    None,
    order.CanonicalFault(order.CanonicalItem { field_index: 4, item_index: 0 }),
    order.NotRequired,
  )
}

test n7_witness_script_item_precedes_a_resolution_fault() {
  order.leaf_admits_fault_v1(
    rejected(script_item(0)),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.NotRequired,
  )
}

test n7_output_reference_script_item_follows_a_resolution_fault() {
  !order.leaf_admits_fault_v1(
    rejected(script_item(1)),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.NotRequired,
  )
}

test n7_output_reference_script_item_admits_a_purpose_fault() {
  purpose_admits(
    script_item(1),
    purpose(0, 0, order.SourceRequired),
    order.NotRequired,
  )
}

// No other item kind exists, so it has no phase.
test n7_script_item_of_unknown_kind_has_no_phase() fail {
  order.phase_of_reason_v1(
    script_item(2),
    order.NetworkOrderContextV1 { body_network_mismatch: False },
  ) >= 0
}

test n7_script_item_of_negative_kind_has_no_phase() fail {
  order.phase_of_reason_v1(
    script_item(-1),
    order.NetworkOrderContextV1 { body_network_mismatch: False },
  ) >= 0
}

test n7_script_item_of_unknown_kind_refuses() fail {
  !order.leaf_admits_fault_v1(
    rejected(script_item(2)),
    no_network_mismatch(),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    order.NotRequired,
  )
}
