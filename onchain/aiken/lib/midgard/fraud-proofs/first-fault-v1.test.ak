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
    order.CanonicalFault(
      order.CanonicalItem { field_index: 4, item_index: 200 },
    ),
    None,
  )
}

test canonical_earlier_field_end_precedes_later_field_item() {
  order.leaf_admits_fault_v1(
    rejected(reason.FieldPreimageLengthMismatch { field_index: 3 }),
    order.CanonicalFault(order.CanonicalItem { field_index: 4, item_index: 0 }),
    None,
  )
}

test resolve_earlier_input_later_subcheck_precedes_later_input_absence() {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 0 }),
    order.ResolutionFault(point(0, 7, 1, order.spend_signer_subcheck)),
    Some(point(1, 0, 2, order.input_absence_subcheck)),
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
    order.ResolutionFault(point(0, 0, 2, order.input_absence_subcheck)),
    Some(point(1, 7, 1, order.reference_script_depth_subcheck)),
  )
}

test resolve_same_input_absence_precedes_signer_check() {
  !order.leaf_admits_fault_v1(
    rejected(reason.SpendInputSignerMissing { input_index: 9 }),
    order.ResolutionFault(point(0, 9, 2, order.input_absence_subcheck)),
    Some(point(0, 9, 2, order.spend_signer_subcheck)),
  )
}

test resolve_true_first_fault_is_admitted() {
  let observed = point(1, 8, 3, order.input_absence_subcheck)
  order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 8 }),
    order.ResolutionFault(observed),
    Some(observed),
  )
}

test resolve_reference_kind_does_not_override_smaller_spend_outref() {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 1, input_index: 0 }),
    order.ResolutionFault(point(0, 20, 1, order.input_absence_subcheck)),
    Some(point(1, 0, 2, order.input_absence_subcheck)),
  )
}

test resolve_raw_index_does_not_override_actual_outref() {
  !order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 0 }),
    order.ResolutionFault(point(1, 200, 1, order.input_absence_subcheck)),
    Some(point(0, 0, 2, order.input_absence_subcheck)),
  )
}

test resolve_wrong_claimed_raw_coordinate_is_refused() fail {
  order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 3 }),
    order.ResolutionFault(point(1, 0, 1, order.input_absence_subcheck)),
    Some(point(0, 4, 2, order.input_absence_subcheck)),
  )
}

test resolve_wrong_claimed_subcheck_is_refused() fail {
  order.leaf_admits_fault_v1(
    rejected(reason.InputNotFound { source_kind: 0, input_index: 3 }),
    order.ResolutionFault(point(1, 0, 1, order.input_absence_subcheck)),
    Some(point(0, 3, 2, order.spend_signer_subcheck)),
  )
}

test resolve_equal_outref_at_distinct_raw_positions_is_refused() fail {
  order.resolution_position_precedes_or_equals_v1(
    point(0, 0, 1, order.input_absence_subcheck),
    point(1, 0, 1, order.output_canonicity_subcheck),
  )
}

test static_rules_precede_input_sets_despite_wire_tag_order() {
  order.phase_of_reason_v1(reason.NetworkIdMismatch) < order.phase_of_reason_v1(
    reason.EmptyInputs,
  )
}

test resolve_validity_precedes_any_input_without_claimed_item() {
  order.leaf_admits_fault_v1(
    rejected(reason.ValidityIntervalExcludesBlockSlot),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    None,
  )
}

test resolve_earlier_phase_is_admitted_without_claimed_item() {
  order.leaf_admits_fault_v1(
    rejected(reason.RequiredSignerUnsigned { signer_index: 0 }),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    None,
  )
}

test resolve_later_phase_does_not_admit_earlier_input_fault() {
  !order.leaf_admits_fault_v1(
    rejected(reason.OutputNonCanonical { output_index: 0 }),
    order.ResolutionFault(point(0, 0, 1, order.input_absence_subcheck)),
    None,
  )
}
