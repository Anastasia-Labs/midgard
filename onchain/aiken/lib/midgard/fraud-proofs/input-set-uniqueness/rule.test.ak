use midgard/fraud_proofs/input_set_uniqueness/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{DuplicateInput}

const tx_id =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const source_key = #"820102"

const scan_hash = #"22222222222222222222222222222222222222222222222222222222"

fn subject(
  first_field: Int,
  first_item: Int,
  second_field: Int,
  second_item: Int,
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key,
    rejection_reason: Some(
      DuplicateInput {
        first_field_index: first_field,
        first_item_index: first_item,
        second_field_index: second_field,
        second_item_index: second_item,
      },
    ),
  }
}

const item_a =
  #"8258201111111111111111111111111111111111111111111111111111111111111111190000"

const item_b =
  #"8258202222222222222222222222222222222222222222222222222222222222222222190000"

const item_c =
  #"8258203333333333333333333333333333333333333333333333333333333333333333190000"

fn initial() -> rule.UniqueScanStateV1 {
  rule.initial_scan_state_v1(
    rule.bind_forced_duplicate_reason_v1(subject(0, 0, 1, 0)),
    2,
    1,
    scan_hash,
  )
}

test complete_strict_union_contradicts_forced_duplicate_rejection() {
  let s1 = rule.advance_unique_scan_v1(initial(), item_a, scan_hash)
  let s2 = rule.advance_unique_scan_v1(s1, item_b, scan_hash)
  let s3 = rule.advance_unique_scan_v1(s2, item_c, scan_hash)
  rule.unique_scan_is_complete_v1(s3)
}

test equal_adjacent_item_refuses_complete_negation() fail {
  let s1 = rule.advance_unique_scan_v1(initial(), item_a, scan_hash)
  rule.advance_unique_scan_v1(s1, item_a, scan_hash).cursor == 2
}

test descending_adjacent_item_refuses_complete_negation() fail {
  let s1 = rule.advance_unique_scan_v1(initial(), item_b, scan_hash)
  rule.advance_unique_scan_v1(s1, item_a, scan_hash).cursor == 2
}

test incomplete_scan_cannot_finalize() fail {
  rule.unique_scan_is_complete_v1(
    rule.advance_unique_scan_v1(initial(), item_a, scan_hash),
  )
}

test out_of_range_accused_coordinate_is_refused() fail {
  rule.initial_scan_state_v1(
    rule.bind_forced_duplicate_reason_v1(subject(0, 0, 1, 2)),
    2,
    1,
    scan_hash,
  ).cursor == 0
}

test reversed_accused_coordinate_is_refused() fail {
  rule.initial_scan_state_v1(
    rule.bind_forced_duplicate_reason_v1(subject(1, 0, 0, 1)),
    2,
    1,
    scan_hash,
  ).cursor == 0
}

test malformed_checkpoint_is_refused() fail {
  let state = initial()
  rule.scan_state_is_authentic_v1(
    rule.UniqueScanStateV1 { ..state, checkpoint_hash: #"00" },
  )
}

test checkpoint_binds_next_expected_script() {
  let state = initial()
  state.checkpoint_hash != rule.checkpoint_v1(
    state.bound,
    state.spend_count,
    state.reference_count,
    state.cursor,
    state.previous_item,
    #"44444444444444444444444444444444444444444444444444444444",
  )
}

test adjacent_batch_advances_one_authenticated_checkpoint() {
  let state = rule.advance_unique_batch_v1(
    initial(),
    [item_a, item_b, item_c],
    scan_hash,
  )
  and {
    state.cursor == 3,
    state.previous_item == item_c,
    rule.unique_scan_is_complete_v1(state),
  }
}

test equal_item_inside_batch_is_refused() fail {
  rule.advance_unique_batch_v1(initial(), [item_a,
      item_a, item_c], scan_hash).cursor == 3
}

test descending_item_inside_batch_is_refused() fail {
  rule.advance_unique_batch_v1(initial(), [item_b,
      item_a, item_c], scan_hash).cursor == 3
}

test batch_cannot_advance_past_authenticated_total() fail {
  rule.advance_unique_batch_v1(
    initial(),
    [item_a, item_b, item_c, item_c],
    scan_hash,
  ).cursor == 4
}
