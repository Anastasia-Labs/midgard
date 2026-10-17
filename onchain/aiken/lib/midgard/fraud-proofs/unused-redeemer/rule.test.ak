use midgard/bounded_item_v1
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/unused_redeemer/rule
use midgard/redeemer_item_proof_v1
use midgard/rejection_reason_v1.{OutputNonCanonical, UnusedRedeemer}
use midgard/script_proof_v1
use midgard/validation_merkle_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h28 = #"11111111111111111111111111111111111111111111111111111111"

const h32 = #"2222222222222222222222222222222222222222222222222222222222222222"

const source =
  #"3333333333333333333333333333333333333333333333333333333333333333"

fn chunk(item: ByteArray) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: 1,
    field_index: 8,
    item_index: 0,
    total_length: 8,
    chunk_index: 0,
    chunk: item,
    frontier: validation_merkle_v1.append_leaf(
      0,
      [],
      bounded_item_v1.hash_chunk(8, 0, 0, item),
    ),
    siblings: [],
  }
}

fn accepted_control(item: ByteArray) -> rule.AuthenticatedControlV1 {
  let commitment = bounded_item_v1.from_bytes(8, 0, item)
  let control = redeemer_item_proof_v1.initial_control_v1(
    0,
    0,
    1,
    8,
    commitment,
    -1,
    -1,
  )
  rule.AuthenticatedControlV1 {
    bound: rule.bind_redeemer_v1(subject(0), h32, 1, 0),
    program_counter: 88,
    stage: 12,
    expected_item_control_hash: redeemer_item_proof_v1.hash_control_v1(control),
    used_redeemer_bitmap: 0,
    current_purpose_kind: -1,
    current_purpose_index: -1,
    redeemer_count: 1,
    purpose_count: 0,
    purpose_peaks: [],
    execution_count: 0,
    execution_peaks: [],
  }
}

fn subject(direction: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction,
    source_kind: if direction == 0 {
      0
    } else {
      1
    },
    transaction_id: tx_id,
    source_key: if direction == 0 {
      #""
    } else {
      #"01"
    },
    rejection_reason: if direction == 0 {
      None
    } else {
      Some(UnusedRedeemer { redeemer_index: 1 })
    },
  }
}

fn tag(kind: Int) -> Int {
  when kind is {
    0 -> 0
    1 -> 1
    2 -> 3
    3 -> 6
    _ -> fail
  }
}

fn purpose(kind: Int, index: Int) -> ByteArray {
  script_proof_v1.purpose_leaf_hash(kind, index, h28, #"aa")
}

fn authenticated(
  direction: Int,
  purpose_leaves: List<ByteArray>,
  execution_leaves: List<ByteArray>,
) -> rule.AuthenticatedRedeemerV1 {
  let purposes = validation_merkle_v1.build_frontier(purpose_leaves)
  let executions = validation_merkle_v1.build_frontier(execution_leaves)
  rule.AuthenticatedRedeemerV1 {
    bound: rule.bind_redeemer_v1(subject(direction), h32, 1, 1),
    purpose_tag: 3,
    pointer_index: 4,
    item_count: 2,
    item_length: 12,
    item_commitment: h32,
    redeemer_leaf: script_proof_v1.redeemer_item_leaf_hash(1, h32),
    purpose_count: purposes.count,
    purpose_peaks: purposes.peaks,
    execution_count: executions.count,
    execution_peaks: executions.peaks,
  }
}

fn one_scan(kind: Int, pointer: Int, selected: Bool) -> rule.ReverseScanV1 {
  let p = purpose(kind, pointer)
  let target = script_proof_v1.redeemer_item_leaf_hash(1, h32)
  let redeemer = if selected {
    target
  } else {
    #""
  }
  let e = script_proof_v1.execution_leaf_hash(
    if selected {
      3
    } else {
      0
    },
    p,
    source,
    redeemer,
  )
  rule.scan_selection_v1(
    rule.initial_reverse_scan_v1(authenticated(0, [p], [e])),
    rule.SelectionOpeningV1 {
      frontier_index: 0,
      purpose_kind: kind,
      purpose_index: pointer,
      script_hash: h28,
      purpose_subject: #"aa",
      purpose_siblings: [],
      language_tag: if selected {
        3
      } else {
        0
      },
      source_leaf: source,
      redeemer_leaf: redeemer,
      execution_siblings: [],
    },
  )
}

test purpose_tag_map_is_exact() {
  and {
    rule.redeemer_tag_for_purpose_kind_v1(0) == Some(0),
    rule.redeemer_tag_for_purpose_kind_v1(1) == Some(1),
    rule.redeemer_tag_for_purpose_kind_v1(2) == Some(3),
    rule.redeemer_tag_for_purpose_kind_v1(3) == Some(6),
    rule.redeemer_tag_for_purpose_kind_v1(4) == None,
  }
}

test observe_selection_marks_exact_pointer_used() {
  one_scan(2, 4, True).used
}

test native_selection_does_not_mark_redeemer_used() {
  !one_scan(2, 4, False).used
}

test another_pointer_is_unused() {
  !one_scan(2, 3, True).used
}

test complete_absence_convicts_wrongful_acceptance() {
  let state = one_scan(2, 3, True)
  rule.terminal_contradiction_v1(rule.decision_v1(state))
}

test used_pointer_contradicts_forced_rejection() {
  let p = purpose(2, 4)
  let target = script_proof_v1.redeemer_item_leaf_hash(1, h32)
  let e = script_proof_v1.execution_leaf_hash(3, p, source, target)
  let state = rule.scan_selection_v1(
    rule.initial_reverse_scan_v1(authenticated(1, [p], [e])),
    rule.SelectionOpeningV1 {
      frontier_index: 0,
      purpose_kind: 2,
      purpose_index: 4,
      script_hash: h28,
      purpose_subject: #"aa",
      purpose_siblings: [],
      language_tag: 3,
      source_leaf: source,
      redeemer_leaf: target,
      execution_siblings: [],
    },
  )
  rule.terminal_contradiction_v1(rule.decision_v1(state))
}

test premature_absence_refuses() {
  let p0 = purpose(0, 0)
  let p1 = purpose(2, 4)
  let target = script_proof_v1.redeemer_item_leaf_hash(1, h32)
  let e0 = script_proof_v1.execution_leaf_hash(0, p0, source, #"")
  let e1 = script_proof_v1.execution_leaf_hash(3, p1, source, target)
  let state = rule.initial_reverse_scan_v1(authenticated(0, [p0, p1], [e0, e1]))
  !rule.reverse_scan_complete_v1(state)
}

test wrong_reason_constructor_is_not_exact() {
  let bad = substrate.VerdictSubjectV1 {
    ..subject(1),
    rejection_reason: Some(OutputNonCanonical { output_index: 1 }),
  }
  bad.rejection_reason != Some(UnusedRedeemer { redeemer_index: 1 })
}

test bounded_header_and_tail_authenticate_exact_descriptor() {
  let item = #"8400044100820101"
  let authenticated = accepted_control(item)
  let initial = redeemer_item_proof_v1.initial_control_v1(
    0,
    0,
    1,
    8,
    bounded_item_v1.from_bytes(8, 0, item),
    -1,
    -1,
  )
  let header = rule.authenticate_item_header_v1(
    authenticated,
    initial,
    chunk(item),
    None,
  )
  let complete = rule.authenticate_item_tail_v1(header, chunk(item), None)
  and {
    complete.purpose_tag == 0,
    complete.pointer_index == 4,
  }
}

test substituted_item_chunk_refuses() {
  let item = #"8400044100820101"
  let initial = redeemer_item_proof_v1.initial_control_v1(
    0,
    0,
    1,
    8,
    bounded_item_v1.from_bytes(8, 0, item),
    -1,
    -1,
  )
  let bad = bounded_item_v1.ChunkProofV1 {
    ..chunk(item),
    chunk: #"8400034100820101",
  }
  and {
    redeemer_item_proof_v1.hash_control_v1(initial) == accepted_control(item).expected_item_control_hash,
    !bounded_item_v1.verify_chunk(initial.item_commitment, bad),
  }
}
