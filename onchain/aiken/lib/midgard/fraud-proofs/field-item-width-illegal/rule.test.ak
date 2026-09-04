use midgard/fraud_proofs/field_item_width_illegal/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{
  FieldItemWidthIllegal, FieldPreimageLengthMismatch,
}

const tx_id: ByteArray =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn rejected(field_index: Int, item_index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"d8799f581c000000000000000000000000000000000000000000000000000000001affffffff",
    rejection_reason: Some(FieldItemWidthIllegal { field_index, item_index }),
  }
}

fn width_state(
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  item_index: Int,
  item_width: Int,
) -> rule.AuthenticatedWidthV1 {
  rule.AuthenticatedWidthV1 { subject, field_index, item_index, item_width }
}

test field_item_width_rule_acceptance_convicts_oversize_output() {
  rule.terminal_contradiction_v1(
    width_state(accepted(), 2, 0, rule.max_serialized_output_preimage_bytes + 1),
  )
}

test field_item_width_rule_acceptance_convicts_empty_mint_item() {
  rule.terminal_contradiction_v1(width_state(accepted(), 5, 3, 0))
}

test field_item_width_rule_rejection_convicts_legal_output() {
  rule.terminal_contradiction_v1(
    width_state(rejected(2, 0), 2, 0, rule.max_serialized_output_preimage_bytes),
  )
}

test field_item_width_rule_rejection_convicts_nonempty_mint_item() {
  rule.terminal_contradiction_v1(width_state(rejected(5, 3), 5, 3, 1))
}

test field_item_width_rule_honest_acceptance_refuses_boundary_output() fail {
  rule.terminal_contradiction_v1(
    width_state(accepted(), 2, 0, rule.max_serialized_output_preimage_bytes),
  )
}

test field_item_width_rule_honest_acceptance_refuses_nonempty_mint() fail {
  rule.terminal_contradiction_v1(width_state(accepted(), 5, 0, 1))
}

test field_item_width_rule_honest_rejection_refuses_oversize_output() fail {
  rule.terminal_contradiction_v1(
    width_state(
      rejected(2, 0),
      2,
      0,
      rule.max_serialized_output_preimage_bytes + 1,
    ),
  )
}

test field_item_width_rule_honest_rejection_refuses_empty_mint() fail {
  rule.terminal_contradiction_v1(width_state(rejected(5, 0), 5, 0, 0))
}

test field_item_width_rule_binds_exact_forced_coordinate() {
  rule.bind_coordinate_v1(rejected(2, 7), 2, 7).item_index == 7
}

test field_item_width_rule_rejects_mutated_item_coordinate() fail {
  rule.bind_coordinate_v1(rejected(2, 7), 2, 8).item_index == 8
}

test field_item_width_rule_rejects_mutated_field_coordinate() fail {
  rule.bind_coordinate_v1(rejected(2, 7), 5, 7).field_index == 5
}

test field_item_width_rule_rejects_other_reason_constructor() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..rejected(2, 0),
    rejection_reason: Some(FieldPreimageLengthMismatch { field_index: 2 }),
  }
  rule.bind_coordinate_v1(subject, 2, 0).field_index == 2
}

test field_item_width_rule_rejects_unrelated_field() fail {
  rule.bind_coordinate_v1(accepted(), 4, 0).field_index == 4
}

test field_item_width_rule_rejects_negative_coordinate() fail {
  rule.bind_coordinate_v1(accepted(), 2, -1).item_index == -1
}

test field_item_width_rule_authentication_derives_width_from_bytes() {
  let bound = rule.bind_coordinate_v1(accepted(), 5, 0)
  let state = rule.authenticate_item_width_v1(bound, #"000102")
  and {
    state.item_width == 3,
    state.field_index == 5,
    state.item_index == 0,
  }
}

test field_item_width_rule_rejects_negative_width_state() fail {
  rule.terminal_contradiction_v1(width_state(accepted(), 2, 0, -1))
}

test field_item_width_rule_abi_vectors_are_stable() {
  let bound = rule.bind_coordinate_v1(accepted(), 5, 0)
  let authenticated = rule.authenticate_item_width_v1(bound, #"000102")
  and {
    rule.encode_bound_coordinate_v1(bound) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500ff",
    rule.encode_authenticated_width_v1(authenticated) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff050003ff",
  }
}
