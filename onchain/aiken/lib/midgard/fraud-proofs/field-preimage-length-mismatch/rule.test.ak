use midgard/fraud_proofs/field_preimage_length_mismatch/rule
use midgard/fraud_proofs/native_tx/types.{NativeTxFieldPreimageLengthsV1}
use midgard/native_tx_field_access_v1.{max_transaction_aggregate_field_bytes}

fn lengths() -> NativeTxFieldPreimageLengthsV1 {
  NativeTxFieldPreimageLengthsV1 {
    spend_inputs: 1,
    reference_inputs: 2,
    outputs: 3,
    required_observers: 4,
    required_signers: 5,
    mint: 6,
    script_witnesses: 7,
    address_witnesses: 8,
    redeemers: 9,
  }
}

test field_preimage_length_mismatch_acceptance_direction_fault_is_decisive() {
  rule.decisive_fault_holds_v1(0, 41, 40)
}

test field_preimage_length_mismatch_rejection_direction_honest_equality_is_not_a_fault() {
  !rule.decisive_fault_holds_v1(8, 104, 104)
}

test field_preimage_length_mismatch_maps_all_nine_coordinates_in_wire_order() {
  and {
    rule.field_preimage_length_at_v1(lengths(), 0) == 1,
    rule.field_preimage_length_at_v1(lengths(), 1) == 2,
    rule.field_preimage_length_at_v1(lengths(), 2) == 3,
    rule.field_preimage_length_at_v1(lengths(), 3) == 4,
    rule.field_preimage_length_at_v1(lengths(), 4) == 5,
    rule.field_preimage_length_at_v1(lengths(), 5) == 6,
    rule.field_preimage_length_at_v1(lengths(), 6) == 7,
    rule.field_preimage_length_at_v1(lengths(), 7) == 8,
    rule.field_preimage_length_at_v1(lengths(), 8) == 9,
  }
}

test field_preimage_length_mismatch_rejects_negative_coordinate() fail {
  rule.decisive_fault_holds_v1(-1, 1, 2)
}

test field_preimage_length_mismatch_rejects_coordinate_nine() fail {
  rule.decisive_fault_holds_v1(9, 1, 2)
}

test field_preimage_length_mismatch_accepts_maximum_actual_length() {
  rule.decisive_fault_holds_v1(
    2,
    max_transaction_aggregate_field_bytes - 1,
    max_transaction_aggregate_field_bytes,
  )
}

test field_preimage_length_mismatch_rejects_adjacent_over_bound() fail {
  rule.decisive_fault_holds_v1(
    2,
    max_transaction_aggregate_field_bytes,
    max_transaction_aggregate_field_bytes + 1,
  )
}

test field_preimage_length_mismatch_rejects_negative_declared_length() fail {
  rule.decisive_fault_holds_v1(2, -1, 1)
}
