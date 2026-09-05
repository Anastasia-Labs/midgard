use midgard/fraud_proofs/invalid_signature/rule
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1.{AddressWitnessSignatureInvalid, EmptyInputs}

fn forced(index: Int) -> VerdictSubjectV1 {
  VerdictSubjectV1 {
    version: 1,
    direction: 1,
    source_kind: 1,
    transaction_id: #"1111111111111111111111111111111111111111111111111111111111111111",
    source_key: #"01",
    rejection_reason: Some(
      AddressWitnessSignatureInvalid { witness_index: index },
    ),
  }
}

test valid_signature_contradicts_exact_rejection() {
  rule.terminal_contradiction(forced(3), 3, True, True)
}

test honest_invalid_signature_rejection_refuses() fail {
  rule.terminal_contradiction(forced(3), 3, True, False)
}

test coordinate_substitution_refuses() fail {
  rule.terminal_contradiction(forced(3), 2, True, True)
}

test reason_substitution_refuses() fail {
  rule.terminal_contradiction(
    VerdictSubjectV1 { ..forced(3), rejection_reason: Some(EmptyInputs) },
    3,
    True,
    True,
  )
}

test absent_witness_contradicts_rejection() {
  rule.terminal_contradiction(forced(3), 3, False, False)
}

test negative_coordinate_contradicts_rejection() {
  rule.terminal_contradiction(forced(-1), -1, False, False)
}

test accepted_invalid_signature_convicts() {
  rule.terminal_contradiction(
    VerdictSubjectV1 {
      ..forced(3),
      direction: 0,
      source_kind: 0,
      source_key: #"",
      rejection_reason: None,
    },
    3,
    True,
    False,
  )
}

test accepted_absent_witness_refuses() fail {
  rule.terminal_contradiction(
    VerdictSubjectV1 {
      ..forced(3),
      direction: 0,
      source_kind: 0,
      source_key: #"",
      rejection_reason: None,
    },
    3,
    False,
    False,
  )
}

test accepted_valid_signature_refuses() fail {
  rule.terminal_contradiction(
    VerdictSubjectV1 {
      ..forced(3),
      direction: 0,
      source_kind: 0,
      source_key: #"",
      rejection_reason: None,
    },
    3,
    True,
    True,
  )
}
