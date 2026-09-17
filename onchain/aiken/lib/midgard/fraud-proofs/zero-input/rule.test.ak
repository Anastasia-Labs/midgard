use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/zero_input/rule
use midgard/rejection_reason_v1.{EmptyInputs, NetworkIdMismatch}

const tx_id =
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

fn rejected() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(EmptyInputs),
  }
}

test zero_input_accepts_empty_wrongful_acceptance() {
  rule.terminal_contradiction_v1(rule.bind_state_v1(accepted()), 0)
}

test zero_input_accepts_non_empty_wrongful_rejection_at_exact_boundary() {
  rule.terminal_contradiction_v1(rule.bind_state_v1(rejected()), 1)
}

test zero_input_accepts_larger_non_empty_wrongful_rejection() {
  rule.terminal_contradiction_v1(rule.bind_state_v1(rejected()), 2)
}

test zero_input_refuses_non_empty_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(rule.bind_state_v1(accepted()), 1)
}

test zero_input_refuses_empty_honest_forced_rejection() fail {
  rule.terminal_contradiction_v1(rule.bind_state_v1(rejected()), 0)
}

test zero_input_refuses_negative_input_count() fail {
  rule.terminal_contradiction_v1(rule.bind_state_v1(rejected()), -1)
}

test zero_input_refuses_typed_reason_substitution() fail {
  rule.bind_state_v1(
    substrate.VerdictSubjectV1 {
      ..rejected(),
      rejection_reason: Some(NetworkIdMismatch),
    },
  ).subject.transaction_id == tx_id
}

test zero_input_refuses_wrongful_rejection_without_forced_reason() fail {
  rule.bind_state_v1(
    substrate.VerdictSubjectV1 { ..rejected(), rejection_reason: None },
  ).subject.transaction_id == tx_id
}
