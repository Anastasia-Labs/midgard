use midgard/fraud_proofs/observers_forbidden_on_untagged_network/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{
  ObserversForbiddenOnUntaggedNetwork, OutputNonCanonical,
}

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
    rejection_reason: Some(ObserversForbiddenOnUntaggedNetwork),
  }
}

test observers_forbidden_accepts_non_empty_untagged_wrongful_acceptance() {
  rule.terminal_contradiction_v1(rule.bind_state_v1(accepted(), 255), 1)
}

test observers_forbidden_accepts_empty_untagged_wrongful_rejection() {
  rule.terminal_contradiction_v1(rule.bind_state_v1(rejected(), 255), 0)
}

test observers_forbidden_accepts_non_empty_tagged_wrongful_rejection() {
  rule.terminal_contradiction_v1(rule.bind_state_v1(rejected(), 0), 1)
}

test observers_forbidden_refuses_empty_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(rule.bind_state_v1(accepted(), 255), 0)
}

test observers_forbidden_refuses_untagged_non_empty_wrongful_rejection() fail {
  rule.terminal_contradiction_v1(rule.bind_state_v1(rejected(), 255), 1)
}

test observers_forbidden_refuses_typed_reason_substitution() fail {
  rule.bind_state_v1(
    substrate.VerdictSubjectV1 {
      ..rejected(),
      rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
    },
    255,
  ).network_id == 255
}

test observers_forbidden_refuses_network_scalar_substitution() fail {
  rule.bind_state_v1(accepted(), 2).network_id == 2
}
