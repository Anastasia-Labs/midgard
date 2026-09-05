use midgard/fraud_proofs/observers_forbidden_on_untagged_network/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{
  NetworkIdMismatch, ObserversForbiddenOnUntaggedNetwork, OutputNonCanonical,
}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

/// A script integrity hash the machine treats as present: Plutus evaluation
/// is required and the observer arm is reachable.
const present_hash =
  #"abababababababababababababababababababababababababababababababab"

const zero_hash = rule.zero_script_integrity_hash

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

// ── Decisive predicate: exact twin of the phase-A observer arm ─────────

test observers_forbidden_holds_only_past_a_present_integrity_hash() {
  and {
    rule.forbidden_observers_hold_v1(1, 255, present_hash),
    !rule.forbidden_observers_hold_v1(1, 255, zero_hash),
    !rule.forbidden_observers_hold_v1(0, 255, present_hash),
    !rule.forbidden_observers_hold_v1(1, 0, present_hash),
    !rule.forbidden_observers_hold_v1(1, 1, present_hash),
  }
}

test observers_forbidden_refuses_a_malformed_integrity_hash() fail {
  rule.forbidden_observers_hold_v1(1, 255, #"ab")
}

// ── Wrongful acceptance ────────────────────────────────────────────────

test observers_forbidden_accepts_non_empty_untagged_wrongful_acceptance() {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(accepted(), 255),
    1,
    present_hash,
  )
}

test observers_forbidden_refuses_empty_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(accepted(), 255),
    0,
    present_hash,
  )
}

test observers_forbidden_refuses_tagged_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(accepted(), 1),
    1,
    present_hash,
  )
}

/// A native-only transaction (zero integrity hash) carrying observers on
/// scalar 255 is accepted by the machine; convicting it would convict an
/// honest operator.
test observers_forbidden_refuses_native_only_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(accepted(), 255),
    1,
    zero_hash,
  )
}

test observers_forbidden_refuses_negative_observer_count() fail {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(accepted(), 255),
    -1,
    present_hash,
  )
}

// ── Wrongful rejection ─────────────────────────────────────────────────

test observers_forbidden_accepts_empty_untagged_wrongful_rejection() {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(rejected(), 255),
    0,
    present_hash,
  )
}

test observers_forbidden_accepts_non_empty_tagged_wrongful_rejection() {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(rejected(), 0),
    1,
    present_hash,
  )
}

/// The operator rejected a native-only transaction for this reason: the
/// observer arm was never reachable, so the rejection is wrongful.
test observers_forbidden_accepts_native_only_wrongful_rejection() {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(rejected(), 255),
    1,
    zero_hash,
  )
}

test observers_forbidden_refuses_honest_untagged_non_empty_rejection() fail {
  rule.terminal_contradiction_v1(
    rule.bind_state_v1(rejected(), 255),
    1,
    present_hash,
  )
}

// ── Binding: subject, reason, and network scalar ───────────────────────

test observers_forbidden_refuses_typed_reason_substitution() fail {
  rule.bind_state_v1(
    substrate.VerdictSubjectV1 {
      ..rejected(),
      rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
    },
    255,
  ).network_id == 255
}

test observers_forbidden_refuses_sibling_tx_global_reason_substitution() fail {
  rule.bind_state_v1(
    substrate.VerdictSubjectV1 {
      ..rejected(),
      rejection_reason: Some(NetworkIdMismatch),
    },
    255,
  ).network_id == 255
}

test observers_forbidden_refuses_missing_rejection_reason() fail {
  rule.bind_state_v1(
    substrate.VerdictSubjectV1 { ..rejected(), rejection_reason: None },
    255,
  ).network_id == 255
}

test observers_forbidden_refuses_accepted_subject_with_reason() fail {
  rule.bind_state_v1(
    substrate.VerdictSubjectV1 {
      ..accepted(),
      rejection_reason: Some(ObserversForbiddenOnUntaggedNetwork),
    },
    255,
  ).network_id == 255
}

test observers_forbidden_refuses_network_scalar_substitution() fail {
  rule.bind_state_v1(accepted(), 2).network_id == 2
}

test observers_forbidden_binds_every_canonical_network_scalar() {
  and {
    rule.bind_state_v1(accepted(), 0).network_id == 0,
    rule.bind_state_v1(accepted(), 1).network_id == 1,
    rule.bind_state_v1(rejected(), 255).network_id == 255,
  }
}
