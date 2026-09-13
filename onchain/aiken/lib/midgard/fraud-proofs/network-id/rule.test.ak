use midgard/fraud_proofs/network_id/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{EmptyInputs, NetworkIdMismatch}

const tx_id =
  #"1111111111111111111111111111111111111111111111111111111111111111"

fn forced_subject(reason) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"820102",
    rejection_reason: Some(reason),
  }
}

test forced_network_reason_binds_exact_constructor() {
  substrate.bind_exact_rejection_reason_v1(
    forced_subject(NetworkIdMismatch),
    NetworkIdMismatch,
  ) == NetworkIdMismatch
}

test another_forced_reason_cannot_enter_network_family() fail {
  substrate.bind_exact_rejection_reason_v1(
    forced_subject(EmptyInputs),
    NetworkIdMismatch,
  ) == NetworkIdMismatch
}

test equal_transaction_and_output_networks_contradict_forced_rejection() {
  substrate.terminal_contradiction_v1(
    forced_subject(NetworkIdMismatch),
    rule.is_any_network_violation_v1(0, [0, 0, 0], 0),
  )
}

test absent_transaction_network_and_matching_outputs_contradict_rejection() {
  substrate.terminal_contradiction_v1(
    forced_subject(NetworkIdMismatch),
    rule.is_any_network_violation_v1(rule.native_network_id_none, [1], 1),
  )
}

test explicit_transaction_mismatch_refuses_wrongful_rejection() fail {
  substrate.terminal_contradiction_v1(
    forced_subject(NetworkIdMismatch),
    rule.is_any_network_violation_v1(1, [0], 0),
  )
}

test one_output_mismatch_refuses_wrongful_rejection() fail {
  substrate.terminal_contradiction_v1(
    forced_subject(NetworkIdMismatch),
    rule.is_any_network_violation_v1(0, [0, 1], 0),
  )
}

test empty_output_domain_is_a_complete_equality_proof() {
  substrate.terminal_contradiction_v1(
    forced_subject(NetworkIdMismatch),
    rule.is_any_network_violation_v1(0, [], 0),
  )
}
