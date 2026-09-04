use cardano/transaction.{OutputReference}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/resolved_output_non_canonical/rule
use midgard/rejection_reason_v1.{InputNotFound, InputSpentOutputNonCanonical}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const prior_root =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

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

fn rejected(source_kind: Int, input_index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(
      InputSpentOutputNonCanonical { source_kind, input_index },
    ),
  }
}

fn verdict(
  subject: substrate.VerdictSubjectV1,
  invalid: Bool,
) -> rule.CanonicalVerdictV1 {
  rule.CanonicalVerdictV1 { subject, output_is_non_canonical: invalid }
}

test resolved_output_non_canonical_convicts_wrongful_acceptance() {
  rule.terminal_contradiction_v1(verdict(accepted(), True))
}

test resolved_output_non_canonical_convicts_wrongful_rejection() {
  rule.terminal_contradiction_v1(verdict(rejected(1, 4), False))
}

test resolved_output_non_canonical_refuses_honest_acceptance() fail {
  rule.terminal_contradiction_v1(verdict(accepted(), False))
}

test resolved_output_non_canonical_refuses_honest_rejection() fail {
  rule.terminal_contradiction_v1(verdict(rejected(0, 2), True))
}

test resolved_output_non_canonical_binds_exact_reason_coordinate() {
  let bound = rule.bind_input_v1(rejected(1, 4), 1, 4, prior_root)
  and {
    bound.source_kind == 1,
    bound.input_index == 4,
    bound.prior_root == prior_root,
  }
}

test resolved_output_non_canonical_refuses_source_substitution() fail {
  rule.bind_input_v1(rejected(1, 4), 0, 4, prior_root).source_kind == 0
}

test resolved_output_non_canonical_refuses_index_substitution() fail {
  rule.bind_input_v1(rejected(1, 4), 1, 3, prior_root).input_index == 3
}

test resolved_output_non_canonical_refuses_other_reason() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..rejected(1, 4),
    rejection_reason: Some(InputNotFound { source_kind: 1, input_index: 4 }),
  }
  rule.bind_input_v1(subject, 1, 4, prior_root).input_index == 4
}

test resolved_output_non_canonical_refuses_prior_root_substitution() fail {
  rule.bind_input_v1(accepted(), 0, 0, #"00").input_index == 0
}

test resolved_output_non_canonical_authenticates_exact_out_ref() {
  let bound = rule.bind_input_v1(accepted(), 0, 0, prior_root)
  let out_ref = OutputReference { transaction_id: tx_id, output_index: 7 }
  rule.authenticate_out_ref_v1(bound, out_ref).out_ref == out_ref
}
