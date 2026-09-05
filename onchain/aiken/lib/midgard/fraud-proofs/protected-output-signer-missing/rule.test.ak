use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule
use midgard/rejection_reason_v1.{
  ProtectedOutputSignerMissing, SpendInputSignerMissing,
}

const tx_id =
  #"0101010101010101010101010101010101010101010101010101010101010101"

fn forced_subject(output_index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"81",
    rejection_reason: Some(ProtectedOutputSignerMissing { output_index }),
  }
}

test accepted_missing_signer_closes() {
  rule.terminal_v1(
    rule.VerdictV1 {
      subject: substrate.VerdictSubjectV1 {
        version: substrate.version,
        direction: substrate.direction_wrongful_acceptance,
        source_kind: substrate.source_kind_accepted,
        transaction_id: tx_id,
        source_key: #"",
        rejection_reason: None,
      },
      signer_present: False,
    },
  )
}

test accepted_present_signer_is_honest() fail {
  rule.terminal_v1(
    rule.VerdictV1 {
      subject: substrate.VerdictSubjectV1 {
        version: substrate.version,
        direction: substrate.direction_wrongful_acceptance,
        source_kind: substrate.source_kind_accepted,
        transaction_id: tx_id,
        source_key: #"",
        rejection_reason: None,
      },
      signer_present: True,
    },
  )
}

test forced_present_signer_closes() {
  let subject = forced_subject(4)
  expect _ = rule.bind_output_v1(subject, 4)
  rule.terminal_v1(rule.VerdictV1 { subject, signer_present: True })
}

test forced_missing_signer_is_honest() fail {
  rule.terminal_v1(
    rule.VerdictV1 { subject: forced_subject(4), signer_present: False },
  )
}

test forced_wrong_output_coordinate_is_refused() fail {
  expect _ = rule.bind_output_v1(forced_subject(4), 3)
  True
}

/// The sibling spend-input reason at the same coordinate is another
/// family's subject: the bind compares the whole canonical reason encoding.
test forced_foreign_reason_constructor_is_refused() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..forced_subject(4),
    rejection_reason: Some(SpendInputSignerMissing { input_index: 4 }),
  }
  expect _ = rule.bind_output_v1(subject, 4)
  True
}

test accepted_subject_binds_any_in_range_coordinate() {
  let subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
  rule.bind_output_v1(subject, 7).output_index == 7
}

test negative_output_coordinate_is_refused() fail {
  let subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
  expect _ = rule.bind_output_v1(subject, -1)
  True
}

/// An accepted-root subject carrying a reason is not canonical and cannot be
/// bound in either direction.
test accepted_subject_with_a_reason_is_refused() fail {
  let subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: Some(ProtectedOutputSignerMissing { output_index: 0 }),
  }
  expect _ = rule.bind_output_v1(subject, 0)
  True
}
