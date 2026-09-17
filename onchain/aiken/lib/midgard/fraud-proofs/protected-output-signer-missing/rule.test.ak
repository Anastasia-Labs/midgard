use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule
use midgard/rejection_reason_v1.{
  ProtectedOutputSignerMissing, SpendInputSignerMissing,
}

const tx_id =
  #"0101010101010101010101010101010101010101010101010101010101010101"

fn accepted_subject() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

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
  rule.terminal_v1(rule.scan_verdict_v1(accepted_subject(), False))
}

test accepted_present_signer_is_honest() fail {
  rule.terminal_v1(rule.scan_verdict_v1(accepted_subject(), True))
}

test forced_present_signer_closes() {
  let subject = forced_subject(4)
  expect _ = rule.bind_output_v1(subject, 4)
  rule.terminal_v1(rule.scan_verdict_v1(subject, True))
}

test forced_missing_signer_is_honest() fail {
  rule.terminal_v1(rule.scan_verdict_v1(forced_subject(4), False))
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
  rule.bind_output_v1(accepted_subject(), 7).output_index == 7
}

test negative_output_coordinate_is_refused() fail {
  expect _ = rule.bind_output_v1(accepted_subject(), -1)
  True
}

/// An accepted-root subject carrying a reason is not canonical and cannot be
/// bound in either direction.
test accepted_subject_with_a_reason_is_refused() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..accepted_subject(),
    rejection_reason: Some(ProtectedOutputSignerMissing { output_index: 0 }),
  }
  expect _ = rule.bind_output_v1(subject, 0)
  True
}

/// A coordinate canonical validation authorizes without a signer contradicts
/// the forced rejection with no scan at all.
test forced_no_signer_required_closes() {
  let subject = forced_subject(4)
  let verdict = rule.direct_verdict_v1(rule.bind_output_v1(subject, 4))
  and {
    !verdict.signer_required,
    !verdict.signer_present,
    rule.terminal_v1(verdict),
  }
}

/// The direct verdict is a wrongful-rejection instrument only: an accepted
/// subject has no fault to close on when no signer is required.
test accepted_subject_cannot_take_the_direct_exit() fail {
  expect _ = rule.direct_verdict_v1(rule.bind_output_v1(accepted_subject(), 4))
  True
}

/// Even a hand-built `signer_required = False` verdict never closes an
/// accepted subject: the decisive fault needs a required, absent signer.
test accepted_unrequired_signer_never_closes() fail {
  rule.terminal_v1(
    rule.VerdictV1 {
      subject: accepted_subject(),
      signer_required: False,
      signer_present: False,
    },
  )
}
