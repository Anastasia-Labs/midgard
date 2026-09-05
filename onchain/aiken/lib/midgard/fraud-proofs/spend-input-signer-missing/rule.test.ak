use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/spend_input_signer_missing/rule
use midgard/rejection_reason_v1.{SpendInputSignerMissing}

const tx_id =
  #"0101010101010101010101010101010101010101010101010101010101010101"

const prior_root =
  #"0202020202020202020202020202020202020202020202020202020202020202"

const witness_set_hash =
  #"0303030303030303030303030303030303030303030303030303030303030303"

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

fn forced_subject(input_index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"81",
    rejection_reason: Some(SpendInputSignerMissing { input_index }),
  }
}

test accepted_missing_signer_closes() {
  rule.terminal_v1(rule.scan_verdict_v1(accepted_subject(), True))
}

test accepted_present_signer_is_honest() fail {
  rule.terminal_v1(rule.scan_verdict_v1(accepted_subject(), False))
}

test forced_present_signer_closes() {
  let subject = forced_subject(3)
  expect _ = rule.bind_spend_input_v1(subject, 3, prior_root, witness_set_hash)
  rule.terminal_v1(rule.scan_verdict_v1(subject, False))
}

test forced_missing_signer_is_honest() fail {
  rule.terminal_v1(rule.scan_verdict_v1(forced_subject(3), True))
}

test forced_wrong_coordinate_is_refused() fail {
  expect _ = rule.bind_spend_input_v1(
    forced_subject(3),
    2,
    prior_root,
    witness_set_hash,
  )
  True
}

/// A coordinate canonical validation authorizes without a signer contradicts
/// the forced rejection with no scan at all.
test forced_no_signer_required_closes() {
  let subject = forced_subject(3)
  let bound = rule.bind_spend_input_v1(subject, 3, prior_root, witness_set_hash)
  let verdict = rule.direct_verdict_v1(bound)
  and {
    !verdict.signer_required,
    !verdict.signer_missing,
    rule.terminal_v1(verdict),
  }
}

/// The direct verdict is a wrongful-rejection instrument only: an accepted
/// subject has no fault to close on when no signer is required.
test accepted_subject_cannot_take_the_direct_exit() fail {
  let bound = rule.bind_spend_input_v1(
    accepted_subject(),
    3,
    prior_root,
    witness_set_hash,
  )
  expect _ = rule.direct_verdict_v1(bound)
  True
}

/// Even a hand-built `signer_required = False` verdict never closes an
/// accepted subject: the decisive fault needs a required, missing signer.
test accepted_unrequired_signer_never_closes() fail {
  rule.terminal_v1(
    rule.VerdictV1 {
      subject: accepted_subject(),
      signer_required: False,
      signer_missing: True,
    },
  )
}
