use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/signer_witness_scan_v1/state
use midgard/fraud_proofs/spend_input_signer_missing/rule
use midgard/rejection_reason_v1.{
  ProtectedOutputSignerMissing, SpendInputSignerMissing,
}

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

/// The verdict the shared scan writes once it reached the field's end or a
/// valid matching signature.
fn scan_verdict(
  subject: substrate.VerdictSubjectV1,
  signer_present: Bool,
) -> state.VerdictV1 {
  state.VerdictV1 { subject, signer_required: True, signer_present }
}

test accepted_missing_signer_closes() {
  state.terminal_v1(scan_verdict(accepted_subject(), False))
}

test accepted_present_signer_is_honest() fail {
  state.terminal_v1(scan_verdict(accepted_subject(), True))
}

test forced_present_signer_closes() {
  let subject = forced_subject(3)
  expect _ = rule.bind_spend_input_v1(subject, 3, prior_root, witness_set_hash)
  state.terminal_v1(scan_verdict(subject, True))
}

test forced_missing_signer_is_honest() fail {
  state.terminal_v1(scan_verdict(forced_subject(3), False))
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

/// The sibling protected-output reason at the same coordinate is another
/// family's subject: the bind compares the whole canonical reason encoding.
test forced_foreign_reason_constructor_is_refused() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..forced_subject(3),
    rejection_reason: Some(ProtectedOutputSignerMissing { output_index: 3 }),
  }
  expect _ = rule.bind_spend_input_v1(subject, 3, prior_root, witness_set_hash)
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
    !verdict.signer_present,
    state.terminal_v1(verdict),
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
  state.terminal_v1(
    state.VerdictV1 {
      subject: accepted_subject(),
      signer_required: False,
      signer_present: False,
    },
  )
}

/// A negative coordinate names no spend input, so canonical validation never
/// evaluates it: a forced rejection citing one closes with no scan.
test forced_negative_coordinate_closes() {
  let subject = forced_subject(-1)
  let bound = rule.bind_spend_input_v1(
    subject,
    -1,
    prior_root,
    witness_set_hash,
  )
  state.terminal_v1(rule.direct_verdict_v1(bound))
}

/// An accepted subject at a negative coordinate has no fault to close on.
test accepted_negative_coordinate_never_closes() fail {
  let bound = rule.bind_spend_input_v1(
    accepted_subject(),
    -1,
    prior_root,
    witness_set_hash,
  )
  state.terminal_v1(rule.direct_verdict_v1(bound))
}
