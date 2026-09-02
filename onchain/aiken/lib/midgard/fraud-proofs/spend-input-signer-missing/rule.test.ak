use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/spend_input_signer_missing/rule
use midgard/rejection_reason_v1.{SpendInputSignerMissing}

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
  rule.terminal_v1(
    rule.VerdictV1 { subject: accepted_subject(), signer_missing: True },
  )
}

test accepted_present_signer_is_honest() fail {
  rule.terminal_v1(
    rule.VerdictV1 { subject: accepted_subject(), signer_missing: False },
  )
}

test forced_present_signer_closes() {
  let subject = forced_subject(3)
  expect _ = rule.bind_spend_input_v1(
    subject,
    3,
    #"0202020202020202020202020202020202020202020202020202020202020202",
    #"0303030303030303030303030303030303030303030303030303030303030303",
  )
  rule.terminal_v1(rule.VerdictV1 { subject, signer_missing: False })
}

test forced_missing_signer_is_honest() fail {
  rule.terminal_v1(
    rule.VerdictV1 { subject: forced_subject(3), signer_missing: True },
  )
}

test forced_wrong_coordinate_is_refused() fail {
  expect _ = rule.bind_spend_input_v1(
    forced_subject(3),
    2,
    #"0202020202020202020202020202020202020202020202020202020202020202",
    #"0303030303030303030303030303030303030303030303030303030303030303",
  )
  True
}
