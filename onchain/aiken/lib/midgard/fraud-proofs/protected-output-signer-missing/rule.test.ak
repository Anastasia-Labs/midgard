use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

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
