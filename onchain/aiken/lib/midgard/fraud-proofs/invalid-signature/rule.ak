use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{AddressWitnessSignatureInvalid}

pub fn terminal_contradiction(
  subject: substrate.VerdictSubjectV1,
  witness_index: Int,
  in_range: Bool,
  signature_valid: Bool,
) -> Bool {
  expect
    if subject.direction == substrate.direction_wrongful_rejection {
      expect _ = substrate.bind_exact_rejection_reason_v1(
        subject,
        AddressWitnessSignatureInvalid { witness_index },
      )
      True
    } else {
      True
    }
  // An absent witness cannot produce this typed signature rejection.
  substrate.terminal_contradiction_v1(subject, in_range && !signature_valid)
}
