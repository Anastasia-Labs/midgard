use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{InputSpentOutputNonCanonical}

pub type BoundInputV1 {
  subject: substrate.VerdictSubjectV1,
  source_kind: Int,
  input_index: Int,
  prior_root: ByteArray,
}

pub type AuthenticatedOutRefV1 {
  subject: substrate.VerdictSubjectV1,
  prior_root: ByteArray,
  out_ref: OutputReference,
}

pub type CanonicalVerdictV1 {
  subject: substrate.VerdictSubjectV1,
  output_is_non_canonical: Bool,
}

pub fn bind_input_v1(
  subject: substrate.VerdictSubjectV1,
  source_kind: Int,
  input_index: Int,
  prior_root: ByteArray,
) -> BoundInputV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect source_kind == 0 || source_kind == 1
  expect input_index >= 0
  expect bytearray.length(prior_root) == 32
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    let expected = InputSpentOutputNonCanonical { source_kind, input_index }
    Some(substrate.bind_exact_rejection_reason_v1(subject, expected))
  } else {
    None
  }
  BoundInputV1 { subject, source_kind, input_index, prior_root }
}

pub fn authenticate_out_ref_v1(
  bound: BoundInputV1,
  out_ref: OutputReference,
) -> AuthenticatedOutRefV1 {
  expect bytearray.length(out_ref.transaction_id) == 32
  expect out_ref.output_index >= 0
  AuthenticatedOutRefV1 {
    subject: bound.subject,
    prior_root: bound.prior_root,
    out_ref,
  }
}

pub fn terminal_contradiction_v1(verdict: CanonicalVerdictV1) -> Bool {
  substrate.terminal_contradiction_v1(
    verdict.subject,
    verdict.output_is_non_canonical,
  )
}
