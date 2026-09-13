//// Narrow rule for `ProtectedOutputSignerMissing`.

use aiken/cbor
use aiken/crypto.{VerificationKeyHash}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

pub const outputs_field_index: Int = 2

pub const address_witnesses_field_index: Int = 7

pub const witness_scan_batch_size: Int = 32

pub type BoundOutputV1 {
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
}

pub type ProtectedCredentialV1 {
  subject: substrate.VerdictSubjectV1,
  transaction_id: ByteArray,
  witness_set_hash: ByteArray,
  output_index: Int,
  payment_credential: VerificationKeyHash,
}

pub type WitnessScanV1 {
  protected: ProtectedCredentialV1,
  checkpoint_hash: ByteArray,
  signer_present: Bool,
}

/// Terminal verdict. `signer_required` is `True` when the coordinate is an
/// in-range protected pub-key output and `signer_present` is the complete
/// scan's answer; it is `False` when canonical validation needs no signer at
/// the coordinate (an unprotected output, a protected script output, or a
/// position past the field-2 count), in which case `signer_present` is
/// `False` by construction.
pub type VerdictV1 {
  subject: substrate.VerdictSubjectV1,
  signer_required: Bool,
  signer_present: Bool,
}

pub fn bind_output_v1(
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
) -> BoundOutputV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect output_index >= 0
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        ProtectedOutputSignerMissing { output_index },
      ),
    )
  } else {
    None
  }
  BoundOutputV1 { subject, output_index }
}

/// The verdict the witness scan writes when it reaches the field's end.
pub fn scan_verdict_v1(
  subject: substrate.VerdictSubjectV1,
  signer_present: Bool,
) -> VerdictV1 {
  VerdictV1 { subject, signer_required: True, signer_present }
}

/// The verdict step 02 writes straight to the terminal when canonical
/// validation requires no signer at the bound coordinate. Only a wrongful
/// forced rejection can be contradicted this way: an accepted subject has no
/// fault to close on, so the direct exit is refused for it.
pub fn direct_verdict_v1(bound: BoundOutputV1) -> VerdictV1 {
  expect bound.subject.direction == substrate.direction_wrongful_rejection
  VerdictV1 {
    subject: bound.subject,
    signer_required: False,
    signer_present: False,
  }
}

pub fn terminal_v1(verdict: VerdictV1) -> Bool {
  substrate.terminal_contradiction_v1(
    verdict.subject,
    verdict.signer_required && !verdict.signer_present,
  )
}

pub fn encode_bound_v1(value: BoundOutputV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_protected_v1(value: ProtectedCredentialV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_scan_v1(value: WitnessScanV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_verdict_v1(value: VerdictV1) -> ByteArray {
  cbor.serialise(value)
}
