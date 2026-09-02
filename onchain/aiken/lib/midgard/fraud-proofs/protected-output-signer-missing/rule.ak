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

pub type VerdictV1 {
  subject: substrate.VerdictSubjectV1,
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

pub fn terminal_v1(verdict: VerdictV1) -> Bool {
  substrate.terminal_contradiction_v1(verdict.subject, !verdict.signer_present)
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
