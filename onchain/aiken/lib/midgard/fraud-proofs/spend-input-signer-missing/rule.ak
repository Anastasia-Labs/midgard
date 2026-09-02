//// Narrow semantic and state wire for `spendInputSignerMissing`.

use aiken/cbor
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{SpendInputSignerMissing}

pub const witness_scan_batch_size: Int = 16

pub type BoundSpendInputV1 {
  subject: substrate.VerdictSubjectV1,
  input_index: Int,
  prior_root: ByteArray,
  witness_set_hash: ByteArray,
}

pub type AuthenticatedCredentialV1 {
  subject: substrate.VerdictSubjectV1,
  transaction_id: ByteArray,
  witness_set_hash: ByteArray,
  payment_credential: VerificationKeyHash,
}

pub type WitnessScanV1 {
  authenticated: AuthenticatedCredentialV1,
  checkpoint_hash: ByteArray,
}

pub type VerdictV1 {
  subject: substrate.VerdictSubjectV1,
  signer_missing: Bool,
}

pub fn bind_spend_input_v1(
  subject: substrate.VerdictSubjectV1,
  input_index: Int,
  prior_root: ByteArray,
  witness_set_hash: ByteArray,
) -> BoundSpendInputV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect input_index >= 0
  expect bytearray.length(prior_root) == 32
  expect bytearray.length(witness_set_hash) == 32
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        SpendInputSignerMissing { input_index },
      ),
    )
  } else {
    None
  }
  BoundSpendInputV1 { subject, input_index, prior_root, witness_set_hash }
}

pub fn authenticate_credential_v1(
  bound: BoundSpendInputV1,
  payment_credential: VerificationKeyHash,
) -> AuthenticatedCredentialV1 {
  expect bytearray.length(payment_credential) == 28
  AuthenticatedCredentialV1 {
    subject: bound.subject,
    transaction_id: bound.subject.transaction_id,
    witness_set_hash: bound.witness_set_hash,
    payment_credential,
  }
}

pub fn terminal_v1(verdict: VerdictV1) -> Bool {
  substrate.terminal_contradiction_v1(verdict.subject, verdict.signer_missing)
}

pub fn encode_bound_v1(value: BoundSpendInputV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_authenticated_v1(value: AuthenticatedCredentialV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_scan_v1(value: WitnessScanV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_verdict_v1(value: VerdictV1) -> ByteArray {
  cbor.serialise(value)
}
