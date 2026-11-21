//// Shared state for the signer-witness scan: the address-witness walk and the
//// terminal that `spendInputSignerMissing` and
//// `ProtectedOutputSignerMissing` both run after their own step 02.
////
//// Each family keeps its own step 01 and step 02: the rejection-reason bind,
//// the coordinate's credential derivation and the direct-exit decision stay
//// there. Step 02 hands this module's `ScanV1` to the shared scan validator,
//// or a `direct_verdict` straight to the shared terminal. The thread token
//// carries the family identity, so sharing the later steps does not let one
//// family's thread finish under the other's rule.

use aiken/crypto.{VerificationKeyHash}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

/// Witnesses one scan transaction visits at most. Every visited item costs a
/// key hash; an item whose key hashes to the target also costs one Ed25519
/// check, so the worst turn is a batch of matching keys with bad signatures.
pub const witness_scan_batch_size: Int = 32

/// The scan's state between transactions. `checkpoint_hash` is empty before
/// the first batch (the opening turn) and is the field-walk checkpoint digest
/// after it. The transaction id is `subject.transaction_id`.
pub type ScanV1 {
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  payment_credential: VerificationKeyHash,
  checkpoint_hash: ByteArray,
}

/// Terminal verdict. `signer_required` is `True` when the scan ran, and then
/// `signer_present` says whether a witness both hashes to the credential and
/// signs the transaction id. A family's step 02 writes `signer_required =
/// False` when canonical validation needs no signer at its coordinate.
pub type VerdictV1 {
  subject: substrate.VerdictSubjectV1,
  signer_required: Bool,
  signer_present: Bool,
}

/// The state a family's step 02 writes when the coordinate needs a key
/// signature: the walk has not opened yet.
pub fn start(
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  payment_credential: VerificationKeyHash,
) -> ScanV1 {
  ScanV1 { subject, witness_set_hash, payment_credential, checkpoint_hash: #"" }
}

/// The verdict a family's step 02 writes straight to the terminal.
pub fn direct_verdict(
  subject: substrate.VerdictSubjectV1,
  signer_required: Bool,
  signer_present: Bool,
) -> VerdictV1 {
  VerdictV1 { subject, signer_required, signer_present }
}

/// The decisive fault is a required signer the scan did not find. An
/// accepted subject closes on it; a forced wrongful-rejection subject closes
/// when it does not hold.
pub fn terminal_v1(verdict: VerdictV1) -> Bool {
  substrate.terminal_contradiction_v1(
    verdict.subject,
    verdict.signer_required && !verdict.signer_present,
  )
}
