use aiken/crypto.{verify_ed25519_signature}
use midgard/common/utils
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{WitnessNativeScriptFalse}

pub fn bind_script_index(
  subject: substrate.VerdictSubjectV1,
  script_index: Int,
) -> Bool {
  expect substrate.subject_is_canonical_v1(subject)
  expect script_index >= 0
  if subject.direction == substrate.direction_wrongful_rejection {
    expect Some(WitnessNativeScriptFalse { script_index: committed_index }) = subject.rejection_reason
    script_index == committed_index
  } else {
    True
  }
}

pub fn terminal(subject: substrate.VerdictSubjectV1, satisfied: Bool) -> Bool {
  substrate.terminal_contradiction_v1(subject, !satisfied)
}

pub fn accepted_subject(tx_id: ByteArray) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

// A false-rejection conviction may use only signatures authenticated against
// the committed body hash. Invalid witnesses contribute no signer.
pub fn authenticated_signer(
  subject: substrate.VerdictSubjectV1,
  item: ByteArray,
) -> Option<ByteArray> {
  let witness = decode_midgard_address_witness_cbor(item)
  expect encode_midgard_address_witness(witness) == item
  if subject.direction == substrate.direction_wrongful_rejection && !verify_ed25519_signature(
    witness.verification_key,
    subject.transaction_id,
    witness.signature,
  ) {
    None
  } else {
    Some(utils.get_verification_key_hash(witness.verification_key))
  }
}
