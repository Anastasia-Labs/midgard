pub type Proof =
  ByteArray

pub type MembershipProofInputs {
  tx_root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  tx_to_root_proof_path: Proof,
}

// Last step datum for fraud proofs
pub type FraudProofDatum {
  fraud_prover: ByteArray,
}
