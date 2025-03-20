pub type Proof =
  ByteArray

pub type MembershipProofInputs {
  tx_root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  tx_to_root_proof_path: Proof,
}

pub type ExclusionProofInputTx {
  tx_root: ByteArray,
  invalid_input_tx_hash: ByteArray,
  invalid_input_info_hash: ByteArray,
  input_to_root_proof_path: Proof,
}

pub type ExclusionProofUtxo {
  utxo_root: ByteArray,
  invalid_utxo: ByteArray,
  invalid_utxo_info_hash: ByteArray,
  utxo_to_root_proof_path: Proof,
}

// Last step datum for fraud proofs
pub type FraudProofDatum {
  fraud_prover: ByteArray,
}
