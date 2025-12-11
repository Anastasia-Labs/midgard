pub type Datum {
  fraud_prover: ByteArray,
}

/// The spending validator of a fraud proof token never allows it to be spent.
/// Midgard fraud proof tokens last forever.
pub type SpendRedeemer =
  Never

pub type MintRedeemer {
  hub_oracle_ref_input_index: Int,
  fraud_proof_last_step_input_index: Int,
  computation_thread_token_asset_name: ByteArray,
  output_with_fraud_proof_index: Int,
  hub_oracle_asset_name: ByteArray,
}
