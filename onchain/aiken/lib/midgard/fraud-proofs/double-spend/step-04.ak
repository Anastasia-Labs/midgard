use midgard/computation_thread as ct

pub type State {
  verified_tx2_spend_inputs_hash: ByteArray,
  double_spent_input_cbor: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  tx2_spend_input_cbors: List<ByteArray>,
  double_spent_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
