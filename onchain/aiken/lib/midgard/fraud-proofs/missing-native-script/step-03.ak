use midgard/computation_thread as ct

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
