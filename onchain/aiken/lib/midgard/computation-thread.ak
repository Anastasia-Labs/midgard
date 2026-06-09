use aiken/crypto.{VerificationKeyHash}

pub type StepDatum<a> {
  fraud_prover: VerificationKeyHash,
  data: Option<a>,
}

pub type StepRedeemer<a> {
  Cancel { input_index: Int, computation_thread_mint_redeemer_index: Int }
  Continue(a)
}

pub type MintRedeemer {
  Init {
    first_step_output_index: Int,
    fraud_proof_catalogue_ref_input_index: Int,
    hub_oracle_ref_input_index: Int,
    fraudulent_block_ref_input_index: Int,
  }
  Success { burning_token_asset_name: ByteArray }
  BurnForCancellation { burning_token_asset_name: ByteArray }
}
