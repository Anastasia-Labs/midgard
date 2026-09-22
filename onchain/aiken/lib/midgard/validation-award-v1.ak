use midgard/computation_thread as ct
use midgard/validation_resolution_v1

pub type Datum =
  ct.StepDatum<validation_resolution_v1.WinningValidationResolutionStateV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
