use midgard/computation_thread as ct
use midgard/fraud_proofs/protected_output_signer_missing/rule.{VerdictV1}

pub type Datum =
  ct.StepDatum<VerdictV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
