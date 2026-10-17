use midgard/computation_thread as ct
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{CanonicalVerdictV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<CanonicalVerdictV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
