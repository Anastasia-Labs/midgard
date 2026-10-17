use midgard/computation_thread as ct
use midgard/fraud_proofs/script_integrity_hash_mismatch/rule.{DecisionV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<DecisionV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
