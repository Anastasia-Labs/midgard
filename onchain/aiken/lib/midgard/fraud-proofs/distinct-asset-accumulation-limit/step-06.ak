use midgard/computation_thread as ct
use midgard/fraud_proofs/distinct_asset_accumulation_limit/rule.{FoldStateV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<FoldStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
