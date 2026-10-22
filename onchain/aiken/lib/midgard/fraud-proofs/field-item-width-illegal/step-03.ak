use midgard/computation_thread as ct
use midgard/fraud_proofs/field_item_width_illegal/rule.{AuthenticatedItemV1}

pub type Datum =
  ct.StepDatum<AuthenticatedItemV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
