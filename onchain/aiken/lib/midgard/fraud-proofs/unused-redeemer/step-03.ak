use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_redeemer/rule.{AuthenticatedRedeemerV1}

pub type Args {
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<AuthenticatedRedeemerV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
