use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_script_witness/rule.{AuthenticatedWitnessV1}

pub type Args {
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<AuthenticatedWitnessV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
