use midgard/computation_thread as ct
use midgard/fraud_proofs/missing_script_source/rule.{
  AuthenticatedTransactionSourcesV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  resolved_reference_source_count: Int,
}

pub type Datum =
  ct.StepDatum<AuthenticatedTransactionSourcesV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
