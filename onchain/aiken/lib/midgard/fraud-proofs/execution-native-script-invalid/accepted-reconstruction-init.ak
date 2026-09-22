use midgard/computation_thread as ct
use midgard/fraud_proofs/execution_native_script_invalid/rule.{BoundExecutionV1}

pub type Args {
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<BoundExecutionV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
