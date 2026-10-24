use midgard/computation_thread as ct
use midgard/fraud_proofs/missing_script_source/rule.{
  SourceDescriptorV1, SourceScanStateV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  sources: List<SourceDescriptorV1>,
  item_budget: Int,
}

pub type Datum =
  ct.StepDatum<SourceScanStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
