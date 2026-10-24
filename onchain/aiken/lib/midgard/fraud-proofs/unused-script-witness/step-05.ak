use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_script_witness/rule.{
  PurposeOpeningV1, ReverseScanV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  openings: List<PurposeOpeningV1>,
  item_budget: Int,
}

pub type Datum =
  ct.StepDatum<ReverseScanV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
