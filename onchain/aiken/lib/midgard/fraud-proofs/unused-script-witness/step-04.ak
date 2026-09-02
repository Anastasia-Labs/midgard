use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_script_witness/rule.{
  ReverseScanV1, SourceOpeningV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  openings: List<SourceOpeningV1>,
}

pub type Datum =
  ct.StepDatum<ReverseScanV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
