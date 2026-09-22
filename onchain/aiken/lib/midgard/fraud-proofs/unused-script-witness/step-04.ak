use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_script_witness/rule.{
  ReverseScanV1, SourceOpeningV1,
}

/// One bounded batch of the alternate-source walk. The walk is resumable
/// because consensus admits far more inline field-6 scripts than one L1
/// transaction can open: an accused coordinate deep in the field is reached
/// through repeated self-loop batches of at most `maximum_scan_batch` openings.
pub type Args {
  input_index: Int,
  output_index: Int,
  openings: List<SourceOpeningV1>,
  item_budget: Int,
}

pub type Datum =
  ct.StepDatum<ReverseScanV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
