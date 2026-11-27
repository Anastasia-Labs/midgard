use midgard/computation_thread as ct
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  OutputScanStateV1,
}

pub type Datum =
  ct.StepDatum<OutputScanStateV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  window: ByteArray,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
