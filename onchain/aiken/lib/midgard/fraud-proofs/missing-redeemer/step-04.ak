use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{ScanStateV1}

pub type ActionV1 {
  Scan { args: ProgressArgsV1 }
  Finish { input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<ScanStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>
