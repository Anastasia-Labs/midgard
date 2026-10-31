use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{ScanStateV1}

pub type Datum =
  ct.StepDatum<ScanStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ProgressArgsV1>
