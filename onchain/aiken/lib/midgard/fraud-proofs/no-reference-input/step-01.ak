use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}

pub type Datum =
  ct.StepDatum<Data>

pub type Args =
  NativeTxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
