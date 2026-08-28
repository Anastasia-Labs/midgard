use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

pub type Datum =
  ct.StepDatum<Data>

pub type Args =
  NativeTxInclusionCarriage

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
