use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}

pub type Datum =
  ct.StepDatum<Data>

pub type Args =
  TxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
