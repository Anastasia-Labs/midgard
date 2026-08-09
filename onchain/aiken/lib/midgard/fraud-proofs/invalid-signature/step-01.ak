use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}

pub type Datum =
  ct.StepDatum<Data>

/// The witness-set *preimage* no longer travels through step-01: step-02 opens
/// field 7 through the §8.8 door, which re-derives it from whatever carriage the
/// prover chose. What step-01 still owes the thread is the witness-set *hash*,
/// which it alone can authenticate (§3's transaction id does not commit it) —
/// that goes into `step_02.State`, not into these arguments.
pub type Args =
  NativeTxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
