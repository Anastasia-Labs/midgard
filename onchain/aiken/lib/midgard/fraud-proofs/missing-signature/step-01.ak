use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}

pub type Datum =
  ct.StepDatum<Data>

/// The witness-set *preimage* no longer travels through step-01: step-04 opens
/// field 7 through the §8.8 door, which re-derives it from whatever §8 carriage
/// the prover chose. The witness-set *hash* still does travel — not as a
/// redeemer argument, but as the half of the §2.5 anchor step-01 writes into
/// `step_02.State`, because §3's transaction id does not commit it.
pub type Args =
  NativeTxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
