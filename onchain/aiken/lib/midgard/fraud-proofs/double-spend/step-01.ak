use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

/// Only the verified transaction id travels. Step-03 re-opens tx1's field 0
/// through the §8.8 door from this id, so carrying the spend-inputs commitment
/// as well would be carrying a second, weaker name for the same thing — and
/// under §4's plain hashing a field commitment names no slot at all.
pub type State {
  verified_tx1_id: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args =
  NativeTxInclusionCarriage

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
