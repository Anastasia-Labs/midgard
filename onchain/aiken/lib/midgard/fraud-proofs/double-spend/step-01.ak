use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

pub type State {
  verified_tx1_id: ByteArray,
  verified_tx1_spend_inputs_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args =
  NativeTxInclusionCarriage

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
