use midgard/computation_thread as ct
use midgard/fraud_proofs/mint_item_non_canonical/rule.{MintScanStateV1}

pub type Datum =
  ct.StepDatum<MintScanStateV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  window: ByteArray,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
