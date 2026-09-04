use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/mint_declared_asset_limit/rule.{FoldStateV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  item_budget: Int,
}

pub type Datum =
  ct.StepDatum<FoldStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
