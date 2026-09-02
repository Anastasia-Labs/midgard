use midgard/computation_thread as ct
use midgard/fraud_proofs/distinct_asset_accumulation_limit/rule.{FoldStateV1}
use midgard/validation_machine_v1.{ValueAssetMutationWitnessV1}

pub type MintEvidenceV1 {
  mint_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
}

pub type ActionV1 {
  Skip { input_index: Int, output_index: Int }
  Authenticate { input_index: Int, output_index: Int, evidence: MintEvidenceV1 }
}

pub type Datum =
  ct.StepDatum<FoldStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>
