use midgard/computation_thread as ct
use midgard/fraud_proofs/distinct_asset_accumulation_limit/rule.{FoldStateV1}
use midgard/validation_machine_v1.{ValueAssetMutationWitnessV1}
use midgard/validation_merkle_v1.{FrontierPeak}

pub type InputEvidenceV1 {
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  asset_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  asset_peaks: List<FrontierPeak>,
  asset_siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
}

pub type ActionV1 {
  Skip { input_index: Int, output_index: Int }
  Authenticate {
    input_index: Int,
    output_index: Int,
    evidence: InputEvidenceV1,
  }
}

pub type Datum =
  ct.StepDatum<FoldStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>
