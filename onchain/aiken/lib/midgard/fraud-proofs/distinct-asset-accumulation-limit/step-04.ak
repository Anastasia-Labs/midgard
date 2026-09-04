use midgard/computation_thread as ct
use midgard/fraud_proofs/distinct_asset_accumulation_limit/rule.{FoldStateV1}
use midgard/validation_machine/machine_types.{ValueAssetMutationWitnessV1}
use midgard/validation_merkle_v1.{FrontierPeak}

pub type OutputEvidenceV1 {
  output_index: Int,
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
    evidence: OutputEvidenceV1,
  }
}

pub type Datum =
  ct.StepDatum<FoldStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>
