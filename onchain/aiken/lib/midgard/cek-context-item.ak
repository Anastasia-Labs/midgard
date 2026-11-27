use midgard/cek_data_v1.{DataSummaryV1}
use midgard/computation_thread as ct
use midgard/redeemer_item_proof_v1.{RedeemerItemProofControlV1}
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}

pub type ItemAction {
  VerifyItem {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
    claimed_next: RedeemerItemProofControlV1,
  }
}

pub type ItemRedeemer =
  ct.StepRedeemer<ItemAction>

pub type ItemHashResult {
  verified: Data,
  next_hash: ByteArray,
}

pub type ItemValueResult {
  verified: Data,
  summary: DataSummaryV1,
}
