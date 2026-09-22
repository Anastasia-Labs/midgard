use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/redeemer_item_proof_v1 as item
use midgard/validation_machine/machine_types.{
  RedeemerItemStepWitness, ValidationAuxiliaryWitnessV1,
}

pub type DescriptorControl {
  version: Int,
  mode: Int,
  stage: Int,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  expected_purpose_tag: Int,
  expected_pointer_index: Int,
  purpose_tag: Int,
  pointer_index: Int,
  data_offset: Int,
  data_length: Int,
  execution_memory: Int,
  execution_steps: Int,
}

pub type DescriptorStepClaim {
  control: DescriptorControl,
  open_tail: Bool,
  chunk_proof: ChunkProofV1,
  next_chunk_proof: Option<ChunkProofV1>,
  claimed_next: DescriptorControl,
}

pub fn full_control(
  control: DescriptorControl,
) -> item.RedeemerItemProofControlV1 {
  item.RedeemerItemProofControlV1 {
    version: control.version,
    mode: control.mode,
    stage: control.stage,
    item_index: control.item_index,
    item_count: control.item_count,
    total_length: control.total_length,
    item_commitment: control.item_commitment,
    expected_purpose_tag: control.expected_purpose_tag,
    expected_pointer_index: control.expected_pointer_index,
    purpose_tag: control.purpose_tag,
    pointer_index: control.pointer_index,
    data_offset: control.data_offset,
    data_length: control.data_length,
    execution_memory: control.execution_memory,
    execution_steps: control.execution_steps,
    traversal: None,
  }
}

pub fn auxiliary(claim: DescriptorStepClaim) -> ValidationAuxiliaryWitnessV1 {
  RedeemerItemStepWitness {
    redeemer_control: None,
    control: full_control(claim.control),
    witness: item.RedeemerItemProofWitnessV1 {
      action: if claim.open_tail {
        item.RedeemerItemOpenTail
      } else {
        item.RedeemerItemOpenHeader
      },
      chunk_proof: Some(claim.chunk_proof),
      next_chunk_proof: claim.next_chunk_proof,
    },
  }
}

pub fn narrow_control(
  control: item.RedeemerItemProofControlV1,
) -> DescriptorControl {
  expect control.traversal == None
  DescriptorControl {
    version: control.version,
    mode: control.mode,
    stage: control.stage,
    item_index: control.item_index,
    item_count: control.item_count,
    total_length: control.total_length,
    item_commitment: control.item_commitment,
    expected_purpose_tag: control.expected_purpose_tag,
    expected_pointer_index: control.expected_pointer_index,
    purpose_tag: control.purpose_tag,
    pointer_index: control.pointer_index,
    data_offset: control.data_offset,
    data_length: control.data_length,
    execution_memory: control.execution_memory,
    execution_steps: control.execution_steps,
  }
}
