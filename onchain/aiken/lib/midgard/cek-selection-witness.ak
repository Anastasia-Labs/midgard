use aiken/builtin
use midgard/bounded_item_v1
use midgard/validation_machine/machine_types.{
  NativeExecutionScanWitness, ValidationAuxiliaryWitnessV1,
}

pub fn decode_selection_auxiliary(data: Data) -> ValidationAuxiliaryWitnessV1 {
  expect
      Pair(11,
      [
        execution_index_data, language_tag_data, purpose_kind_data,
        purpose_index_data, script_hash_data, subject_data,
        purpose_siblings_data, source_index_data, origin_kind_data,
        source_key_data, script_total_length_data, script_item_commitment_data,
        source_siblings_data, redeemer_leaf_data, execution_siblings_data,
        first_chunk_proof_data,
      ])
    = builtin.un_constr_data(data)
  expect execution_index: Int = execution_index_data
  expect language_tag: Int = language_tag_data
  expect purpose_kind: Int = purpose_kind_data
  expect purpose_index: Int = purpose_index_data
  expect script_hash: ByteArray = script_hash_data
  expect subject: ByteArray = subject_data
  expect purpose_siblings: List<ByteArray> = purpose_siblings_data
  expect source_index: Int = source_index_data
  expect origin_kind: Int = origin_kind_data
  expect source_key: ByteArray = source_key_data
  expect script_total_length: Int = script_total_length_data
  expect script_item_commitment: ByteArray = script_item_commitment_data
  expect source_siblings: List<ByteArray> = source_siblings_data
  expect redeemer_leaf: ByteArray = redeemer_leaf_data
  expect execution_siblings: List<ByteArray> = execution_siblings_data
  expect first_chunk_proof: bounded_item_v1.ChunkProofV1 = first_chunk_proof_data
  NativeExecutionScanWitness {
    execution_index,
    language_tag,
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
    purpose_siblings,
    source_index,
    origin_kind,
    source_key,
    script_total_length,
    script_item_commitment,
    source_siblings,
    redeemer_leaf,
    execution_siblings,
    first_chunk_proof,
  }
}
