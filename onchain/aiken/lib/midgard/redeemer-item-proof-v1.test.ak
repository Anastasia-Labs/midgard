use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/redeemer_item_proof_v1
use midgard/validation_merkle_v1

const small_redeemer = #"8400004100820a14"

fn singleton_chunk_proof(
  item: ByteArray,
  item_index: Int,
) -> bounded_item_v1.ChunkProofV1 {
  let leaf = bounded_item_v1.hash_chunk(8, item_index, 0, item)
  bounded_item_v1.ChunkProofV1 {
    version: 1,
    field_index: 8,
    item_index,
    total_length: bytearray.length(item),
    chunk_index: 0,
    chunk: item,
    frontier: validation_merkle_v1.build_frontier([leaf]).peaks,
    siblings: [],
  }
}

fn source_witness(
  action: redeemer_item_proof_v1.RedeemerItemProofActionV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
  redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action,
    chunk_proof: Some(proof),
    next_chunk_proof: None,
  }
}

test redeemer_item_descriptor_authenticates_header_tail_and_exact_metadata() {
  let proof = singleton_chunk_proof(small_redeemer, 0)
  let initial = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(small_redeemer),
    bounded_item_v1.from_bytes(8, 0, small_redeemer),
    0,
    0,
  )
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: tail })
    = redeemer_item_proof_v1.step_v1(
    initial,
    source_witness(redeemer_item_proof_v1.RedeemerItemOpenHeader, proof),
  )
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: terminal,
      })
    = redeemer_item_proof_v1.step_v1(
    tail,
    source_witness(redeemer_item_proof_v1.RedeemerItemOpenTail, proof),
  )
  expect Some(descriptor) = redeemer_item_proof_v1.descriptor_v1(terminal)
  and {
    terminal.stage == redeemer_item_proof_v1.stage_terminal,
    descriptor.item_index == 0,
    descriptor.item_count == 1,
    descriptor.total_length == bytearray.length(small_redeemer),
    descriptor.item_commitment == bounded_item_v1.from_bytes(
      8,
      0,
      small_redeemer,
    ),
    descriptor.purpose_tag == 0,
    descriptor.pointer_index == 0,
    descriptor.data_offset == 4,
    descriptor.data_length == 1,
    descriptor.execution_memory == 10,
    descriptor.execution_steps == 20,
  }
}

test redeemer_item_rejects_mutated_header_tail_and_chunk_evidence() {
  let commitment = bounded_item_v1.from_bytes(8, 0, small_redeemer)
  let initial = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(small_redeemer),
    commitment,
    0,
    0,
  )
  let wrong_header = #"8300004100820a14"
  let wrong_header_proof = singleton_chunk_proof(wrong_header, 0)
  let wrong_header_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(wrong_header),
    bounded_item_v1.from_bytes(8, 0, wrong_header),
    0,
    0,
  )
  expect Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) = redeemer_item_proof_v1.step_v1(
    wrong_header_control,
    source_witness(
      redeemer_item_proof_v1.RedeemerItemOpenHeader,
      wrong_header_proof,
    ),
  )
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: tail })
    = redeemer_item_proof_v1.step_v1(
    initial,
    source_witness(
      redeemer_item_proof_v1.RedeemerItemOpenHeader,
      singleton_chunk_proof(small_redeemer, 0),
    ),
  )
  let wrong_tail = #"8400004100830a1415"
  let wrong_tail_control = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    ..tail,
    total_length: bytearray.length(wrong_tail),
    item_commitment: bounded_item_v1.from_bytes(8, 0, wrong_tail),
  }
  expect Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) = redeemer_item_proof_v1.step_v1(
    wrong_tail_control,
    source_witness(
      redeemer_item_proof_v1.RedeemerItemOpenTail,
      singleton_chunk_proof(wrong_tail, 0),
    ),
  )
  and {
    redeemer_item_proof_v1.step_v1(
      initial,
      source_witness(
        redeemer_item_proof_v1.RedeemerItemOpenHeader,
        singleton_chunk_proof(#"8400004100820a15", 0),
      ),
    ) == None,
    redeemer_item_proof_v1.hash_control_v1(initial) != redeemer_item_proof_v1.hash_control_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..initial,
        item_count: 2,
      },
    ),
    redeemer_item_proof_v1.hash_control_v1(tail) != redeemer_item_proof_v1.hash_control_v1(
      redeemer_item_proof_v1.RedeemerItemProofControlV1 {
        ..tail,
        item_count: 2,
      },
    ),
  }
}

test redeemer_item_terminal_data_summary_agrees_with_typescript() {
  let terminal = redeemer_item_proof_v1.decode_control_v1(
    #"900101030001085820f7f0d1013b70311a09ce7afa0e0ff24d93f67af1ce449ce73e699c88058f93ed2020000004010a14d8799f8a010704010140d87a80d87a80d87a80d8799f8358200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ffff",
  )
  expect Some(summary) = redeemer_item_proof_v1.finalize_v1(terminal)
  expect Some(traversal) = terminal.traversal
  expect Some(result) = traversal.result
  let wrong_summary = cek_data_v1.DataSummaryV1 {
    ..result,
    root: #"5555555555555555555555555555555555555555555555555555555555555555",
  }
  let wrong_terminal = redeemer_item_proof_v1.RedeemerItemProofControlV1 {
    ..terminal,
    traversal: Some(
      cek_data_traverse_v1.DataTraverseControlV1 {
        ..traversal,
        result: Some(wrong_summary),
      },
    ),
  }
  and {
    terminal.mode == redeemer_item_proof_v1.mode_data,
    terminal.stage == redeemer_item_proof_v1.stage_terminal,
    terminal.data_offset == 4,
    terminal.data_length == 1,
    summary.root == #"0c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da2",
    summary.cbor_length == 1,
    summary.memory == 5,
    redeemer_item_proof_v1.control_is_well_formed(wrong_terminal),
    redeemer_item_proof_v1.hash_control_v1(wrong_terminal) != redeemer_item_proof_v1.hash_control_v1(
      terminal,
    ),
  }
}
