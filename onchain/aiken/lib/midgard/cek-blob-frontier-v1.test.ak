use aiken/builtin
use midgard/cek_blob_frontier_v1
use midgard/cek_proof_v1

test reproduces_a_left_balanced_three_leaf_blob_root() {
  let first = builtin.replicate_byte(4095, 106)
  let second = builtin.replicate_byte(4095, 106)
  let final = #"6a"
  let left = cek_proof_v1.hash_blob_branch_v1(
    cek_proof_v1.hash_blob_chunk_v1(first),
    cek_proof_v1.hash_blob_chunk_v1(second),
    8190,
  )
  let expected = cek_proof_v1.hash_blob_branch_v1(
    left,
    cek_proof_v1.hash_blob_chunk_v1(final),
    8191,
  )
  cek_blob_frontier_v1.root_from_chunks_v1([first, second, final])
    == Some(expected)
}

test encodes_the_typescript_three_leaf_frontier_canonically() {
  expect Some(first) = cek_blob_frontier_v1.append_chunk_v1(
    cek_blob_frontier_v1.empty_frontier_v1(),
    builtin.replicate_byte(4095, 106),
  )
  expect Some(second) = cek_blob_frontier_v1.append_chunk_v1(
    first,
    builtin.replicate_byte(4095, 106),
  )
  expect Some(terminal) = cek_blob_frontier_v1.append_chunk_v1(
    second,
    #"6a",
  )
  cek_blob_frontier_v1.encode_frontier_v1(terminal)
    == #"840103191fff8283005820344b2b0f0e31517cd429e9ed6bc07028defbc437648e4988fbd3f20c64f87d7b01830158203160ec563e32826cc3fc245286437e12ab09796f078e4780a8596b2a09995d8b191ffe"
}

test rejects_append_after_a_partial_final_leaf() {
  expect Some(first) = cek_blob_frontier_v1.append_chunk_v1(
    cek_blob_frontier_v1.empty_frontier_v1(),
    #"6a",
  )
  cek_blob_frontier_v1.append_chunk_v1(first, #"6a") == None
}
