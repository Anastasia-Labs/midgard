use aiken/fuzz
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/redeemer_item_proof_v1
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_item_semantics as semantics
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

fn normalized_hash_is_exact(control_data: Data) -> Bool {
  expect control: redeemer_item_proof_v1.RedeemerItemProofControlV1 = control_data
  and {
    redeemer_item_proof_v1.control_is_well_formed(control),
    redeemer_item_proof_v1.hash_outer_with_checked_optional_traversal(
      control,
      normalized.checked_optional_traversal_cbor(control),
    ) == redeemer_item_proof_v1.hash_control_v1(control),
    redeemer_item_proof_v1.prevalidated_next_source_span(control) == redeemer_item_proof_v1.next_source_span_v1(
      control,
    ),
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
    redeemer_item_proof_v1.hash_descriptor_control_v1(tail) == redeemer_item_proof_v1.hash_control_v1(
      tail,
    ),
    redeemer_item_proof_v1.hash_descriptor_control_v1(terminal) == redeemer_item_proof_v1.hash_control_v1(
      terminal,
    ),
    redeemer_item_proof_v1.descriptor_step_v1(tail, True, proof, None) == Some(
      redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: terminal },
    ),
    normalized_hash_is_exact(initial),
    normalized_hash_is_exact(tail),
    normalized_hash_is_exact(terminal),
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
    #"900101030001085820f7f0d1013b70311a09ce7afa0e0ff24d93f67af1ce449ce73e699c88058f93ed2020000004010a14d8799f89010704010140d87a80d87a80d8799f8358200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ffff",
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
    normalized_hash_is_exact(terminal),
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

test data_item_narrow_header_tail_use_exact_authenticated_windows() {
  let proof = singleton_chunk_proof(small_redeemer, 0)
  let initial = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(small_redeemer),
    bounded_item_v1.from_bytes(8, 0, small_redeemer),
    -1,
    -1,
  )
  let header_witness = source_witness(
    redeemer_item_proof_v1.RedeemerItemOpenHeader,
    proof,
  )
  expect Some(Some(header_bytes)) = redeemer_item_proof_v1.authenticate_source_window(
    initial,
    redeemer_item_proof_v1.next_source_span_v1(initial),
    header_witness,
  )
  let header_result = redeemer_item_proof_v1.prevalidated_open_header(
    initial,
    header_bytes,
  )
  expect
    Some(header_result) == redeemer_item_proof_v1.step_v1(
      initial,
      header_witness,
    )
  expect redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: tail } = header_result
  let tail_witness = source_witness(
    redeemer_item_proof_v1.RedeemerItemOpenTail,
    proof,
  )
  expect Some(Some(tail_bytes)) = redeemer_item_proof_v1.authenticate_source_window(
    tail,
    redeemer_item_proof_v1.next_source_span_v1(tail),
    tail_witness,
  )
  let tail_result = redeemer_item_proof_v1.prevalidated_open_tail(
    tail,
    tail_bytes,
  )
  and {
    Some(tail_result) == redeemer_item_proof_v1.step_v1(tail, tail_witness),
    redeemer_item_proof_v1.authenticate_source_window(
      initial,
      redeemer_item_proof_v1.next_source_span_v1(initial),
      redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
        ..header_witness,
        next_chunk_proof: Some(proof),
      },
    ) == None,
    redeemer_item_proof_v1.authenticate_source_window(
      initial,
      redeemer_item_proof_v1.next_source_span_v1(initial),
      redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
        ..header_witness,
        chunk_proof: None,
      },
    ) == None,
  }
}

test initial_hash_matches_canonical_hash_for_both_modes_and_maximum_coordinates() {
  let commitment = #"1111111111111111111111111111111111111111111111111111111111111111"
  and {
    redeemer_item_proof_v1.initial_control_hash(0, 0, 1, 8, commitment, -1, -1) == redeemer_item_proof_v1.hash_control_v1(
      redeemer_item_proof_v1.initial_control_v1(0, 0, 1, 8, commitment, -1, -1),
    ),
    redeemer_item_proof_v1.initial_control_hash(
      1,
      16383,
      16384,
      32768,
      commitment,
      -1,
      -1,
    ) == redeemer_item_proof_v1.hash_control_v1(
      redeemer_item_proof_v1.initial_control_v1(
        1,
        16383,
        16384,
        32768,
        commitment,
        -1,
        -1,
      ),
    ),
  }
}

test descriptor_narrow_header_equals_generic(
  bytes via fuzz.bytearray_between(1, 128),
) {
  let proof = singleton_chunk_proof(bytes, 0)
  let initial = redeemer_item_proof_v1.initial_control_v1(
    0,
    0,
    1,
    bytearray.length(bytes),
    bounded_item_v1.from_bytes(8, 0, bytes),
    -1,
    -1,
  )
  and {
    redeemer_item_proof_v1.hash_descriptor_control_v1(initial) == redeemer_item_proof_v1.hash_control_v1(
      initial,
    ),
    redeemer_item_proof_v1.descriptor_step_v1(initial, False, proof, None) == redeemer_item_proof_v1.step_v1(
      initial,
      source_witness(redeemer_item_proof_v1.RedeemerItemOpenHeader, proof),
    ),
    redeemer_item_proof_v1.descriptor_step_v1(initial, True, proof, None) == None,
  }
}

fn authenticated_data_head(
  item: ByteArray,
) -> redeemer_item_proof_v1.RedeemerItemProofControlV1 {
  let proof = singleton_chunk_proof(item, 0)
  let initial = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(item),
    bounded_item_v1.from_bytes(8, 0, item),
    -1,
    -1,
  )
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: tail })
    = redeemer_item_proof_v1.step_v1(
    initial,
    source_witness(redeemer_item_proof_v1.RedeemerItemOpenHeader, proof),
  )
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: head })
    = redeemer_item_proof_v1.step_v1(
    tail,
    source_witness(redeemer_item_proof_v1.RedeemerItemOpenTail, proof),
  )
  head
}

test data_head_refusal_authenticates_noncanonical_constructor_and_refuses_bad_views() {
  let item = #"84000044d8798101820a14"
  let head = authenticated_data_head(item)
  let proof = singleton_chunk_proof(item, 0)
  let witness = source_witness(
    redeemer_item_proof_v1.RedeemerItemTraverseData {
      action: cek_data_traverse_v1.NoAction,
    },
    proof,
  )
  expect Some(traversal) = head.traversal
  and {
    redeemer_item_proof_v1.step_v1(head, witness) == Some(
      redeemer_item_proof_v1.RedeemerItemProofInvalid,
    ),
    cek_data_traverse_v1.unreadable_v1(
      traversal,
      Some(#"d8798101"),
      cek_data_traverse_v1.NoAction,
    ),
    !cek_data_traverse_v1.unreadable_v1(
      traversal,
      Some(#"d87981"),
      cek_data_traverse_v1.NoAction,
    ),
    !cek_data_traverse_v1.unreadable_v1(
      traversal,
      Some(#"d879810182"),
      cek_data_traverse_v1.NoAction,
    ),
    redeemer_item_proof_v1.step_v1(
      head,
      redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
        ..witness,
        chunk_proof: None,
      },
    ) == None,
    redeemer_item_proof_v1.step_v1(
      head,
      redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
        ..witness,
        chunk_proof: Some(singleton_chunk_proof(#"84000044d8799f01820a14", 0)),
      },
    ) == None,
    redeemer_item_proof_v1.step_v1(
      head,
      source_witness(
        redeemer_item_proof_v1.RedeemerItemTraverseData {
          action: cek_data_traverse_v1.HeadScalar,
        },
        proof,
      ),
    ) == None,
  }
}

test data_head_refusal_cannot_reject_honest_constructor_under_wrong_action_or_view() {
  let item = #"84000045d8799f01ff820a14"
  let head = authenticated_data_head(item)
  let proof = singleton_chunk_proof(item, 0)
  expect Some(traversal) = head.traversal
  expect Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { .. }) = redeemer_item_proof_v1.step_v1(
    head,
    source_witness(
      redeemer_item_proof_v1.RedeemerItemTraverseData {
        action: cek_data_traverse_v1.HeadSequence,
      },
      proof,
    ),
  )
  and {
    !cek_data_traverse_v1.unreadable_v1(
      traversal,
      Some(#"d8799f01ff"),
      cek_data_traverse_v1.NoAction,
    ),
    !cek_data_traverse_v1.unreadable_v1(
      traversal,
      Some(#"d87981"),
      cek_data_traverse_v1.NoAction,
    ),
    redeemer_item_proof_v1.step_v1(
      head,
      source_witness(
        redeemer_item_proof_v1.RedeemerItemTraverseData {
          action: cek_data_traverse_v1.NoAction,
        },
        proof,
      ),
    ) == None,
    redeemer_item_proof_v1.step_v1(
      head,
      source_witness(
        redeemer_item_proof_v1.RedeemerItemTraverseData {
          action: cek_data_traverse_v1.HeadScalar,
        },
        proof,
      ),
    ) == None,
  }
}

/// 256 bytes, byte i being i.
const byte_ramp =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"

fn repeat_bytes(bytes: ByteArray, count: Int) -> ByteArray {
  if count <= 0 {
    #""
  } else {
    bytearray.concat(bytes, repeat_bytes(bytes, count - 1))
  }
}

/// Two chunks: 4,095 bytes and 5 bytes, byte i being i mod 256.
fn two_chunk_item() -> ByteArray {
  repeat_bytes(byte_ramp, 16) |> bytearray.concat(#"00010203")
}

/// The proofs of a two-chunk item's chunks, in chunk order.
fn two_chunk_proofs(item: ByteArray) -> List<bounded_item_v1.ChunkProofV1> {
  let chunks = [
    bytearray.slice(item, 0, bounded_item_v1.chunk_bytes - 1),
    bytearray.slice(
      item,
      bounded_item_v1.chunk_bytes,
      bytearray.length(item) - 1,
    ),
  ]
  expect [first, second] = chunks
  let leaves = [
    bounded_item_v1.hash_chunk(8, 0, 0, first),
    bounded_item_v1.hash_chunk(8, 0, 1, second),
  ]
  expect [first_leaf, second_leaf] = leaves
  let frontier = validation_merkle_v1.build_frontier(leaves).peaks
  let proof = fn(chunk_index, chunk, sibling) {
    bounded_item_v1.ChunkProofV1 {
      version: 1,
      field_index: 8,
      item_index: 0,
      total_length: bytearray.length(item),
      chunk_index,
      chunk,
      frontier,
      siblings: [sibling],
    }
  }
  [proof(0, first, second_leaf), proof(1, second, first_leaf)]
}

fn data_control(
  item: ByteArray,
) -> redeemer_item_proof_v1.RedeemerItemProofControlV1 {
  redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(item),
    bounded_item_v1.from_bytes(8, 0, item),
    -1,
    -1,
  )
}

/// The window of `length` bytes at `start`, authenticated with `proofs` as the
/// chunk proof and the next chunk proof.
fn window(
  control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  start: Int,
  length: Int,
) -> Option<Option<ByteArray>> {
  when proofs is {
    [] -> window_with(control, None, None, start, length)
    [first] -> window_with(control, Some(first), None, start, length)
    [first, next, ..] ->
      window_with(control, Some(first), Some(next), start, length)
  }
}

fn window_with(
  control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
  chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  start: Int,
  length: Int,
) -> Option<Option<ByteArray>> {
  redeemer_item_proof_v1.authenticate_source_window(
    control,
    Some(
      redeemer_item_proof_v1.RedeemerItemSourceSpanV1 {
        absolute_start: start,
        length,
      },
    ),
    redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
      action: redeemer_item_proof_v1.RedeemerItemFinishData,
      chunk_proof,
      next_chunk_proof,
    },
  )
}

// The source-window vectors are the same as the ones of
// demo/midgard-core/tests/redeemer-item-proof-empty-span.test.ts.

test empty_source_span_inside_item_is_authenticated() {
  let small = data_control(small_redeemer)
  let large = data_control(two_chunk_item())
  and {
    window(small, [], 0, 0) == Some(Some(#"")),
    window(small, [], 3, 0) == Some(Some(#"")),
    window(small, [], 8, 0) == Some(Some(#"")),
    window(large, [], 4095, 0) == Some(Some(#"")),
    window(large, [], 4096, 0) == Some(Some(#"")),
    window(large, [], 4100, 0) == Some(Some(#"")),
  }
}

test empty_source_span_outside_item_or_with_other_evidence_is_refused() {
  let small = data_control(small_redeemer)
  let small_proof = singleton_chunk_proof(small_redeemer, 0)
  let item = two_chunk_item()
  let large = data_control(item)
  expect [chunk_0, chunk_1] = two_chunk_proofs(item)
  and {
    window(small, [], 9, 0) == None,
    window(small, [], -1, 0) == None,
    window(small, [small_proof], 3, 0) == None,
    window(small, [small_proof, small_proof], 3, 0) == None,
    window_with(small, None, Some(small_proof), 3, 0) == None,
    window(large, [chunk_0], 4095, 0) == None,
    window(large, [chunk_1], 4096, 0) == None,
    window(large, [], 4101, 0) == None,
  }
}

test non_empty_source_span_is_read_as_before() {
  let small = data_control(small_redeemer)
  let small_proof = [singleton_chunk_proof(small_redeemer, 0)]
  let item = two_chunk_item()
  let large = data_control(item)
  expect [chunk_0, chunk_1] = two_chunk_proofs(item)
  and {
    window(small, small_proof, 3, 2) == Some(Some(#"4100")),
    window(small, small_proof, 3, -1) == None,
    window(small, [], 3, -1) == None,
    window(small, small_proof, 7, 2) == None,
    window(large, [chunk_0, chunk_1], 4090, 10) == Some(
      Some(bytearray.slice(item, 4090, 4099)),
    ),
    window(large, [chunk_0], 4090, 10) == None,
    window(large, [chunk_1], 4095, 1) == Some(Some(#"ff")),
  }
}

/// The step that reads the empty content of an empty byte string advances to
/// the control TypeScript builds, and the dispute's source window and bytes
/// executor prove it.
fn empty_content_step_agrees(
  control_cbor: ByteArray,
  expected_next_hash: ByteArray,
) -> Bool {
  let control = redeemer_item_proof_v1.decode_control_v1(control_cbor)
  expect Some(span) = redeemer_item_proof_v1.next_source_span_v1(control)
  let witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemTraverseData {
      action: cek_data_traverse_v1.NoAction,
    },
    chunk_proof: None,
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next })
    = redeemer_item_proof_v1.step_v1(control, witness)
  expect Some(source_bytes) = redeemer_item_proof_v1.authenticate_source_window(
    control,
    redeemer_item_proof_v1.prevalidated_next_source_span(control),
    witness,
  )
  expect Some(current_traversal) = control.traversal
  expect Some(next_traversal) = next.traversal
  let attestation: Data = 0
  and {
    span.length == 0,
    source_bytes == Some(#""),
    redeemer_item_proof_v1.hash_control_v1(next) == expected_next_hash,
    semantics.advance_bytes(
      normalized.TraversalExecution {
        output: normalized.ExecutionOutput {
          deployment_id: #"",
          family: 7,
          executor_script_hash: #"",
          settlement_script_hash: #"",
          attestation,
        },
        current: current_traversal,
        next: next_traversal,
        action: cek_data_traverse_v1.NoAction,
        source_bytes,
      },
    ),
  }
}

test empty_byte_string_data_step_agrees_with_typescript() {
  // The redeemer item [0, 0, B"", [10, 20]], its Data a definite byte string.
  let item = #"8400004140820a14"
  let control_cbor = #"90010102000108582072d5071985075018e0da2b0b4fc5ed3c0db54aef7895e312f8782ea6460c569e2020000004010a14d8799f89010204010040d87a80d8799f860101040100d8799f86010000008401000080d8799f8901000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffd87a80ff"
  and {
    empty_content_step_agrees(
      control_cbor,
      #"3b1546b684b0eb942e067ab368ca50190737bd0e0842bcfd65979bf22b596097",
    ),
    redeemer_item_proof_v1.step_v1(
      redeemer_item_proof_v1.decode_control_v1(control_cbor),
      source_witness(
        redeemer_item_proof_v1.RedeemerItemTraverseData {
          action: cek_data_traverse_v1.NoAction,
        },
        singleton_chunk_proof(item, 0),
      ),
    ) == None,
  }
}

test empty_byte_string_at_chunk_boundary_agrees_with_typescript() {
  // A list of 61 64-byte strings and 61 zeros, then B"", whose empty content
  // starts at byte 4,095, the first of chunk 1.
  empty_content_step_agrees(
    #"9001010200011910035820b8ae18328fc1499016cc8505485b4917c1668a0c86c24d6bc2b73a1745362c8e2020000006190ffa0a14d8799f89010206190ffa190ff85820cac4b33b7a2eda9a1950c137bd6b202c83d4541740fb9c0ac35a6bac2f4784c9d87a80d8799f860101190ffe0100d8799f86010000008401000080d8799f8901000016584028c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b40004000ffffffd87a80ff",
    #"a1f3f8a9cdf293e7955bf519ff1e2afc9c1b7c136bc468d098f1afcbf045c28d",
  )
}

/// A step with no source span takes no chunk proof.
test no_span_step_refuses_a_chunk_proof() {
  // The FinishData step of the redeemer item [0, 0, B"00", [10, 20]].
  let control = redeemer_item_proof_v1.decode_control_v1(
    #"900101020001085820f7f0d1013b70311a09ce7afa0e0ff24d93f67af1ce449ce73e699c88058f93ed2020000004010a14d8799f89010704010140d87a80d87a80d8799f8358200c4e8e7a8396d11dd8c0554ec32416c3257d87175f2dfcc7714469663e0f1da20105ffff",
  )
  let proof = singleton_chunk_proof(small_redeemer, 0)
  let witness = fn(chunk_proof, next_chunk_proof) {
    redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
      action: redeemer_item_proof_v1.RedeemerItemFinishData,
      chunk_proof,
      next_chunk_proof,
    }
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next })
    = redeemer_item_proof_v1.step_v1(control, witness(None, None))
  and {
    redeemer_item_proof_v1.next_source_span_v1(control) == None,
    redeemer_item_proof_v1.hash_control_v1(next) == #"26131db89798fb41a38a53b8216f34d12feb7c2465a1391a33e6655d332370dc",
    redeemer_item_proof_v1.step_v1(control, witness(Some(proof), None)) == None,
    redeemer_item_proof_v1.step_v1(control, witness(None, Some(proof))) == None,
  }
}
