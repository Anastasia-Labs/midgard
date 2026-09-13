use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/validation_merkle_v1

fn empty_list_frame(expected_children: Int) -> cek_data_scan_v1.DataScanFrameV1 {
  cek_data_scan_v1.DataScanFrameV1 {
    kind: cek_data_scan_v1.list_frame,
    constructor: 0,
    tail: #"",
    expected_children,
    child_count: 0,
    child_peaks: validation_merkle_v1.empty_frontier(),
    fold_cursor: 0,
    sequence: cek_data_v1.empty_data_list_summary_v1(),
  }
}

fn empty_map_frame(expected_children: Int) -> cek_data_scan_v1.DataScanFrameV1 {
  cek_data_scan_v1.DataScanFrameV1 {
    kind: cek_data_scan_v1.map_frame,
    constructor: 0,
    tail: #"",
    expected_children,
    child_count: 0,
    child_peaks: validation_merkle_v1.empty_frontier(),
    fold_cursor: 0,
    sequence: cek_data_v1.empty_data_pair_summary_v1(),
  }
}

test list_children_fold_to_the_exact_semantic_data_root() {
  let first_data = builtin.i_data(1)
  let second_data = builtin.b_data(#"abcd")
  let first = cek_data_v1.semantic_data_summary_v1(first_data)
  let second = cek_data_v1.semantic_data_summary_v1(second_data)
  expect Some(with_first) = cek_data_scan_v1.append_child_v1(
    empty_list_frame(2),
    first,
  )
  expect Some(complete) = cek_data_scan_v1.append_child_v1(with_first, second)
  expect Some(folded_second) = cek_data_scan_v1.fold_list_child_v1(
    complete,
    1,
    second,
    [cek_data_scan_v1.child_leaf_hash_v1(0, first)],
  )
  expect Some(folded_first) = cek_data_scan_v1.fold_list_child_v1(
    folded_second,
    0,
    first,
    [cek_data_scan_v1.child_leaf_hash_v1(1, second)],
  )
  expect Some(summary) = cek_data_scan_v1.finalized_summary_v1(
    folded_first,
    None,
  )
  summary == cek_data_v1.semantic_data_summary_v1(
    builtin.list_data([first_data, second_data]),
  )
}

test map_pair_folds_to_the_exact_semantic_data_root() {
  let key_data = builtin.b_data(#"01")
  let value_data = builtin.i_data(-7)
  let key = cek_data_v1.semantic_data_summary_v1(key_data)
  let value = cek_data_v1.semantic_data_summary_v1(value_data)
  expect Some(with_key) = cek_data_scan_v1.append_child_v1(
    empty_map_frame(2),
    key,
  )
  expect Some(complete) = cek_data_scan_v1.append_child_v1(with_key, value)
  expect Some(folded) = cek_data_scan_v1.fold_map_pair_v1(
    complete,
    0,
    key,
    value,
    [cek_data_scan_v1.child_leaf_hash_v1(1, value)],
    [cek_data_scan_v1.child_leaf_hash_v1(0, key)],
  )
  expect Some(summary) = cek_data_scan_v1.finalized_summary_v1(folded, None)
  summary == cek_data_v1.semantic_data_summary_v1(
    builtin.map_data([Pair(key_data, value_data)]),
  )
}

test frame_hash_binds_the_parent_stack_link() {
  let frame = empty_list_frame(0)
  cek_data_scan_v1.hash_frame_v1(frame) != cek_data_scan_v1.hash_frame_v1(
    cek_data_scan_v1.DataScanFrameV1 {
      ..frame,
      tail: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    },
  )
}

test raw_list_cursor_authenticates_each_leaf_and_exact_break() {
  let raw = #"9f0142abcdff"
  let raw_hash = blake2b_256(raw)
  expect Some(opened) = cek_data_scan_v1.open_list_at_v1(
    raw,
    raw_hash,
    0,
    2,
    #"",
  )
  expect opened.next_offset == 1
  expect Some(first) = cek_data_scan_v1.small_leaf_at_v1(
    raw,
    raw_hash,
    1,
    #"01",
  )
  expect Some(second) = cek_data_scan_v1.small_leaf_at_v1(
    raw,
    raw_hash,
    first.next_offset,
    #"42abcd",
  )
  expect Some(after_break) = cek_data_scan_v1.close_sequence_at_v1(
    raw,
    raw_hash,
    second.next_offset,
    cek_data_scan_v1.DataScanFrameV1 {
      ..opened.frame,
      child_count: 2,
      child_peaks: validation_merkle_v1.build_frontier(
        [
          cek_data_scan_v1.child_leaf_hash_v1(0, first.summary),
          cek_data_scan_v1.child_leaf_hash_v1(1, second.summary),
        ],
      ).peaks,
    },
  )
  after_break == 6
}

test raw_small_constructor_and_map_headers_are_exact() {
  let constructor_raw = #"d8799f01ff"
  let map_raw = #"a1410101"
  expect Some(constructor) = cek_data_scan_v1.open_small_constructor_at_v1(
    constructor_raw,
    blake2b_256(constructor_raw),
    0,
    0,
    1,
    #"",
  )
  expect Some(map) = cek_data_scan_v1.open_map_at_v1(
    map_raw,
    blake2b_256(map_raw),
    0,
    #"",
  )
  and {
    constructor.next_offset == 3,
    constructor.frame.expected_children == 1,
    map.next_offset == 1,
    map.frame.expected_children == 2,
  }
}

test completed_nested_frame_pops_into_its_authenticated_parent() {
  let parent = cek_data_scan_v1.DataScanFrameV1 {
    kind: cek_data_scan_v1.root_frame,
    constructor: 0,
    tail: #"",
    expected_children: 1,
    child_count: 0,
    child_peaks: validation_merkle_v1.empty_frontier(),
    fold_cursor: 0,
    sequence: cek_data_v1.empty_data_list_summary_v1(),
  }
  let child = cek_data_scan_v1.DataScanFrameV1 {
    ..empty_list_frame(0),
    tail: cek_data_scan_v1.hash_frame_v1(parent),
  }
  expect Some(summary) = cek_data_scan_v1.finalized_summary_v1(child, None)
  expect Some(next_parent) = cek_data_scan_v1.append_completed_child_v1(
    child,
    summary,
    parent,
  )
  expect Some(root_summary) = cek_data_scan_v1.finalized_summary_v1(
    next_parent,
    Some(summary),
  )
  root_summary == summary
}

test maximum_three_chunk_blob_root_matches_offchain_commitment() {
  cek_data_scan_v1.bounded_blob_root_v1(
    bytearray.from_int_big_endian(0, 4500)
      |> bytearray.concat(bytearray.from_int_big_endian(0, 4500)),
  ) == #"69f84362d34689aee864cdf9e81bc71922e2d496c32bf8a81b72bcec0aec2db8"
}

test revealed_large_bytes_leaf_uses_the_exact_chunked_semantic_root() {
  let bytes = bytearray.from_int_big_endian(0, 4400)
    |> bytearray.concat(bytearray.from_int_big_endian(0, 4400))
  let raw = cbor.serialise(builtin.b_data(bytes))
  expect bytearray.length(raw) < 9216
  expect Some(leaf) = cek_data_scan_v1.revealed_leaf_at_v1(
    raw,
    blake2b_256(raw),
    0,
    bytearray.length(raw),
  )
  and {
    leaf.next_offset == bytearray.length(raw),
    leaf.summary == cek_data_v1.bytes_data_summary_v1(
      8800,
      #"def1a3dfbf8dfb6376d606cce8ee5eba4bc52f26dcd492343ed72517cb9e65d3",
    ),
  }
}

test direct_and_streamed_large_bytes_use_the_same_semantic_root() {
  let bytes = bytearray.from_int_big_endian(0, 4400)
    |> bytearray.concat(bytearray.from_int_big_endian(0, 4400))
  let raw = cbor.serialise(builtin.b_data(bytes))
  expect Some(leaf) = cek_data_scan_v1.revealed_leaf_at_v1(
    raw,
    blake2b_256(raw),
    0,
    bytearray.length(raw),
  )
  leaf.summary == cek_data_v1.semantic_data_summary_v1(builtin.b_data(bytes))
}

test large_constructor_header_and_summary_match_direct_semantics() {
  let field = builtin.i_data(1)
  let data = builtin.constr_data(128, [field])
  let raw = cbor.serialise(data)
  let raw_hash = blake2b_256(raw)
  expect Some(opened) = cek_data_scan_v1.open_constructor_at_v1(
    raw,
    raw_hash,
    0,
    128,
    1,
    #"",
  )
  expect Some(leaf) = cek_data_scan_v1.small_leaf_at_v1(
    raw,
    raw_hash,
    opened.next_offset,
    cbor.serialise(field),
  )
  expect Some(complete) = cek_data_scan_v1.append_child_v1(
    opened.frame,
    leaf.summary,
  )
  expect Some(after_break) = cek_data_scan_v1.close_sequence_at_v1(
    raw,
    raw_hash,
    leaf.next_offset,
    complete,
  )
  expect Some(folded) = cek_data_scan_v1.fold_list_child_v1(
    complete,
    0,
    leaf.summary,
    [],
  )
  expect Some(summary) = cek_data_scan_v1.finalized_summary_v1(folded, None)
  and {
    after_break == bytearray.length(raw),
    summary == cek_data_v1.semantic_data_summary_v1(data),
  }
}

test nested_control_steps_reconstruct_the_exact_semantic_root() {
  let first_data = builtin.i_data(1)
  let second_data = builtin.i_data(2)
  let key_data = builtin.i_data(3)
  let value_data = builtin.i_data(4)
  let list_data = builtin.list_data([first_data, second_data])
  let map_data = builtin.map_data([Pair(key_data, value_data)])
  let data = builtin.constr_data(0, [list_data, map_data])
  let raw = cbor.serialise(data)
  let raw_hash = blake2b_256(raw)
  let initial = cek_data_scan_v1.initial_control_v1(
    raw_hash,
    bytearray.length(raw),
  )

  expect Some(after_root_open) = cek_data_scan_v1.open_constructor_control_step_v1(
    initial,
    raw,
    None,
    0,
    2,
  )
  expect Some(opened_root) = cek_data_scan_v1.open_constructor_at_v1(
    raw,
    raw_hash,
    0,
    0,
    2,
    #"",
  )
  let root_frame = opened_root.frame

  expect Some(after_list_open) = cek_data_scan_v1.open_list_control_step_v1(
    after_root_open,
    raw,
    Some(root_frame),
    2,
  )
  expect Some(opened_list) = cek_data_scan_v1.open_list_at_v1(
    raw,
    raw_hash,
    opened_root.next_offset,
    2,
    cek_data_scan_v1.hash_frame_v1(root_frame),
  )
  let list_frame = opened_list.frame
  let first_cbor = cbor.serialise(first_data)
  let second_cbor = cbor.serialise(second_data)
  let first_summary = cek_data_v1.semantic_data_summary_v1(first_data)
  let second_summary = cek_data_v1.semantic_data_summary_v1(second_data)

  expect Some(after_first) = cek_data_scan_v1.reveal_leaf_control_step_v1(
    after_list_open,
    raw,
    Some(list_frame),
    bytearray.length(first_cbor),
  )
  expect Some(list_with_first) = cek_data_scan_v1.append_child_v1(
    list_frame,
    first_summary,
  )
  expect Some(after_second) = cek_data_scan_v1.reveal_leaf_control_step_v1(
    after_first,
    raw,
    Some(list_with_first),
    bytearray.length(second_cbor),
  )
  expect Some(list_complete) = cek_data_scan_v1.append_child_v1(
    list_with_first,
    second_summary,
  )
  expect Some(after_list_close) = cek_data_scan_v1.close_sequence_control_step_v1(
    after_second,
    raw,
    list_complete,
  )
  expect Some(after_second_fold) = cek_data_scan_v1.fold_list_control_step_v1(
    after_list_close,
    list_complete,
    1,
    second_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(0, first_summary)],
  )
  expect Some(list_second_folded) = cek_data_scan_v1.fold_list_child_v1(
    list_complete,
    1,
    second_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(0, first_summary)],
  )
  expect Some(after_first_fold) = cek_data_scan_v1.fold_list_control_step_v1(
    after_second_fold,
    list_second_folded,
    0,
    first_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(1, second_summary)],
  )
  expect Some(list_folded) = cek_data_scan_v1.fold_list_child_v1(
    list_second_folded,
    0,
    first_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(1, second_summary)],
  )
  expect Some(list_summary) = cek_data_scan_v1.finalized_summary_v1(
    list_folded,
    None,
  )
  expect Some(after_list_pop) = cek_data_scan_v1.finalize_frame_control_step_v1(
    after_first_fold,
    list_folded,
    Some(root_frame),
  )
  expect Some(root_with_list) = cek_data_scan_v1.append_completed_child_v1(
    list_folded,
    list_summary,
    root_frame,
  )

  expect Some(after_map_open) = cek_data_scan_v1.open_map_control_step_v1(
    after_list_pop,
    raw,
    Some(root_with_list),
  )
  expect Some(opened_map) = cek_data_scan_v1.open_map_at_v1(
    raw,
    raw_hash,
    after_list_pop.offset,
    cek_data_scan_v1.hash_frame_v1(root_with_list),
  )
  let map_frame = opened_map.frame
  let key_cbor = cbor.serialise(key_data)
  let value_cbor = cbor.serialise(value_data)
  let key_summary = cek_data_v1.semantic_data_summary_v1(key_data)
  let value_summary = cek_data_v1.semantic_data_summary_v1(value_data)
  expect Some(after_key) = cek_data_scan_v1.reveal_leaf_control_step_v1(
    after_map_open,
    raw,
    Some(map_frame),
    bytearray.length(key_cbor),
  )
  expect Some(map_with_key) = cek_data_scan_v1.append_child_v1(
    map_frame,
    key_summary,
  )
  expect Some(after_value) = cek_data_scan_v1.reveal_leaf_control_step_v1(
    after_key,
    raw,
    Some(map_with_key),
    bytearray.length(value_cbor),
  )
  expect after_value.frame_closed
  expect Some(map_complete) = cek_data_scan_v1.append_child_v1(
    map_with_key,
    value_summary,
  )
  expect Some(after_pair_fold) = cek_data_scan_v1.fold_map_control_step_v1(
    after_value,
    map_complete,
    0,
    key_summary,
    value_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(1, value_summary)],
    [cek_data_scan_v1.child_leaf_hash_v1(0, key_summary)],
  )
  expect Some(map_folded) = cek_data_scan_v1.fold_map_pair_v1(
    map_complete,
    0,
    key_summary,
    value_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(1, value_summary)],
    [cek_data_scan_v1.child_leaf_hash_v1(0, key_summary)],
  )
  expect Some(map_summary) = cek_data_scan_v1.finalized_summary_v1(
    map_folded,
    None,
  )
  expect Some(after_map_pop) = cek_data_scan_v1.finalize_frame_control_step_v1(
    after_pair_fold,
    map_folded,
    Some(root_with_list),
  )
  expect Some(root_complete) = cek_data_scan_v1.append_completed_child_v1(
    map_folded,
    map_summary,
    root_with_list,
  )

  expect Some(after_root_close) = cek_data_scan_v1.close_sequence_control_step_v1(
    after_map_pop,
    raw,
    root_complete,
  )
  expect Some(after_map_fold) = cek_data_scan_v1.fold_list_control_step_v1(
    after_root_close,
    root_complete,
    1,
    map_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(0, list_summary)],
  )
  expect Some(root_map_folded) = cek_data_scan_v1.fold_list_child_v1(
    root_complete,
    1,
    map_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(0, list_summary)],
  )
  expect Some(after_list_fold) = cek_data_scan_v1.fold_list_control_step_v1(
    after_map_fold,
    root_map_folded,
    0,
    list_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(1, map_summary)],
  )
  expect Some(root_folded) = cek_data_scan_v1.fold_list_child_v1(
    root_map_folded,
    0,
    list_summary,
    [cek_data_scan_v1.child_leaf_hash_v1(1, map_summary)],
  )
  expect Some(completed) = cek_data_scan_v1.finalize_frame_control_step_v1(
    after_list_fold,
    root_folded,
    None,
  )
  and {
    completed.offset == bytearray.length(raw),
    completed.frame_root == #"",
    !completed.frame_closed,
    completed.result == cek_data_v1.semantic_data_summary_v1(data),
  }
}

test cross_language_data_scan_control_hash_vectors() {
  let raw = #"d8799f9f0102ffa10342abcdff"
  let data = builtin.constr_data(
    0,
    [
      builtin.list_data([builtin.i_data(1), builtin.i_data(2)]),
      builtin.map_data([Pair(builtin.i_data(3), builtin.b_data(#"abcd"))]),
    ],
  )
  let initial = cek_data_scan_v1.initial_control_v1(
    blake2b_256(raw),
    bytearray.length(raw),
  )
  let terminal = cek_data_scan_v1.DataScanControlV1 {
    ..initial,
    offset: bytearray.length(raw),
    result: cek_data_v1.semantic_data_summary_v1(data),
  }
  and {
    cek_data_scan_v1.hash_control_v1(initial) == #"6b5258f74c54a3932194e3087c5ce5652fb1bbffb042b71572aab47bea7d07e4",
    cek_data_scan_v1.hash_control_v1(terminal) == #"865b9c51d15222a3b33dbb422e212aa302d37e4a791268c0f22a8ff6b0a538fa",
  }
}
