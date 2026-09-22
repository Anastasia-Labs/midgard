use aiken/builtin
use aiken/primitive/bytearray
use midgard/cek_data_frame_v1
use midgard/cek_data_v1
use midgard/validation_merkle_v1

fn summary(
  fill: Int,
  cbor_length: Int,
  memory: Int,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 {
    root: builtin.replicate_byte(32, fill),
    cbor_length,
    memory,
  }
}

test large_constructor_frame_is_fixed_size_and_matches_typescript() {
  let frame = cek_data_frame_v1.initial_large_constr_frame_v1(
    builtin.replicate_byte(32, 199),
    16_384,
    16_388,
    builtin.replicate_byte(32, 66),
    2,
  )
  let expected_encoding = #"8b01005820"
    |> bytearray.concat(builtin.replicate_byte(32, 199))
    |> bytearray.concat(#"1940001940045820")
    |> bytearray.concat(builtin.replicate_byte(32, 66))
    |> bytearray.concat(
        #"020080008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa56000000",
      )
  and {
    cek_data_frame_v1.encode_frame_v1(frame) == expected_encoding,
    bytearray.length(expected_encoding) < 256,
    cek_data_frame_v1.hash_frame_v1(frame) == #"f1f01b15e143b47b513a5be7c071a57709fa88b183a9220a9a81a1307b5334db",
  }
}

test folds_large_constructor_children_in_authenticated_reverse_order() {
  let child_0 = summary(17, 3, 5)
  let child_1 = summary(34, 67, 68)
  let initial = cek_data_frame_v1.initial_large_constr_frame_v1(
    builtin.replicate_byte(32, 199),
    16_384,
    16_388,
    builtin.replicate_byte(32, 66),
    2,
  )
  expect Some(with_0) = cek_data_frame_v1.append_child_v1(initial, child_0)
  expect Some(full) = cek_data_frame_v1.append_child_v1(with_0, child_1)
  let leaf_0 = cek_data_frame_v1.child_leaf_hash_v1(0, child_0)
  let leaf_1 = cek_data_frame_v1.child_leaf_hash_v1(1, child_1)
  expect Some(folded_1) = cek_data_frame_v1.fold_list_child_v1(
    full,
    1,
    child_1,
    [leaf_0],
  )
  expect Some(folded_0) = cek_data_frame_v1.fold_list_child_v1(
    folded_1,
    0,
    child_0,
    [leaf_1],
  )
  let fields = cek_data_v1.prepend_data_list_summary_v1(
    child_0,
    cek_data_v1.prepend_data_list_summary_v1(
      child_1,
      cek_data_v1.empty_data_list_summary_v1(),
    ),
  )
  expect Some(actual) = cek_data_frame_v1.finalized_summary_v1(folded_0)
  let expected = cek_data_v1.large_constr_data_summary_from_cbor_v1(
    builtin.replicate_byte(32, 199),
    16_384,
    16_388,
    fields,
  )
  actual == expected
}

test folds_map_pairs_without_reordering() {
  let key_0 = summary(16, 1, 5)
  let value_0 = summary(17, 2, 6)
  let key_1 = summary(32, 3, 7)
  let value_1 = summary(33, 4, 8)
  let leaf_0 = cek_data_frame_v1.child_leaf_hash_v1(0, key_0)
  let leaf_1 = cek_data_frame_v1.child_leaf_hash_v1(1, value_0)
  let leaf_2 = cek_data_frame_v1.child_leaf_hash_v1(2, key_1)
  let leaf_3 = cek_data_frame_v1.child_leaf_hash_v1(3, value_1)
  let branch_0 = validation_merkle_v1.hash_branch(leaf_0, leaf_1)
  let branch_1 = validation_merkle_v1.hash_branch(leaf_2, leaf_3)
  let initial = cek_data_frame_v1.initial_map_frame_v1(#"", 4)
  expect Some(with_0) = cek_data_frame_v1.append_child_v1(initial, key_0)
  expect Some(with_1) = cek_data_frame_v1.append_child_v1(with_0, value_0)
  expect Some(with_2) = cek_data_frame_v1.append_child_v1(with_1, key_1)
  expect Some(full) = cek_data_frame_v1.append_child_v1(with_2, value_1)
  expect Some(folded_1) = cek_data_frame_v1.fold_map_pair_v1(
    full,
    1,
    key_1,
    value_1,
    [leaf_3, branch_0],
    [leaf_2, branch_0],
  )
  expect Some(folded_0) = cek_data_frame_v1.fold_map_pair_v1(
    folded_1,
    0,
    key_0,
    value_0,
    [leaf_1, branch_1],
    [leaf_0, branch_1],
  )
  let entries = cek_data_v1.prepend_data_pair_summary_v1(
    key_0,
    value_0,
    cek_data_v1.prepend_data_pair_summary_v1(
      key_1,
      value_1,
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
  )
  expect Some(actual) = cek_data_frame_v1.finalized_summary_v1(folded_0)
  actual == cek_data_v1.map_data_summary_v1(entries)
}

test exact_empty_frames_finalize_without_forged_sequence_roots() {
  let list = cek_data_frame_v1.initial_list_frame_v1(#"", 0)
  let map = cek_data_frame_v1.initial_map_frame_v1(#"", 0)
  let constr = cek_data_frame_v1.initial_small_constr_frame_v1(127, #"", 0)
  expect Some(list_summary) = cek_data_frame_v1.finalized_summary_v1(list)
  expect Some(map_summary) = cek_data_frame_v1.finalized_summary_v1(map)
  expect Some(constr_summary) = cek_data_frame_v1.finalized_summary_v1(constr)
  and {
    list_summary == cek_data_v1.list_data_summary_v1(
      cek_data_v1.empty_data_list_summary_v1(),
    ),
    map_summary == cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    constr_summary == cek_data_v1.small_constr_data_summary_v1(
      127,
      cek_data_v1.empty_data_list_summary_v1(),
    ),
  }
}

test forged_or_unbounded_frames_fail_closed() {
  let initial = cek_data_frame_v1.initial_list_frame_v1(#"", 2)
  let forged_empty = cek_data_frame_v1.DataFrameV1 {
    ..initial,
    sequence: cek_data_v1.DataSequenceSummaryV1 {
      ..initial.sequence,
      root: builtin.replicate_byte(32, 255),
    },
  }
  let unbounded = cek_data_frame_v1.DataFrameV1 {
    ..initial,
    expected_children: 4_294_967_296,
  }
  and {
    !cek_data_frame_v1.frame_is_well_formed(forged_empty),
    !cek_data_frame_v1.frame_is_well_formed(unbounded),
    cek_data_frame_v1.append_child_v1(initial, summary(68, 0, 4)) == None,
    cek_data_frame_v1.finalized_summary_v1(initial) == None,
  }
}
