use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/validation_merkle_v1

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

test validation_merkle_frontier_appends_and_proves_each_leaf() {
  let empty = validation_merkle_v1.empty_frontier()
  let one = validation_merkle_v1.append_leaf(0, empty, h_a)
  let two = validation_merkle_v1.append_leaf(1, one, h_b)
  let three = validation_merkle_v1.append_leaf(2, two, h_c)
  and {
    validation_merkle_v1.frontier_is_well_formed(3, three),
    validation_merkle_v1.verify_membership(3, three, 0, h_a, [h_b]),
    validation_merkle_v1.verify_membership(3, three, 1, h_b, [h_a]),
    validation_merkle_v1.verify_membership(3, three, 2, h_c, []),
    validation_merkle_v1.frontier_commitment(3, three) == #"f257467b03621e7d54b952ac6be9dd6b965bd9f86da60b367ec62b3eb1118ea0",
    validation_merkle_v1.frontier_commitment(3, three) != validation_merkle_v1.frontier_commitment(
      2,
      two,
    ),
  }
}

test validation_merkle_membership_rejects_a_wrong_sibling() {
  let empty = validation_merkle_v1.empty_frontier()
  let one = validation_merkle_v1.append_leaf(0, empty, h_a)
  let two = validation_merkle_v1.append_leaf(1, one, h_b)
  !validation_merkle_v1.verify_membership(2, two, 0, h_a, [blake2b_256(h_b)])
}

test validation_merkle_frontier_rejects_an_occupied_empty_slot() {
  !validation_merkle_v1.frontier_is_well_formed(
    0,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: h_a }],
  )
}

test batch_frontier_equals_checked_incremental_build(
  count via fuzz.int_between(0, 257),
) {
  let leaves = list.repeat(h_a, count)
  let (next_count, peaks) = list.foldl(
    leaves,
    (0, validation_merkle_v1.empty_frontier()),
    fn(leaf, acc) {
      let (i, previous) = acc
      (i + 1, validation_merkle_v1.append_leaf(i, previous, leaf))
    },
  )
  let batch = validation_merkle_v1.build_frontier(leaves)
  batch.count == next_count && batch.peaks == peaks && validation_merkle_v1.frontier_is_well_formed(
    batch.count,
    batch.peaks,
  )
}

test batch_frontier_rejects_malformed_leaf() fail {
  validation_merkle_v1.build_frontier([h_a, #"00"]).count == 2
}

test append_leaf_rejects_a_malformed_input_frontier() fail {
  // A 33-byte peak at count 1 would be merged by the carry if the input
  // frontier were not checked.
  let peak = validation_merkle_v1.FrontierPeak {
    height: 0,
    hash: #"00" |> bytearray.concat(h_a),
  }
  validation_merkle_v1.append_leaf(1, [peak], h_b) != []
}

fn seven_leaves() -> List<ByteArray> {
  [#"00", #"01", #"02", #"03", #"04", #"05", #"06"]
    |> list.map(blake2b_256)
}

test membership_at_every_peak_of_a_seven_leaf_frontier() {
  expect [l0, l1, l2, l3, l4, l5, l6] = seven_leaves()
  let built = validation_merkle_v1.build_frontier(seven_leaves())
  let left = validation_merkle_v1.hash_branch(l0, l1)
  let right = validation_merkle_v1.hash_branch(l2, l3)
  and {
    built.count == 7,
    validation_merkle_v1.verify_membership(7, built.peaks, 0, l0, [l1, right]),
    validation_merkle_v1.verify_membership(7, built.peaks, 3, l3, [l2, left]),
    validation_merkle_v1.verify_membership(7, built.peaks, 4, l4, [l5]),
    validation_merkle_v1.verify_membership(7, built.peaks, 5, l5, [l4]),
    validation_merkle_v1.verify_membership(7, built.peaks, 6, l6, []),
  }
}

test seven_leaf_membership_rejects_a_leaf_at_another_index() {
  expect [l0, l1, l2, l3, l4, l5, l6] = seven_leaves()
  let built = validation_merkle_v1.build_frontier(seven_leaves())
  let left = validation_merkle_v1.hash_branch(l0, l1)
  let right = validation_merkle_v1.hash_branch(l2, l3)
  and {
    !validation_merkle_v1.verify_membership(7, built.peaks, 1, l0, [l1, right]),
    !validation_merkle_v1.verify_membership(7, built.peaks, 2, l3, [l2, left]),
    !validation_merkle_v1.verify_membership(7, built.peaks, 5, l4, [l5]),
    !validation_merkle_v1.verify_membership(7, built.peaks, 4, l6, []),
    !validation_merkle_v1.verify_membership(7, built.peaks, 6, l5, [l4]),
  }
}
