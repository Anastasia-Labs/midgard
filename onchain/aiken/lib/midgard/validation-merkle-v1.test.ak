use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/fuzz
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
