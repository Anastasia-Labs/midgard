use aiken/crypto.{blake2b_256}
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
