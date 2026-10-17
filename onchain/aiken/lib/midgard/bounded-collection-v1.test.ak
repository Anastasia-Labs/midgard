use midgard/bounded_collection_v1
use midgard/validation_merkle_v1.{FrontierPeak}

const leaf_a =
  #"252f1f362ac201ba6803bcd0a22c3295bc562721b5e518caa65c966d306d4589"

const leaf_bb =
  #"c06b6c3d255adb5089eb6599c97027cec2b48f04e4b7bf6af3c6d2db2c7c95ab"

const leaf_ccc =
  #"4224350be47eecf833606847b9419d5e0555ac664ee7e9506c94ca516a799cad"

const pair_ab =
  #"c457e03d9d60de55d41e221961f6214696ad2fe8adb62e810ec7adc2feba1a54"

const collection_commitment =
  #"6b79e49eabb6aaa1e49ea511db08d438c16c022292394f5d782b7ab6cd6713d6"

const item_a =
  #"c4e238f2d3c5c3b535341bfcb66f449e2170ed3506d1f15bf7d496aa3dca97a9"

const item_bb =
  #"d0a6f8f5a8dfc2a6455d90dd6be9f18fbab15e0a9aef55003cb3af5e61404864"

const item_ccc =
  #"330b2e4f898b5dc2e191c9479ec5834c3109359bfd58ba7de6f9a8cd04119b93"

fn frontier() -> List<FrontierPeak> {
  [
    FrontierPeak { height: 0, hash: leaf_ccc },
    FrontierPeak { height: 1, hash: pair_ab },
  ]
}

test canonical_cross_language_collection_vector() {
  and {
    bounded_collection_v1.hash_item(2, 0, 1, item_a) == leaf_a,
    bounded_collection_v1.hash_item(2, 1, 2, item_bb) == leaf_bb,
    bounded_collection_v1.hash_item(2, 2, 3, item_ccc) == leaf_ccc,
    bounded_collection_v1.commitment(2, 3, frontier()) == collection_commitment,
  }
}

test exact_item_membership_is_accepted() {
  bounded_collection_v1.verify_item(
    collection_commitment,
    bounded_collection_v1.ItemProofV1 {
      version: bounded_collection_v1.version,
      field_index: 2,
      item_count: 3,
      item_index: 2,
      item_length: 3,
      item_commitment: item_ccc,
      frontier: frontier(),
      siblings: [],
    },
  )
}

test cross_field_replay_fails_closed() {
  !bounded_collection_v1.verify_item(
    collection_commitment,
    bounded_collection_v1.ItemProofV1 {
      version: bounded_collection_v1.version,
      field_index: 1,
      item_count: 3,
      item_index: 2,
      item_length: 3,
      item_commitment: item_ccc,
      frontier: frontier(),
      siblings: [],
    },
  )
}
