use midgard/bounded_item_v1
use midgard/validation_merkle_v1.{FrontierPeak}

const hello_chunk =
  #"8c61c7975b3faa9d5248d9dfd3ebd992ec7b0679c723f26841557a040c422f55"

const hello_commitment =
  #"3df275a9639180b74b52f354e0d9f9cd81a409a19e7b9ef6889a1b719db98cf4"

fn hello_frontier() -> List<FrontierPeak> {
  [FrontierPeak { height: 0, hash: hello_chunk }]
}

test canonical_cross_language_item_vector() {
  and {
    bounded_item_v1.hash_chunk(2, 7, 0, #"68656c6c6f") == hello_chunk,
    bounded_item_v1.commitment(2, 7, 5, hello_frontier()) == hello_commitment,
    bounded_item_v1.from_bytes(2, 7, #"68656c6c6f") == hello_commitment,
  }
}

test exact_chunk_membership_is_accepted() {
  bounded_item_v1.verify_chunk(
    hello_commitment,
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 2,
      item_index: 7,
      total_length: 5,
      chunk_index: 0,
      chunk: #"68656c6c6f",
      frontier: hello_frontier(),
      siblings: [],
    },
  )
}

test cross_item_replay_fails_closed() {
  !bounded_item_v1.verify_chunk(
    hello_commitment,
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index: 2,
      item_index: 8,
      total_length: 5,
      chunk_index: 0,
      chunk: #"68656c6c6f",
      frontier: hello_frontier(),
      siblings: [],
    },
  )
}
