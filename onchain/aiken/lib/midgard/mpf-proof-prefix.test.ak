use aiken/merkle_patricia_forestry as mpf
use midgard/mpf_proof_fold_v1 as fold
use midgard/mpf_proof_v1 as proof
use midgard/validation_merkle_v1 as merkle

// Captured from a canonical 1,304-asset trie. Deleting this unit leaves a
// terminal fork with skip=1 and an empty neighbor prefix. Both the JS trie
// deletion and its independently reconstructed exclusion proof give `after`.
const before =
  #"a3cb6c7cc11c8ab91629b7fa17089818c9751c5a9a0f256b0449acc17a6fb5b7"

const after =
  #"aae9218b732ed0dd511191290953ea78b87bf1d3a9e53fa0d2753fe678c9ba9b"

const dropped_prefix_root =
  #"c8aa9e6b96398745d8c5ec972fc6a0f3baaf7cbe154b9fcd5ae642d8ffa82f36"

const key = #"abababababababababababababababababababababababababababab001f"

const value = #"01"

fn opening() -> mpf.Proof {
  [
    mpf.Branch {
      skip: 0,
      neighbors: #"bd3871c02105e5ec24751ba8fb1a5e6d285cdcc8399993a0ca82b26f5ae179d65fed1c2681e29bbb4baed7e7d2a0f618637b48308f351ef786980f3dcdf001205318b031fecedfbfb987a7db1e2fcbb1c1d1c82dbf31e099558ddab0e96e3d38c73adabe2c5d1f9741c04a9dc2e856e29ecb6b96346905f7fb7b266b7865c857",
    },
    mpf.Branch {
      skip: 0,
      neighbors: #"42a0d1b9e50273451d2a93f195a000843d6a0c114672d0b5fadbf8fce300fb90b6be92c2f492c9bce1b8ecac7b530487537b2f448a80be834ad7a21df127a5468fe7d543b2434538a0c6d78fae96ed7f767ed36752ce7c1f54242d57eeaaf059cec5360efb71d228292cdf2b52f551f199b32bda593c4a4745a9b94aa17f8c9a",
    },
    mpf.Fork {
      skip: 1,
      neighbor: mpf.Neighbor {
        nibble: 8,
        prefix: #"",
        root: #"2f425ececd6ad91ad57238470324c4623ff0bcd1b51f6393ce0105874e651ccb",
      },
    },
  ]
}

test deletion_preserves_terminal_fork_shared_prefix() {
  and {
    proof.has(before, key, value, opening()),
    proof.delete_root(before, key, value, opening()) == Some(after),
    proof.delete_root_paired_fold(before, key, value, opening()) == Some(after),
    proof.delete_root_two_fold(before, key, value, opening()) == Some(after),
  }
}

test insertion_reverses_compressed_fork_deletion() {
  and {
    proof.does_not_have(after, key, opening()),
    proof.insert_root(after, key, value, opening()) == Some(before),
    proof.insert_root_paired_fold(after, key, value, opening()) == Some(before),
    proof.insert_root_two_fold(after, key, value, opening()) == Some(before),
  }
}

test dropped_prefix_is_not_a_valid_predecessor() {
  and {
    !proof.does_not_have(dropped_prefix_root, key, opening()),
    proof.insert_root(dropped_prefix_root, key, value, opening()) == None,
    proof.insert_root_paired_fold(dropped_prefix_root, key, value, opening()) == None,
    proof.insert_root_two_fold(dropped_prefix_root, key, value, opening()) == None,
  }
}

test resumable_frames_preserve_the_same_terminal_fork_prefix() {
  expect [a, b, c] = opening()
  let first = fold.ProofFrameV1 {
    version: 1,
    frame_index: 0,
    cursor: 0,
    next_cursor: 1,
    step: a,
  }
  let second = fold.ProofFrameV1 {
    version: 1,
    frame_index: 1,
    cursor: 1,
    next_cursor: 2,
    step: b,
  }
  let third = fold.ProofFrameV1 {
    version: 1,
    frame_index: 2,
    cursor: 2,
    next_cursor: 4,
    step: c,
  }
  let first_hash = fold.proof_frame_leaf_hash_v1(first)
  let second_hash = fold.proof_frame_leaf_hash_v1(second)
  let frontier = merkle.build_frontier(
    [first_hash, second_hash, fold.proof_frame_leaf_hash_v1(third)],
  )
  let descriptor = fold.ProofDescriptorV1 {
    version: 1,
    frame_count: 3,
    terminal_cursor: 4,
    peaks: frontier.peaks,
  }
  expect Some(initial) = fold.initial_fold_control_v1(key, value, descriptor)
  expect Some(after_third) = fold.fold_proof_frame_v1(
    key,
    descriptor,
    initial,
    third,
    [],
  )
  expect Some(after_second) = fold.fold_proof_frame_v1(
    key,
    descriptor,
    after_third,
    second,
    [first_hash],
  )
  expect Some(terminal) = fold.fold_proof_frame_v1(
    key,
    descriptor,
    after_second,
    first,
    [second_hash],
  )
  and {
    fold.fold_is_complete_v1(terminal),
    terminal.including_root == before,
    terminal.excluding_root == after,
  }
}
