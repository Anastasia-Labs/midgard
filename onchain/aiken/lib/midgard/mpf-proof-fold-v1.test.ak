use aiken/merkle_patricia_forestry.{Branch, Leaf}
use midgard/mpf_proof_fold_v1
use midgard/validation_merkle_v1

const first_neighbors =
  #"fcb3a266af3765b1034ed5b3aa4b4207a3326fe6c8d819833330688f2c789576f78acc03c7a5dfb5cfa9fea66cf3fee9122c7674c71ada7cb7fa0a4d63e3628facf6bc2599db2c5d6d299a15d5721ae11ee7fd7249ae8617b57509a74f2119bd756f6f99694666e954705f5a7b02b7750498dbeb4b889fb344e1d9bc59a9ed10"

const second_neighbors =
  #"235b143ef050a1b6bbea33803c7077088871d2ecc2f098591c4c34c7616ccb86c9f40a6f0e8c1d1017d7527c6efe204e479e449a5fed9a88311c2998c76352969ccc9dbea8cf853d71d324c571a99f08a9434bb3da9018c068e04e5c3f51841b0000000000000000000000000000000000000000000000000000000000000000"

fn first_frame() {
  mpf_proof_fold_v1.ProofFrameV1 {
    version: 1,
    frame_index: 0,
    cursor: 0,
    next_cursor: 1,
    step: Branch { skip: 0, neighbors: first_neighbors },
  }
}

fn second_frame() {
  mpf_proof_fold_v1.ProofFrameV1 {
    version: 1,
    frame_index: 1,
    cursor: 1,
    next_cursor: 2,
    step: Branch { skip: 0, neighbors: second_neighbors },
  }
}

fn third_frame() {
  mpf_proof_fold_v1.ProofFrameV1 {
    version: 1,
    frame_index: 2,
    cursor: 2,
    next_cursor: 3,
    step: Leaf {
      skip: 0,
      key: #"38c04ff63fbdbb3fc408a19f3831335596c6d145008c4ef6c089f43eeffc114f",
      value: #"7dec4f4f919b8eb0fa49dc74dc302781e288ef3e558fb6f97e314d5850269a84",
    },
  }
}

fn fourth_frame() {
  mpf_proof_fold_v1.ProofFrameV1 {
    version: 1,
    frame_index: 3,
    cursor: 3,
    next_cursor: 4,
    step: Leaf {
      skip: 0,
      key: #"38d8f7fe941c2b69bd72d0a30a310a3f1d207043e5afabf6e07baeb3f927ea6c",
      value: #"3a9cba86616d4ef370f645ed0f512dd5c2065f8d2de8417d1d4706c67896e62f",
    },
  }
}

fn descriptor() {
  mpf_proof_fold_v1.ProofDescriptorV1 {
    version: 1,
    frame_count: 4,
    terminal_cursor: 4,
    peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 2,
        hash: #"91c6884ab9f858a70432d5a302cad27c74e95c0470014c380204bcf55f5f4ac7",
      },
    ],
  }
}

test proof_frame_encoding_matches_typescript() {
  and {
    mpf_proof_fold_v1.proof_frame_leaf_hash_v1(first_frame()) == #"b1d2e32017535fb2d185011ce5b79b96512ee28661c415c76559a149f8d4f22b",
    mpf_proof_fold_v1.proof_frame_leaf_hash_v1(second_frame()) == #"dcb48b82764a667afbcf88c699636b9b7c16c8793b5707320df8130192265700",
    mpf_proof_fold_v1.proof_frame_leaf_hash_v1(third_frame()) == #"95f4251f61f4a1f2896f30b61a07852c2ba70c01af74211e252189ba46d05b5a",
    mpf_proof_fold_v1.proof_frame_leaf_hash_v1(fourth_frame()) == #"99ebc08dce5d1546a8628f8f4f5baec65fc0d85f7142002a7ca3d51fa7051b9e",
  }
}

test proof_fold_matches_real_typescript_trie_roots() {
  let key = #"616273656e742d323532"
  let value = #"696e7365727465642d76616c7565"
  let first_two_root = #"bb4b8c9161ab8382666814ad76a901b73306efc458d83c94f949b6ab3f6a179f"
  let last_two_root = #"5cecfab2d235824274f6ce0ad22daf43e517a060827432ac58b6634837989c46"
  expect Some(initial) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor(),
  )
  expect Some(after_fourth) = mpf_proof_fold_v1.fold_proof_frame_v1(
    key,
    descriptor(),
    initial,
    fourth_frame(),
    [
      #"95f4251f61f4a1f2896f30b61a07852c2ba70c01af74211e252189ba46d05b5a",
      first_two_root,
    ],
  )
  expect Some(after_third) = mpf_proof_fold_v1.fold_proof_frame_v1(
    key,
    descriptor(),
    after_fourth,
    third_frame(),
    [
      #"99ebc08dce5d1546a8628f8f4f5baec65fc0d85f7142002a7ca3d51fa7051b9e",
      first_two_root,
    ],
  )
  expect Some(after_second) = mpf_proof_fold_v1.fold_proof_frame_v1(
    key,
    descriptor(),
    after_third,
    second_frame(),
    [
      #"b1d2e32017535fb2d185011ce5b79b96512ee28661c415c76559a149f8d4f22b",
      last_two_root,
    ],
  )
  expect Some(terminal) = mpf_proof_fold_v1.fold_proof_frame_v1(
    key,
    descriptor(),
    after_second,
    first_frame(),
    [
      #"dcb48b82764a667afbcf88c699636b9b7c16c8793b5707320df8130192265700",
      last_two_root,
    ],
  )
  and {
    mpf_proof_fold_v1.fold_is_complete_v1(terminal),
    terminal.excluding_root == #"a636554b7a642f6a7cf97a544d21b858c3dab7450c2aa178b8279a91fae41541",
    terminal.including_root == #"341d675c5ea37d8fa4eed271f4d62f4546a441102c30271281f7ffceb6ea6300",
  }
}

test malformed_or_reordered_frame_fails_closed() {
  let key = #"616273656e742d323532"
  let value = #"696e7365727465642d76616c7565"
  expect Some(initial) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor(),
  )
  mpf_proof_fold_v1.fold_proof_frame_v1(
    key,
    descriptor(),
    initial,
    third_frame(),
    [],
  ) == None
}
