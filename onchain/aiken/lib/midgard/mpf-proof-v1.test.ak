use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/mpf_proof_v1

const key = #"010203"

const value = #"040506"

const absent_key = #"070809"

const absent_value = #"0a0b0c"

fn singleton_root() -> ByteArray {
  combine(suffix(blake2b_256(key), 0), blake2b_256(value))
}

test mpf_proof_v1_matches_the_canonical_typescript_abi_vector() {
  let proof: mpf.Proof = [
    mpf.Branch { skip: 0, neighbors: #"aa" },
    mpf.Fork {
      skip: 1,
      neighbor: mpf.Neighbor { nibble: 2, prefix: #"bb", root: #"cc" },
    },
    mpf.Leaf { skip: 3, key: #"dd", value: #"ee" },
  ]
  cbor.serialise(proof) == #"9fd8799f0041aaffd87a9f01d8799f0241bb41ccffffd87b9f0341dd41eeffff"
}

test mpf_proof_v1_rejects_the_obsolete_double_wrapped_fork_neighbor() fail {
  expect Some(data) = cbor.deserialise(
    #"9fd87a9f01d8799fd8799f0241bb41ccffffffff",
  )
  expect [fork_data] = builtin.un_list_data(data)
  let Pair(fork_tag, fork_fields) = builtin.un_constr_data(fork_data)
  expect fork_tag == 1
  expect [_, neighbor_data] = fork_fields
  let Pair(neighbor_tag, neighbor_fields) = builtin.un_constr_data(
    neighbor_data,
  )
  expect neighbor_tag == 0
  expect [_, _, _] = neighbor_fields
  True
}

test mpf_proof_v1_verifies_empty_proof_membership() {
  mpf_proof_v1.has(singleton_root(), key, value, [])
}

test mpf_proof_v1_value_hash_membership_agrees_with_the_library() {
  let absent_path = blake2b_256(absent_key)
  let divergent_leaf = mpf.Leaf {
    skip: 0,
    key: absent_path,
    value: blake2b_256(absent_value),
  }
  let two_leaf_root = mpf_proof_v1.has(singleton_root(), key, value, [])
  and {
    two_leaf_root,
    mpf_proof_v1.has_value_hash(singleton_root(), key, blake2b_256(value), []),
    // A wrong digest, a wrong root and a non-32-byte digest all fail closed.
    !mpf_proof_v1.has_value_hash(
      singleton_root(),
      key,
      blake2b_256(absent_value),
      [],
    ),
    !mpf_proof_v1.has_value_hash(null_hash, key, blake2b_256(value), []),
    !mpf_proof_v1.has_value_hash(singleton_root(), key, #"0102", []),
    // And the one-step shape agrees with `has` on both verdicts.
    mpf_proof_v1.has_value_hash(
      singleton_root(),
      key,
      blake2b_256(value),
      [divergent_leaf],
    ) == mpf_proof_v1.has(singleton_root(), key, value, [divergent_leaf]),
  }
}

test mpf_proof_v1_verifies_empty_trie_non_membership() {
  mpf_proof_v1.does_not_have(null_hash, key, [])
}

test mpf_proof_v1_rejects_singleton_present_as_non_membership() {
  !mpf_proof_v1.does_not_have(
    singleton_root(),
    key,
    [mpf.Leaf { skip: 0, key: blake2b_256(key), value: blake2b_256(value) }],
  )
}

test mpf_proof_v1_rejects_same_branch_terminal_fork_as_non_membership() {
  let path = blake2b_256(key)
  let same_branch = nibble(path, 0)
  let proof = [
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: same_branch,
        prefix: #"",
        root: null_hash,
      },
    },
  ]
  !mpf_proof_v1.does_not_have(
    combine(bytearray.push(#"", same_branch), null_hash),
    key,
    proof,
  )
}

test mpf_proof_v1_accepts_a_divergent_terminal_leaf_non_membership() {
  let query_path = blake2b_256(key)
  let absent_path = blake2b_256(absent_key)
  let proof = [
    mpf.Leaf { skip: 0, key: absent_path, value: blake2b_256(absent_value) },
  ]
  and {
    nibble(query_path, 0) != nibble(absent_path, 0),
    mpf_proof_v1.does_not_have(
      combine(suffix(absent_path, 0), blake2b_256(absent_value)),
      key,
      proof,
    ),
  }
}

test mpf_proof_v1_accepts_a_divergent_terminal_fork_non_membership() {
  let path = blake2b_256(key)
  let same_branch = nibble(path, 0)
  let different_branch = if same_branch == 15 {
    0
  } else {
    same_branch + 1
  }
  let proof = [
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: different_branch,
        prefix: #"",
        root: null_hash,
      },
    },
  ]
  mpf_proof_v1.does_not_have(
    combine(bytearray.push(#"", different_branch), null_hash),
    key,
    proof,
  )
}

test mpf_proof_v1_returns_false_for_wrong_roots() {
  and {
    !mpf_proof_v1.has(null_hash, key, value, []),
    !mpf_proof_v1.does_not_have(singleton_root(), key, []),
  }
}

test mpf_proof_v1_rejects_a_malformed_fork_without_aborting() {
  let path = blake2b_256(key)
  let same_branch = nibble(path, 0)
  let malformed = [
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: same_branch,
        prefix: #"",
        root: null_hash,
      },
    },
  ]
  and {
    !mpf_proof_v1.has(null_hash, key, value, malformed),
    !mpf_proof_v1.does_not_have(null_hash, key, malformed),
  }
}

test mpf_proof_v1_rejects_oversized_skips_and_bad_branch_bytes() {
  and {
    !mpf_proof_v1.has(
      null_hash,
      key,
      value,
      [
        mpf.Fork {
          skip: 64,
          neighbor: mpf.Neighbor { nibble: 1, prefix: #"", root: null_hash },
        },
      ],
    ),
    !mpf_proof_v1.does_not_have(
      null_hash,
      key,
      [mpf.Branch { skip: 0, neighbors: #"00" }],
    ),
  }
}

test mpf_proof_v1_total_root_mutations_do_not_call_partial_paths_first() {
  let updated_value = #"070809"
  let updated_root = combine(
    suffix(blake2b_256(key), 0),
    blake2b_256(updated_value),
  )
  and {
    mpf_proof_v1.insert_root(null_hash, key, value, []) == Some(
      singleton_root(),
    ),
    mpf_proof_v1.insert_root(singleton_root(), key, value, []) == None,
    mpf_proof_v1.update_root(singleton_root(), key, value, updated_value, []) == Some(
      updated_root,
    ),
    mpf_proof_v1.update_root(null_hash, key, value, updated_value, []) == None,
    mpf_proof_v1.delete_root(singleton_root(), key, value, []) == Some(
      null_hash,
    ),
    mpf_proof_v1.delete_root(null_hash, key, value, []) == None,
  }
}
