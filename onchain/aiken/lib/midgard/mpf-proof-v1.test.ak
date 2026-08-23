use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use env
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

// ## Empty-trie sentinel translation (#625)
//
// `does_not_have` translates `env.empty_merkle_tree_root` — the root Midgard
// commits for an empty MPF — to the library's own empty root before comparing
// (`library_root`), and `insert_root` folds in the same translation. These
// mirror the three selectors `validators/pexcludes.ak` already carries for the
// redeemer-carried route, because the two arms of the translation each need a
// positive: a positive on only one of them leaves the other free to be mutated
// silently.

/// A root the prover offers no proof for. `do_excluding(path, 0, [])` is the
/// library's empty root, which is not this, so it must be rejected — the
/// sentinel branch must not become a way to wave an arbitrary root through.
const unproven_root =
  #"1111111111111111111111111111111111111111111111111111111111111111"

/// A genuinely populated trie, borrowed verbatim from
/// `validators/pexcludes.ak`: the MPF library's own 29-element "fruits"
/// fixture with `apple[uid: 58]` removed, i.e. its `without_apple` root. The
/// library's `example_insert` selector proves this root and the witness below
/// are a real pair, so nothing here rests on this module's own arithmetic, and
/// a genuine multi-step absence witness cannot otherwise be written by hand.
const fruits_root =
  #"cb7812785c2f6b56e3dba69923e37625e94953257915d25f1041f43cefd5cb62"

/// The key absent from `fruits_root`.
const fruits_absent_key = "apple[uid: 58]"

/// Three steps — a branch and two leaf neighbours — so this exercises the real
/// recursion rather than the empty-proof base case the sentinel arm takes.
const fruits_absence_proof: mpf.Proof =
  [
    mpf.Branch {
      skip: 0,
      neighbors: #"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d79519b8cdfbd053e5a86cf28a781debae71638cd77f85aad4b88869373d9dcfd",
    },
    mpf.Leaf {
      skip: 0,
      key: #"5cddcd30a0a388cf6feb3fd6e112c96e9daf23e3a9c8a334e7044650471aaa9e",
      value: #"f429821ddf89c9df3c7fbb5aa6fadb6c246d75ceede53173ce59d70dde375d14",
    },
    mpf.Leaf {
      skip: 0,
      key: #"5e7ccfedd44c90423b191ecca1eb21dfbac865d561bace8c0f3e94ae7edf4440",
      value: #"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911",
    },
  ]

/// The sentinel arm, and the path #608 found dead on this route: a Midgard
/// header carries `env.empty_merkle_tree_root` for an empty MPF, and the empty
/// proof is the only witness an empty trie can produce. Before #625 this
/// answered `False`, so non-membership over an empty Midgard trie was
/// unprovable through every caller of this function.
test mpf_proof_v1_accepts_non_membership_against_the_empty_sentinel() {
  mpf_proof_v1.does_not_have(env.empty_merkle_tree_root, key, [])
}

/// The production arm: a populated trie reached through `mpf.from_root`, with
/// a real three-step absence witness. Every root but a genesis one takes this
/// arm, so without this selector a mutation collapsing the `else` branch onto
/// the sentinel would be silent under `aiken check`.
test mpf_proof_v1_accepts_non_membership_over_a_populated_trie() {
  mpf_proof_v1.does_not_have(
    fruits_root,
    fruits_absent_key,
    fruits_absence_proof,
  )
}

/// The translation stays a translation: an unproven root is still refused.
test mpf_proof_v1_rejects_an_unproven_root_as_non_membership() {
  !mpf_proof_v1.does_not_have(unproven_root, key, [])
}

/// `insert_root`'s fold-in, which is what keeps the opened gate from turning
/// into a live abort: `mpf.insert`'s own `excluding(key, proof) == self.root`
/// precondition would fail on exactly the sentinel roots `does_not_have` now
/// admits. The successor is the one-leaf root this module computes
/// independently from the MPF helpers, and is the same root the library's own
/// empty root produces — an insert never yields an empty trie, so the
/// successor is never itself the sentinel.
test mpf_proof_v1_inserts_from_the_empty_sentinel() {
  and {
    mpf_proof_v1.insert_root(env.empty_merkle_tree_root, key, value, []) == Some(
      singleton_root(),
    ),
    mpf_proof_v1.insert_root(env.empty_merkle_tree_root, key, value, []) == mpf_proof_v1.insert_root(
      null_hash,
      key,
      value,
      [],
    ),
    singleton_root() != env.empty_merkle_tree_root,
  }
}
