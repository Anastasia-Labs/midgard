use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use midgard/mpf_proof_v1

const key = #"010203"

const value = #"040506"

fn singleton_root() -> ByteArray {
  combine(suffix(blake2b_256(key), 0), blake2b_256(value))
}

test mpf_proof_v1_verifies_empty_proof_membership() {
  mpf_proof_v1.has(singleton_root(), key, value, [])
}

test mpf_proof_v1_verifies_empty_trie_non_membership() {
  mpf_proof_v1.does_not_have(null_hash, key, [])
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
