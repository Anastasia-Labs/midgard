use aiken/builtin.{blake2b_256, slice_bytearray}
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{
  merkle_16, null_hash, sparse_merkle_16,
}
use aiken/primitive/bytearray

/// Canonical Midgard V1 MPF proof helpers.
const digest_byte_count: Int = 32

const path_nibble_count: Int = 64

/// A Midgard V1 MPF path is 64 nibbles, so a well-formed proof can never carry
/// more than one step per nibble. This is the protocol-wide ceiling on the
/// number of steps any single proof may contain, however it is carried.
pub const maximum_proof_step_count: Int = 64

fn neighbor_is_well_formed(neighbor: mpf.Neighbor) -> Bool {
  and {
    neighbor.nibble >= 0,
    neighbor.nibble < 16,
    bytearray.length(neighbor.prefix) <= digest_byte_count,
    bytearray.length(neighbor.root) == digest_byte_count,
  }
}

fn common_step_is_well_formed(
  cursor: Int,
  skip: Int,
  remaining_step_count: Int,
) -> Bool {
  and {
    cursor >= 0,
    skip >= 0,
    cursor + 1 + skip <= path_nibble_count,
    remaining_step_count >= 0,
    remaining_step_count <= maximum_proof_step_count,
  }
}

fn membership_proof_is_well_formed(
  path: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
  remaining_step_count: Int,
) -> Bool {
  when proof is {
    [] -> and {
        cursor >= 0,
        cursor <= path_nibble_count,
      }
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(neighbors) == 4 * digest_byte_count,
        membership_proof_is_well_formed(
          path,
          next_cursor,
          steps,
          remaining_step_count - 1,
        ),
      }
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
        membership_proof_is_well_formed(
          path,
          next_cursor,
          steps,
          remaining_step_count - 1,
        ),
      }
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
        membership_proof_is_well_formed(
          path,
          next_cursor,
          steps,
          remaining_step_count - 1,
        ),
      }
    }
  }
}

fn non_membership_proof_is_well_formed(
  path: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
  remaining_step_count: Int,
) -> Bool {
  when proof is {
    [] -> and {
        cursor >= 0,
        cursor <= path_nibble_count,
      }
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(neighbors) == 4 * digest_byte_count,
        non_membership_proof_is_well_formed(
          path,
          next_cursor,
          steps,
          remaining_step_count - 1,
        ),
      }
    }
    [mpf.Fork { skip, neighbor }] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      }
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
        non_membership_proof_is_well_formed(
          path,
          next_cursor,
          steps,
          remaining_step_count - 1,
        ),
      }
    }
    [mpf.Leaf { skip, key, value }] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
      }
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, cursor),
        non_membership_proof_is_well_formed(
          path,
          next_cursor,
          steps,
          remaining_step_count - 1,
        ),
      }
    }
  }
}

fn do_branch(
  path: ByteArray,
  cursor: Int,
  next_cursor: Int,
  root: ByteArray,
  neighbors: ByteArray,
) -> ByteArray {
  let branch = nibble(path, next_cursor - 1)
  let prefix = nibbles(path, cursor, next_cursor - 1)
  combine(
    prefix,
    merkle_16(
      branch,
      root,
      slice_bytearray(0, digest_byte_count, neighbors),
      slice_bytearray(32, digest_byte_count, neighbors),
      slice_bytearray(64, digest_byte_count, neighbors),
      slice_bytearray(96, digest_byte_count, neighbors),
    ),
  )
}

fn do_fork(
  path: ByteArray,
  cursor: Int,
  next_cursor: Int,
  root: ByteArray,
  neighbor: mpf.Neighbor,
) -> ByteArray {
  let branch = nibble(path, next_cursor - 1)
  let prefix = nibbles(path, cursor, next_cursor - 1)
  combine(
    prefix,
    sparse_merkle_16(
      branch,
      root,
      neighbor.nibble,
      combine(neighbor.prefix, neighbor.root),
    ),
  )
}

fn do_excluding(path: ByteArray, cursor: Int, proof: mpf.Proof) -> ByteArray {
  when proof is {
    [] -> null_hash
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      let root = do_excluding(path, next_cursor, steps)
      do_branch(path, cursor, next_cursor, root, neighbors)
    }
    [mpf.Fork { neighbor, .. }] -> {
      let prefix = bytearray.push(neighbor.prefix, neighbor.nibble)
      combine(prefix, neighbor.root)
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      let root = do_excluding(path, next_cursor, steps)
      do_fork(path, cursor, next_cursor, root, neighbor)
    }
    [mpf.Leaf { key, value, .. }] -> combine(suffix(key, cursor), value)
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      let root = do_excluding(path, next_cursor, steps)
      let neighbor = mpf.Neighbor {
        prefix: suffix(key, next_cursor),
        nibble: nibble(key, cursor),
        root: value,
      }
      do_fork(path, cursor, next_cursor, root, neighbor)
    }
  }
}

/// Total, fail-closed membership verification for untrusted dispute witnesses.
pub fn has(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Bool {
  let path = blake2b_256(key)
  if and {
    bytearray.length(root) == digest_byte_count,
    membership_proof_is_well_formed(path, 0, proof, maximum_proof_step_count),
  } {
    mpf.has(mpf.from_root(root), key, value, proof)
  } else {
    False
  }
}

/// Mirror of the library's private `do_including`, specialised to take the
/// *hash* of the proved value instead of its preimage.
///
/// A published-chunk challenge authenticates the target value by its digest
/// (the preimage never reaches L1), so the walk has to start from the digest
/// directly. `mpf.has` hashes the value it is given and is therefore unusable
/// here. The recursion is otherwise identical to the library's, and
/// `mpf_proof_v1_value_hash_membership_agrees_with_the_library` pins that
/// equivalence.
fn do_including(
  path: ByteArray,
  value_hash: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
) -> ByteArray {
  when proof is {
    [] -> combine(suffix(path, cursor), value_hash)
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      let root = do_including(path, value_hash, next_cursor, steps)
      do_branch(path, cursor, next_cursor, root, neighbors)
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      let root = do_including(path, value_hash, next_cursor, steps)
      do_fork(path, cursor, next_cursor, root, neighbor)
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      let root = do_including(path, value_hash, next_cursor, steps)
      let neighbor = mpf.Neighbor {
        prefix: suffix(key, next_cursor),
        nibble: nibble(key, next_cursor - 1),
        root: value,
      }
      do_fork(path, cursor, next_cursor, root, neighbor)
    }
  }
}

/// Total, fail-closed membership verification against a 32-byte value digest.
///
/// Equivalent to `has(root, key, value, proof)` whenever
/// `value_hash == blake2b_256(value)`, with the same well-formedness gate.
pub fn has_value_hash(
  root: ByteArray,
  key: ByteArray,
  value_hash: ByteArray,
  proof: mpf.Proof,
) -> Bool {
  let path = blake2b_256(key)
  if and {
    bytearray.length(root) == digest_byte_count,
    bytearray.length(value_hash) == digest_byte_count,
    membership_proof_is_well_formed(path, 0, proof, maximum_proof_step_count),
  } {
    do_including(path, value_hash, 0, proof) == root
  } else {
    False
  }
}

/// Total, fail-closed non-membership verification. This deliberately exposes
/// the MPF `excluding` check as a Boolean instead of using `insert`, whose
/// invalid-proof branch aborts script evaluation.
pub fn does_not_have(root: ByteArray, key: ByteArray, proof: mpf.Proof) -> Bool {
  let path = blake2b_256(key)
  let well_formed = and {
    bytearray.length(root) == digest_byte_count,
    non_membership_proof_is_well_formed(
      path,
      0,
      proof,
      maximum_proof_step_count,
    ),
  }
  if well_formed {
    do_excluding(path, 0, proof) == root
  } else {
    False
  }
}

/// Returns a new root only after a total non-membership check has made the
/// library's partial `insert` precondition true.
pub fn insert_root(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  if does_not_have(root, key, proof) {
    Some(
      mpf.insert(mpf.from_root(root), key, value, proof)
        |> mpf.root,
    )
  } else {
    None
  }
}

/// Returns a new root only after a total membership check has made the
/// library's partial `update` precondition true.
pub fn update_root(
  root: ByteArray,
  key: ByteArray,
  old_value: ByteArray,
  new_value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  if has(root, key, old_value, proof) {
    Some(
      mpf.update(mpf.from_root(root), key, proof, old_value, new_value)
        |> mpf.root,
    )
  } else {
    None
  }
}

/// Returns a new root only after a total membership check has made the
/// library's partial `delete` precondition true.
pub fn delete_root(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  if has(root, key, value, proof) {
    Some(
      mpf.delete(mpf.from_root(root), key, value, proof)
        |> mpf.root,
    )
  } else {
    None
  }
}

pub fn proof_has_at_most_steps(proof: mpf.Proof, maximum: Int) -> Bool {
  and {
    maximum >= 0,
    list.length(proof) <= maximum,
  }
}
