use aiken/builtin.{blake2b_256, slice_bytearray}
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{
  merkle_16, null_hash, sparse_merkle_16,
}
use aiken/primitive/bytearray
use env

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

// Strict predecessor folds for the two-fold mutation helpers below. Each
// branch validates the untrusted proof step before it contributes to the
// reconstructed root, so validation and predecessor reconstruction share one
// traversal. The successor fold may then reuse the now-proven proof without a
// third walk.
fn do_excluding_strict(
  path: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
  remaining_step_count: Int,
) -> Option<ByteArray> {
  when proof is {
    [] ->
      if and {
        cursor >= 0,
        cursor <= path_nibble_count,
      } {
        Some(null_hash)
      } else {
        None
      }
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(neighbors) == 4 * digest_byte_count,
      } {
        when
          do_excluding_strict(
            path,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some(root) ->
            Some(do_branch(path, cursor, next_cursor, root, neighbors))
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        let prefix = bytearray.push(neighbor.prefix, neighbor.nibble)
        Some(combine(prefix, neighbor.root))
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        when
          do_excluding_strict(
            path,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some(root) -> Some(do_fork(path, cursor, next_cursor, root, neighbor))
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
      } {
        Some(combine(suffix(key, cursor), value))
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, cursor),
      } {
        when
          do_excluding_strict(
            path,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some(root) -> {
            let neighbor = mpf.Neighbor {
              prefix: suffix(key, next_cursor),
              nibble: nibble(key, cursor),
              root: value,
            }
            Some(do_fork(path, cursor, next_cursor, root, neighbor))
          }
          None -> None
        }
      } else {
        None
      }
    }
  }
}

fn do_including_strict(
  path: ByteArray,
  value_hash: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
  remaining_step_count: Int,
) -> Option<ByteArray> {
  when proof is {
    [] ->
      if and {
        cursor >= 0,
        cursor <= path_nibble_count,
      } {
        Some(combine(suffix(path, cursor), value_hash))
      } else {
        None
      }
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(neighbors) == 4 * digest_byte_count,
      } {
        when
          do_including_strict(
            path,
            value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some(root) ->
            Some(do_branch(path, cursor, next_cursor, root, neighbors))
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        when
          do_including_strict(
            path,
            value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some(root) -> Some(do_fork(path, cursor, next_cursor, root, neighbor))
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
      } {
        when
          do_including_strict(
            path,
            value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some(root) -> {
            let neighbor = mpf.Neighbor {
              prefix: suffix(key, next_cursor),
              nibble: nibble(key, next_cursor - 1),
              root: value,
            }
            Some(do_fork(path, cursor, next_cursor, root, neighbor))
          }
          None -> None
        }
      } else {
        None
      }
    }
  }
}

// Strict paired folds used by singleton state machines that must authenticate
// a predecessor and derive its exact successor in one bounded traversal. Each
// proof step is validated once and applies the same branch transform to both
// accumulators. The terminal Fork/Leaf arms deliberately preserve the
// forestry insert/delete semantics rather than treating a non-membership
// terminal as an ordinary recursive step.
fn do_insert_pair_strict(
  path: ByteArray,
  new_value_hash: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
  remaining_step_count: Int,
) -> Option<(ByteArray, ByteArray)> {
  when proof is {
    [] ->
      if and {
        cursor >= 0,
        cursor <= path_nibble_count,
      } {
        Some((null_hash, combine(suffix(path, cursor), new_value_hash)))
      } else {
        None
      }
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(neighbors) == 4 * digest_byte_count,
      } {
        when
          do_insert_pair_strict(
            path,
            new_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) ->
            Some(
              (
                do_branch(path, cursor, next_cursor, predecessor, neighbors),
                do_branch(path, cursor, next_cursor, successor, neighbors),
              ),
            )
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        let predecessor = combine(
          bytearray.push(neighbor.prefix, neighbor.nibble),
          neighbor.root,
        )
        let inserted = combine(suffix(path, next_cursor), new_value_hash)
        Some(
          (predecessor, do_fork(path, cursor, next_cursor, inserted, neighbor)),
        )
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        when
          do_insert_pair_strict(
            path,
            new_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) ->
            Some(
              (
                do_fork(path, cursor, next_cursor, predecessor, neighbor),
                do_fork(path, cursor, next_cursor, successor, neighbor),
              ),
            )
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
      } {
        let predecessor = combine(suffix(key, cursor), value)
        let inserted = combine(suffix(path, next_cursor), new_value_hash)
        let neighbor = mpf.Neighbor {
          prefix: suffix(key, next_cursor),
          nibble: nibble(key, next_cursor - 1),
          root: value,
        }
        Some(
          (predecessor, do_fork(path, cursor, next_cursor, inserted, neighbor)),
        )
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, cursor),
      } {
        when
          do_insert_pair_strict(
            path,
            new_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) -> {
            let predecessor_neighbor = mpf.Neighbor {
              prefix: suffix(key, next_cursor),
              nibble: nibble(key, cursor),
              root: value,
            }
            let successor_neighbor = mpf.Neighbor {
              prefix: suffix(key, next_cursor),
              nibble: nibble(key, next_cursor - 1),
              root: value,
            }
            Some(
              (
                do_fork(
                  path,
                  cursor,
                  next_cursor,
                  predecessor,
                  predecessor_neighbor,
                ),
                do_fork(
                  path,
                  cursor,
                  next_cursor,
                  successor,
                  successor_neighbor,
                ),
              ),
            )
          }
          None -> None
        }
      } else {
        None
      }
    }
  }
}

fn do_update_pair_strict(
  path: ByteArray,
  old_value_hash: ByteArray,
  new_value_hash: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
  remaining_step_count: Int,
) -> Option<(ByteArray, ByteArray)> {
  when proof is {
    [] ->
      if and {
        cursor >= 0,
        cursor <= path_nibble_count,
      } {
        Some(
          (
            combine(suffix(path, cursor), old_value_hash),
            combine(suffix(path, cursor), new_value_hash),
          ),
        )
      } else {
        None
      }
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(neighbors) == 4 * digest_byte_count,
      } {
        when
          do_update_pair_strict(
            path,
            old_value_hash,
            new_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) ->
            Some(
              (
                do_branch(path, cursor, next_cursor, predecessor, neighbors),
                do_branch(path, cursor, next_cursor, successor, neighbors),
              ),
            )
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        when
          do_update_pair_strict(
            path,
            old_value_hash,
            new_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) ->
            Some(
              (
                do_fork(path, cursor, next_cursor, predecessor, neighbor),
                do_fork(path, cursor, next_cursor, successor, neighbor),
              ),
            )
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
      } {
        when
          do_update_pair_strict(
            path,
            old_value_hash,
            new_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) -> {
            let neighbor = mpf.Neighbor {
              prefix: suffix(key, next_cursor),
              nibble: nibble(key, next_cursor - 1),
              root: value,
            }
            Some(
              (
                do_fork(path, cursor, next_cursor, predecessor, neighbor),
                do_fork(path, cursor, next_cursor, successor, neighbor),
              ),
            )
          }
          None -> None
        }
      } else {
        None
      }
    }
  }
}

fn do_delete_pair_strict(
  path: ByteArray,
  old_value_hash: ByteArray,
  cursor: Int,
  proof: mpf.Proof,
  remaining_step_count: Int,
) -> Option<(ByteArray, ByteArray)> {
  when proof is {
    [] ->
      if and {
        cursor >= 0,
        cursor <= path_nibble_count,
      } {
        Some((combine(suffix(path, cursor), old_value_hash), null_hash))
      } else {
        None
      }
    [mpf.Branch { skip, neighbors }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(neighbors) == 4 * digest_byte_count,
      } {
        when
          do_delete_pair_strict(
            path,
            old_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) ->
            Some(
              (
                do_branch(path, cursor, next_cursor, predecessor, neighbors),
                do_branch(path, cursor, next_cursor, successor, neighbors),
              ),
            )
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        let included = combine(suffix(path, next_cursor), old_value_hash)
        Some(
          (
            do_fork(path, cursor, next_cursor, included, neighbor),
            combine(
              bytearray.push(neighbor.prefix, neighbor.nibble),
              neighbor.root,
            ),
          ),
        )
      } else {
        None
      }
    }
    [mpf.Fork { skip, neighbor }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        neighbor_is_well_formed(neighbor),
        nibble(path, next_cursor - 1) != neighbor.nibble,
      } {
        when
          do_delete_pair_strict(
            path,
            old_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) ->
            Some(
              (
                do_fork(path, cursor, next_cursor, predecessor, neighbor),
                do_fork(path, cursor, next_cursor, successor, neighbor),
              ),
            )
          None -> None
        }
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
      } {
        let included = combine(suffix(path, next_cursor), old_value_hash)
        let neighbor = mpf.Neighbor {
          prefix: suffix(key, next_cursor),
          nibble: nibble(key, next_cursor - 1),
          root: value,
        }
        Some(
          (
            do_fork(path, cursor, next_cursor, included, neighbor),
            combine(suffix(key, cursor), value),
          ),
        )
      } else {
        None
      }
    }
    [mpf.Leaf { skip, key, value }, ..steps] -> {
      let next_cursor = cursor + 1 + skip
      if and {
        common_step_is_well_formed(cursor, skip, remaining_step_count),
        bytearray.length(key) == digest_byte_count,
        bytearray.length(value) == digest_byte_count,
        nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1),
      } {
        when
          do_delete_pair_strict(
            path,
            old_value_hash,
            next_cursor,
            steps,
            remaining_step_count - 1,
          )
        is {
          Some((predecessor, successor)) -> {
            let predecessor_neighbor = mpf.Neighbor {
              prefix: suffix(key, next_cursor),
              nibble: nibble(key, next_cursor - 1),
              root: value,
            }
            let successor_neighbor = mpf.Neighbor {
              prefix: suffix(key, next_cursor),
              nibble: nibble(key, cursor),
              root: value,
            }
            Some(
              (
                do_fork(
                  path,
                  cursor,
                  next_cursor,
                  predecessor,
                  predecessor_neighbor,
                ),
                do_fork(
                  path,
                  cursor,
                  next_cursor,
                  successor,
                  successor_neighbor,
                ),
              ),
            )
          }
          None -> None
        }
      } else {
        None
      }
    }
  }
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

/// The root the MPF library gives to the trie a Midgard root names.
///
/// Midgard commits `env.empty_merkle_tree_root = blake2b_256("")` for an empty
/// MPF — `state-queue.ak` forces a first block over a genesis confirmed state
/// to carry exactly that value as `prev_utxos_root` — while the library
/// reconstructs its empty trie as `merkling.null_hash` (32 zero bytes). The
/// sentinel therefore has to be translated before a reconstructed root is
/// compared against a Midgard-committed one.
///
/// This is the adapter `validators/pexcludes.ak` and
/// `transition-trace/proof.ak`'s `ledger_trie` already apply, expressed on the
/// root rather than on the trie so that `does_not_have` (which compares roots)
/// and `insert_root` (which needs a trie) translate through one place and
/// cannot drift apart: `mpf.from_root(null_hash)` is `mpf.empty` by
/// construction.
fn library_root(root: ByteArray) -> ByteArray {
  if root == env.empty_merkle_tree_root {
    null_hash
  } else {
    root
  }
}

/// Total, fail-closed non-membership verification. This deliberately exposes
/// the MPF `excluding` check as a Boolean instead of using `insert`, whose
/// invalid-proof branch aborts script evaluation.
///
/// The claimed root is translated through the Midgard empty-trie sentinel
/// (`library_root` above) before the reconstructed root is compared against it.
/// Until #625 it was not, so for `root == env.empty_merkle_tree_root` this
/// answered `False` for the empty proof — the only witness an empty trie can
/// produce — and non-membership over an empty Midgard trie was unprovable
/// through every caller of this function: `chunked_inclusion_v1`'s
/// published-chunk carriage, `validators/mpf-chunked-verify.ak`,
/// `mpf_chunked_proof_v1.proof_reaches_challenged_terminal`, `insert_root`
/// below, and `validation_machine_v1.resolve_non_membership_step` against an
/// empty `prior_ledger_root`. The gap was liveness-only — nothing false was
/// ever accepted — and it is closed here, so the published-chunk route now
/// answers the same question the redeemer-carried route already answered.
///
/// Provenance: owner-approved 2026-08-17 and pinned to the single blueprint
/// regeneration event, ruled into that event as R5 item 6 by the owner ruling
/// of 2026-08-23 on #625 (decision 0005, wave #617). The `insert_root` fold-in
/// below travels with it — both, or neither.
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
    do_excluding(path, 0, proof) == library_root(root)
  } else {
    False
  }
}

/// Returns a new root only after a total non-membership check has made the
/// library's partial `insert` precondition true.
///
/// The sentinel translation is folded in here as well (#625). `does_not_have`
/// is the gate, so the moment it accepts `env.empty_merkle_tree_root` this
/// function is handed a root the library does not recognise as its empty trie
/// and `mpf.insert`'s own `excluding(key, proof) == self.root` precondition
/// aborts script evaluation on exactly the inputs the gate has just admitted.
/// The two translations move together or neither does.
///
/// The returned root is the library's, untranslated: an insert never yields an
/// empty trie, so a successor root is never the sentinel.
pub fn insert_root(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  if does_not_have(root, key, proof) {
    Some(
      mpf.insert(mpf.from_root(library_root(root)), key, value, proof)
        |> mpf.root,
    )
  } else {
    None
  }
}

/// Returns a new root only after a total membership check has made the
/// library's partial `update` precondition true.
/// `root` must be a non-empty trie root: membership of `key` cannot hold in
/// an empty trie, so the `env.empty_merkle_tree_root` sentinel translation of
/// `library_root` is deliberately not applied here (#625).
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
/// `root` must be a non-empty trie root: membership of `key` cannot hold in
/// an empty trie, so the `env.empty_merkle_tree_root` sentinel translation of
/// `library_root` is deliberately not applied here (#625).
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

// ## Total paired-fold and two-fold mutations

/// Authenticates exact non-membership and derives the insertion successor in
/// one traversal. This is the preferred atomic singleton-state transition at
/// the 64-step protocol ceiling.
pub fn insert_root_paired_fold(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  let path = blake2b_256(key)
  if bytearray.length(root) != digest_byte_count {
    None
  } else {
    when
      do_insert_pair_strict(
        path,
        blake2b_256(value),
        0,
        proof,
        maximum_proof_step_count,
      )
    is {
      Some((predecessor, successor)) ->
        if predecessor == library_root(root) {
          Some(successor)
        } else {
          None
        }
      None -> None
    }
  }
}

/// Authenticates membership at `old_value` and derives the exact update to
/// `new_value` in one traversal.
pub fn update_root_paired_fold(
  root: ByteArray,
  key: ByteArray,
  old_value: ByteArray,
  new_value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  let path = blake2b_256(key)
  if bytearray.length(root) != digest_byte_count {
    None
  } else {
    when
      do_update_pair_strict(
        path,
        blake2b_256(old_value),
        blake2b_256(new_value),
        0,
        proof,
        maximum_proof_step_count,
      )
    is {
      Some((predecessor, successor)) ->
        if predecessor == root {
          Some(successor)
        } else {
          None
        }
      None -> None
    }
  }
}

/// Authenticates membership and derives the exact deletion successor in one
/// traversal.
pub fn delete_root_paired_fold(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  let path = blake2b_256(key)
  if bytearray.length(root) != digest_byte_count {
    None
  } else {
    when
      do_delete_pair_strict(
        path,
        blake2b_256(value),
        0,
        proof,
        maximum_proof_step_count,
      )
    is {
      Some((predecessor, successor)) ->
        if predecessor == root {
          Some(successor)
        } else {
          None
        }
      None -> None
    }
  }
}

//
// The compatibility helpers above first run a total predicate and then invoke
// the forestry library's partial mutation. That is deliberately safe, but the
// library repeats the predecessor fold internally before computing the
// successor: three complete proof walks. A registry transition at the
// protocol-wide 64-step ceiling cannot afford that repetition once published
// proof chunks are decoded from reference inputs.
//
// These helpers preserve the exact fail-closed preconditions while deriving
// the predecessor and successor directly with this module's already strict,
// total folds. They perform exactly two walks and never enter a partial
// forestry mutation.

pub fn insert_root_two_fold(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  let path = blake2b_256(key)
  if bytearray.length(root) != digest_byte_count {
    None
  } else {
    when do_excluding_strict(path, 0, proof, maximum_proof_step_count) is {
      Some(predecessor) ->
        if predecessor == library_root(root) {
          Some(do_including(path, blake2b_256(value), 0, proof))
        } else {
          None
        }
      None -> None
    }
  }
}

pub fn update_root_two_fold(
  root: ByteArray,
  key: ByteArray,
  old_value: ByteArray,
  new_value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  let path = blake2b_256(key)
  if bytearray.length(root) != digest_byte_count {
    None
  } else {
    when
      do_including_strict(
        path,
        blake2b_256(old_value),
        0,
        proof,
        maximum_proof_step_count,
      )
    is {
      Some(predecessor) ->
        if predecessor == root {
          Some(do_including(path, blake2b_256(new_value), 0, proof))
        } else {
          None
        }
      None -> None
    }
  }
}

pub fn delete_root_two_fold(
  root: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
) -> Option<ByteArray> {
  let path = blake2b_256(key)
  if bytearray.length(root) != digest_byte_count {
    None
  } else {
    when
      do_including_strict(
        path,
        blake2b_256(value),
        0,
        proof,
        maximum_proof_step_count,
      )
    is {
      Some(predecessor) ->
        if predecessor == root {
          Some(do_excluding(path, 0, proof))
        } else {
          None
        }
      None -> None
    }
  }
}

pub fn proof_has_at_most_steps(proof: mpf.Proof, maximum: Int) -> Bool {
  and {
    maximum >= 0,
    list.length(proof) <= maximum,
  }
}
