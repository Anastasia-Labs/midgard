//// Leaf and branch node preimages are disjoint. A leaf preimage opens with a
//// marker byte of 16 or more (0xff at an even cursor, another value at an odd
//// one) and a branch preimage opens with its prefix nibbles or, for an empty
//// prefix, with its merkle root, so no node hashes both ways. These tests pin
//// the rule, the honest proofs around it in both directions, and the checks
//// that still read a neighbour's bytes: the neighbour nibble bound, the
//// neighbour root length, the first-byte check on a deletion's terminal Fork,
//// and the two-children rule on a deletion's terminal Branch.
////
//// Every construction reads the odd-cursor marker back from `suffix`, so the
//// module does not restate the marker's value.

use aiken/builtin.{blake2b_256, index_bytearray, slice_bytearray}
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{
  merkle_16, null_hash, null_hash_4, null_hash_8, sparse_merkle_16,
}
use aiken/primitive/bytearray
use midgard/mpf_proof_fold_v1 as fold
use midgard/mpf_proof_v1
use midgard/validation_merkle_v1 as merkle

// ## Shared

fn marker() -> Int {
  index_bytearray(suffix(blake2b_256(#""), 1), 0)
}

const value = #"76616c7565"

const other =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const neighbors =
  #"1111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222222222222222222222222222222222222233333333333333333333333333333333333333333333333333333333333333334444444444444444444444444444444444444444444444444444444444444444"

fn nb() {
  slice_bytearray(0, 128, neighbors)
}

fn merkle_16_nb(slot: Int, node: ByteArray) {
  let n = nb()
  merkle_16(
    slot,
    node,
    slice_bytearray(0, 32, n),
    slice_bytearray(32, 32, n),
    slice_bytearray(64, 32, n),
    slice_bytearray(96, 32, n),
  )
}

fn under_root_branch(prefix: ByteArray, slot: Int, node: ByteArray) {
  combine(prefix, merkle_16_nb(slot, node))
}

fn frame(index: Int, cursor: Int, step: mpf.ProofStep) {
  let skip = when step is {
    mpf.Branch { skip, .. } -> skip
    mpf.Fork { skip, .. } -> skip
    mpf.Leaf { skip, .. } -> skip
  }
  fold.ProofFrameV1 {
    version: 1,
    frame_index: index,
    cursor,
    next_cursor: cursor + 1 + skip,
    step,
  }
}

fn build_frames(
  steps: List<mpf.ProofStep>,
  index: Int,
  cursor: Int,
) -> List<fold.ProofFrameV1> {
  when steps is {
    [] -> []
    [step, ..rest] -> {
      let this = frame(index, cursor, step)
      [this, ..build_frames(rest, index + 1, this.next_cursor)]
    }
  }
}

fn fold_back(
  key: ByteArray,
  descriptor: fold.ProofDescriptorV1,
  control: fold.ProofFoldControlV1,
  frames: List<fold.ProofFrameV1>,
  hashes: List<ByteArray>,
  deletion_opening: Option<ByteArray>,
) -> Option<fold.ProofFoldControlV1> {
  when frames is {
    [] -> Some(control)
    [this, ..rest] -> {
      let siblings = when hashes is {
        [_] -> []
        [first, second] ->
          if this.frame_index == 0 {
            [second]
          } else {
            [first]
          }
        _ -> fail @"at most two frames"
      }
      when
        fold.fold_proof_frame_v1(
          key,
          descriptor,
          control,
          this,
          siblings,
          deletion_opening,
        )
      is {
        Some(next) ->
          fold_back(key, descriptor, next, rest, hashes, deletion_opening)
        None -> None
      }
    }
  }
}

// (including_root, excluding_root) of the resumable fold, or None.
fn fold_steps(
  key: ByteArray,
  v: ByteArray,
  steps: List<mpf.ProofStep>,
  deletion_opening: Option<ByteArray>,
) -> Option<(ByteArray, ByteArray)> {
  let frames = build_frames(steps, 0, 0)
  let hashes = list.map(frames, fold.proof_frame_leaf_hash_v1)
  expect Some(last) = list.last(frames)
  let descriptor = fold.ProofDescriptorV1 {
    version: 1,
    frame_count: list.length(frames),
    terminal_cursor: last.next_cursor,
    peaks: merkle.build_frontier(hashes).peaks,
  }
  expect Some(initial) = fold.initial_fold_control_v1(key, v, descriptor)
  when
    fold_back(
      key,
      descriptor,
      initial,
      list.reverse(frames),
      hashes,
      deletion_opening,
    )
  is {
    Some(c) ->
      if fold.fold_is_complete_v1(c) {
        Some((c.including_root, c.excluding_root))
      } else {
        None
      }
    None -> None
  }
}

// ## The rule: a leaf preimage never opens with a nibble

fn leaf_heads_are_not_nibbles(path: ByteArray, cursor: Int) -> Bool {
  if cursor > 64 {
    True
  } else {
    let s = suffix(path, cursor)
    and {
      index_bytearray(s, 0) >= 16,
      bytearray.length(s) >= 1,
      leaf_heads_are_not_nibbles(path, cursor + 1),
    }
  }
}

test leaf_preimage_never_starts_with_a_nibble() {
  and {
    leaf_heads_are_not_nibbles(blake2b_256(#""), 0),
    leaf_heads_are_not_nibbles(
      #"000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f",
      0,
    ),
    leaf_heads_are_not_nibbles(
      #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      0,
    ),
  }
}

test even_and_odd_leaf_markers_differ() {
  and {
    index_bytearray(suffix(blake2b_256(#""), 0), 0) == 255,
    marker() != 255,
  }
}

// ## A branch whose prefix opens like an odd-cursor leaf suffix
//
// A present key x sits under a branch at cursor 1 with a 33-nibble prefix P
// and one other child. `k` is the key whose odd-cursor suffix at cursor 1
// carries P's nibbles after the marker.
const x = #"70726573656e742d3232"

const y = #"696e736572742d33"

fn a_setup() {
  let path = blake2b_256(x)
  let p = nibbles(path, 1, 34)
  let q = ( nibble(path, 34) + 1 ) % 16
  let m = sparse_merkle_16(
    nibble(path, 34),
    combine(suffix(path, 35), blake2b_256(value)),
    q,
    combine(#"", other),
  )
  let root = combine(#"", merkle_16_nb(nibble(path, 0), combine(p, m)))
  let k = bytearray.push(
    slice_bytearray(2, 31, p),
    nibble(path, 0) * 16 + index_bytearray(p, 1),
  )
  (p, q, m, root, k)
}

fn a_honest(p: ByteArray, m: ByteArray) {
  [
    mpf.Branch { skip: 0, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: index_bytearray(p, 0),
        prefix: slice_bytearray(1, 32, p),
        root: m,
      },
    },
  ]
}

fn a_leaf_reading(m: ByteArray, k: ByteArray) {
  [
    mpf.Branch { skip: 0, neighbors: nb() },
    mpf.Leaf { skip: 0, key: k, value: m },
  ]
}

test present_key_is_member() {
  let (_p, q, _m, root, _k) = a_setup()
  mpf_proof_v1.has(
    root,
    x,
    value,
    [
      mpf.Branch { skip: 0, neighbors: nb() },
      mpf.Fork {
        skip: 33,
        neighbor: mpf.Neighbor { nibble: q, prefix: #"", root: other },
      },
    ],
  )
}

test honest_insertion_is_accepted() {
  let (p, _q, m, root, _k) = a_setup()
  expect Some(_) = mpf_proof_v1.insert_root_paired_fold(
    root,
    y,
    value,
    a_honest(p, m),
  )
  expect Some(_) = fold_steps(y, value, a_honest(p, m), None)
  True
}

test leaf_suffix_never_equals_a_branch_prefix() {
  let (p, _q, _m, _root, k) = a_setup()
  suffix(k, 1) != p
}

test present_key_cannot_be_proven_absent() {
  let (_p, _q, m, root, k) = a_setup()
  !mpf_proof_v1.does_not_have(root, x, a_leaf_reading(m, k))
}

// An insertion beside the branch has exactly one successor: a leaf reading of
// the branch does not open the authenticated root.
test present_key_cannot_be_proven_absent_on_insert() {
  let (p, _q, m, root, k) = a_setup()
  expect Some(h) = mpf_proof_v1.insert_root_paired_fold(
    root,
    y,
    value,
    a_honest(p, m),
  )
  when
    mpf_proof_v1.insert_root_paired_fold(root, y, value, a_leaf_reading(m, k))
  is {
    None -> True
    Some(f) -> f == h
  }
}

test present_key_cannot_be_proven_absent_in_the_fold() {
  let (p, _q, m, root, k) = a_setup()
  expect Some((h, hx)) = fold_steps(y, value, a_honest(p, m), None)
  and {
    hx == root,
    when fold_steps(y, value, a_leaf_reading(m, k), None) is {
      None -> True
      Some((f, fx)) -> fx != root || f == h
    },
  }
}

// ## A leaf neighbour one nibble deeper than its branch
const leaf_preimage = #"736b69702d6c6561662d313933"

fn b_setup() {
  let k = blake2b_256(leaf_preimage)
  let py = blake2b_256(y)
  let v = #"5555555555555555555555555555555555555555555555555555555555555555"
  let root = under_root_branch(
    nibbles(py, 0, 58),
    nibble(py, 58),
    combine(suffix(k, 59), v),
  )
  let honest = [
    mpf.Branch { skip: 58, neighbors: nb() },
    mpf.Leaf { skip: 0, key: k, value: v },
  ]
  let s = suffix(k, 59)
  let fork = [
    mpf.Branch { skip: 58, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: index_bytearray(s, 0),
        prefix: slice_bytearray(1, bytearray.length(s) - 1, s),
        root: v,
      },
    },
  ]
  (root, honest, fork)
}

test honest_leaf_insertion_is_accepted() {
  let (root, honest, _fork) = b_setup()
  expect Some(_) = mpf_proof_v1.insert_root_paired_fold(root, y, value, honest)
  expect Some(_) = fold_steps(y, value, honest, None)
  mpf_proof_v1.does_not_have(root, y, honest)
}

test leaf_neighbour_cannot_be_read_as_a_fork() {
  let (root, honest, fork) = b_setup()
  expect Some(h) = mpf_proof_v1.insert_root_paired_fold(root, y, value, honest)
  when mpf_proof_v1.insert_root_paired_fold(root, y, value, fork) is {
    None -> True
    Some(f) -> f == h
  }
}

test leaf_neighbour_cannot_be_read_as_a_fork_in_the_fold() {
  let (root, honest, fork) = b_setup()
  expect Some((h, _)) = fold_steps(y, value, honest, None)
  when fold_steps(y, value, fork, None) is {
    None -> True
    Some((f, fx)) -> fx != root || f == h
  }
}

// ## A present leaf at an odd cursor
const z = #"656e632d7a65726f2d30"

fn x_setup() {
  let pz = blake2b_256(z)
  let leaf = combine(suffix(pz, 1), blake2b_256(value))
  let root = combine(#"", merkle_16_nb(nibble(pz, 0), leaf))
  let s = suffix(pz, 1)
  let absent = [
    mpf.Branch { skip: 0, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: index_bytearray(s, 0),
        prefix: slice_bytearray(1, bytearray.length(s) - 1, s),
        root: blake2b_256(value),
      },
    },
  ]
  (root, absent)
}

test present_odd_cursor_leaf_is_member() {
  let (root, _absent) = x_setup()
  mpf_proof_v1.has(root, z, value, [mpf.Branch { skip: 0, neighbors: nb() }])
}

test present_odd_cursor_leaf_cannot_be_proven_absent() {
  let (root, absent) = x_setup()
  !mpf_proof_v1.does_not_have(root, z, absent)
}

// ## A present leaf at an even cursor
const ze = #"76732d6576656e2d30"

fn xe_setup() {
  let pz = blake2b_256(ze)
  let leaf = combine(suffix(pz, 2), blake2b_256(value))
  let root = combine(nibbles(pz, 0, 1), merkle_16_nb(nibble(pz, 1), leaf))
  let s = suffix(pz, 2)
  let absent = [
    mpf.Branch { skip: 1, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: index_bytearray(s, 0),
        prefix: slice_bytearray(1, bytearray.length(s) - 1, s),
        root: blake2b_256(value),
      },
    },
  ]
  (root, absent)
}

test present_even_cursor_leaf_is_member() {
  let (root, _absent) = xe_setup()
  mpf_proof_v1.has(root, ze, value, [mpf.Branch { skip: 1, neighbors: nb() }])
}

test present_even_cursor_leaf_cannot_be_proven_absent() {
  let (root, absent) = xe_setup()
  !mpf_proof_v1.does_not_have(root, ze, absent)
}

// ## Deletion beside a leaf whose key tail is all nibbles
//
// The leaf's path shares nibbles [0, 58) with the deleted key d and ends in
// bytes below 16.
const d = #"656e632d64656c2d30"

const vk = #"6666666666666666666666666666666666666666666666666666666666666666"

fn c_setup() {
  let pd = blake2b_256(d)
  let k58 = ( nibble(pd, 58) + 1 ) % 16
  let kpath = bytearray.concat(
    slice_bytearray(0, 29, pd),
    bytearray.push(#"0400", k58 * 16 + 3),
  )
  let leaf_d = combine(suffix(pd, 59), blake2b_256(value))
  let leaf_k = combine(suffix(kpath, 59), vk)
  let node = combine(#"", sparse_merkle_16(nibble(pd, 58), leaf_d, k58, leaf_k))
  let root = under_root_branch(nibbles(pd, 0, 57), nibble(pd, 57), node)
  let successor = under_root_branch(
    nibbles(pd, 0, 57),
    nibble(pd, 57),
    combine(suffix(kpath, 58), vk),
  )
  let honest = [
    mpf.Branch { skip: 57, neighbors: nb() },
    mpf.Leaf { skip: 0, key: kpath, value: vk },
  ]
  let s = suffix(kpath, 59)
  let fork = [
    mpf.Branch { skip: 57, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor { nibble: k58, prefix: s, root: vk },
    },
  ]
  let split = [
    mpf.Branch { skip: 57, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor {
        nibble: k58,
        prefix: #"",
        root: bytearray.concat(s, vk),
      },
    },
  ]
  (kpath, root, successor, honest, fork, split)
}

test deletion_setup_is_as_claimed() {
  let (kpath, root, _successor, honest, _fork, _split) = c_setup()
  let pd = blake2b_256(d)
  and {
    nibbles(pd, 0, 58) == nibbles(kpath, 0, 58),
    nibble(pd, 58) != nibble(kpath, 58),
    mpf_proof_v1.has(root, d, value, honest),
  }
}

test honest_leaf_deletion_is_accepted() {
  let (_kpath, root, successor, honest, _fork, _split) = c_setup()
  mpf_proof_v1.delete_root_paired_fold(root, d, value, honest, #"") == Some(
    successor,
  )
}

test honest_leaf_deletion_is_accepted_in_the_fold() {
  let (_kpath, root, successor, honest, _fork, _split) = c_setup()
  fold_steps(d, value, honest, Some(#"")) == Some((root, successor))
}

test deletion_refuses_a_fork_reading_of_the_neighbour() {
  let (_kpath, root, successor, _honest, fork, _split) = c_setup()
  when mpf_proof_v1.delete_root_paired_fold(root, d, value, fork, #"") is {
    None -> True
    Some(f) -> f == successor
  }
}

test deletion_refuses_a_fork_reading_of_the_neighbour_in_the_fold() {
  let (_kpath, root, successor, _honest, fork, _split) = c_setup()
  when fold_steps(d, value, fork, Some(#"")) is {
    None -> True
    Some((i, f)) -> i != root || f == successor
  }
}

test deletion_refuses_a_split_reading_of_the_neighbour() {
  let (_kpath, root, successor, _honest, _fork, split) = c_setup()
  when mpf_proof_v1.delete_root_paired_fold(root, d, value, split, #"") is {
    None -> True
    Some(f) -> f == successor
  }
}

// ## A deletion's terminal Branch keeps two other children
//
// The node at cursor 0 has two children at cursor 2: the deleted key and one
// leaf L in its n2 group.
const lone = #"656e632d6c6f6e652d30"

const vl = #"7777777777777777777777777777777777777777777777777777777777777777"

fn s_setup() {
  let pv = blake2b_256(lone)
  let b = nibble(pv, 1)
  let s = if b / 2 % 2 == 0 {
    b + 2
  } else {
    b - 2
  }
  let lpath = bytearray.push(slice_bytearray(1, 31, pv), nibble(pv, 0) * 16 + s)
  let leaf_d = combine(suffix(pv, 2), blake2b_256(value))
  let leaf_l = combine(suffix(lpath, 2), vl)
  let root = combine(nibbles(pv, 0, 1), sparse_merkle_16(b, leaf_d, s, leaf_l))
  let n2 = if s % 2 == 0 {
    combine(leaf_l, null_hash)
  } else {
    combine(null_hash, leaf_l)
  }
  let group = null_hash_8
    |> bytearray.concat(null_hash_4)
    |> bytearray.concat(n2)
    |> bytearray.concat(null_hash)
  let opening = bytearray.push(bytearray.concat(suffix(lpath, 2), vl), s % 2)
  (pv, b, lpath, leaf_d, root, group, opening)
}

test one_child_branch_setup_is_as_claimed() {
  let (pv, b, lpath, leaf_d, root, group, opening) = s_setup()
  and {
    combine(
      nibbles(pv, 0, 1),
      merkle_16(
        b,
        leaf_d,
        slice_bytearray(0, 32, group),
        slice_bytearray(32, 32, group),
        slice_bytearray(64, 32, group),
        slice_bytearray(96, 32, group),
      ),
    ) == root,
    bytearray.length(suffix(lpath, 2)) + 32 == 64,
    bytearray.length(opening) == 65,
  }
}

test honest_deletion_beside_a_lone_leaf_is_accepted() {
  let (_pv, _b, lpath, _leaf_d, root, _group, _opening) = s_setup()
  mpf_proof_v1.delete_root_paired_fold(
    root,
    lone,
    value,
    [mpf.Leaf { skip: 1, key: lpath, value: vl }],
    #"",
  ) == Some(combine(suffix(lpath, 0), vl))
}

test deletion_refuses_a_one_child_branch_opening() {
  let (_pv, _b, lpath, _leaf_d, root, group, opening) = s_setup()
  when
    mpf_proof_v1.delete_root_paired_fold(
      root,
      lone,
      value,
      [mpf.Branch { skip: 1, neighbors: group }],
      opening,
    )
  is {
    None -> True
    Some(f) -> f == combine(suffix(lpath, 0), vl)
  }
}

// ## A branch neighbour with a long prefix
//
// The neighbour's prefix length is not bounded beyond the key path: an honest
// branch whose prefix is 34 nibbles, so the Fork neighbour carries a 33-byte
// prefix, is proven absent beside, inserted beside and deleted beside.
const yl = #"76732d6c6f6e672d30"

fn lp_setup(n: Int) {
  let py = blake2b_256(yl)
  let first = ( nibble(py, 1) + 1 ) % 16
  let rest = nibbles(blake2b_256(#"01"), 0, n - 1)
  let pn = bytearray.push(rest, first)
  let branch = combine(pn, other)
  let root = combine(#"", merkle_16_nb(nibble(py, 0), branch))
  let proof = [
    mpf.Branch { skip: 0, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor { nibble: first, prefix: rest, root: other },
    },
  ]
  let node = combine(
    #"",
    sparse_merkle_16(
      nibble(py, 1),
      combine(suffix(py, 2), blake2b_256(value)),
      first,
      combine(rest, other),
    ),
  )
  let successor = combine(#"", merkle_16_nb(nibble(py, 0), node))
  (root, proof, successor)
}

test long_prefix_branch_neighbour_is_accepted() {
  let (root, proof, successor) = lp_setup(34)
  expect [_, mpf.Fork { neighbor, .. }] = proof
  and {
    bytearray.length(neighbor.prefix) == 33,
    mpf_proof_v1.does_not_have(root, yl, proof),
    mpf_proof_v1.insert_root_paired_fold(root, yl, value, proof) == Some(
      successor,
    ),
    mpf_proof_v1.insert_root(root, yl, value, proof) == Some(successor),
    fold_steps(yl, value, proof, None) == Some((successor, root)),
    mpf_proof_v1.delete_root_paired_fold(successor, yl, value, proof, #"") == Some(
      root,
    ),
    fold_steps(yl, value, proof, Some(#"")) == Some((successor, root)),
  }
}

// ## A deletion's terminal Fork with an empty prefix
const db = #"76732d62302d30"

fn b0_setup() {
  let pd = blake2b_256(db)
  let q = ( nibble(pd, 1) + 1 ) % 16
  let node = combine(
    #"",
    sparse_merkle_16(
      nibble(pd, 1),
      combine(suffix(pd, 2), blake2b_256(value)),
      q,
      combine(#"", other),
    ),
  )
  let root = combine(#"", merkle_16_nb(nibble(pd, 0), node))
  let successor = combine(
    #"",
    merkle_16_nb(nibble(pd, 0), combine(bytearray.push(#"", q), other)),
  )
  let proof = [
    mpf.Branch { skip: 0, neighbors: nb() },
    mpf.Fork {
      skip: 0,
      neighbor: mpf.Neighbor { nibble: q, prefix: #"", root: other },
    },
  ]
  (root, successor, proof)
}

test deletion_beside_an_empty_prefix_branch_is_accepted() {
  let (root, successor, proof) = b0_setup()
  and {
    mpf_proof_v1.delete_root_paired_fold(root, db, value, proof, #"") == Some(
      successor,
    ),
    fold_steps(db, value, proof, Some(#"")) == Some((root, successor)),
  }
}
