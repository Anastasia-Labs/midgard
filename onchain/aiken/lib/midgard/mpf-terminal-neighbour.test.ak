//// The terminal step of an MPF proof names the one neighbour that a mutation
//// reads two ways: beside the proven path in the including root, and as the
//// node the path collapses into in the excluding root. These tests pin a
//// terminal Leaf or Fork neighbour to its honest shape in every total
//// mutation and in the resumable fold, in both directions: a tampered terminal
//// Leaf key or skip, or a leaf re-read as a terminal Fork, is refused; the
//// honest proofs still give the honest roots. A terminal Branch standing in
//// for a node with a single other child is not covered.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{sparse_merkle_16}
use aiken/primitive/bytearray
use midgard/mpf_proof_fold_v1 as fold
use midgard/mpf_proof_v1 as proof
use midgard/validation_merkle_v1 as merkle

// path(a) = 6 4 2 2 ..., path(b) = 6 a 9 5 ...: they share nibble 0, split at
// nibble 1, and differ again at nibble 2. path(c) = f b 3 d ... splits from
// both at nibble 0.
const a = #"04"

const b = #"0106"

const c = #"05"

const va = #"0a"

const va_next = #"1a"

const vb = #"0b"

const vc = #"0c"

fn pa() {
  blake2b_256(a)
}

fn pb() {
  blake2b_256(b)
}

fn pc() {
  blake2b_256(c)
}

fn leaf(path: ByteArray, cursor: Int, value: ByteArray) {
  combine(suffix(path, cursor), blake2b_256(value))
}

// ## A two-leaf trie whose terminal Leaf neighbour sits behind a skip of one

fn a_and_b_with(value: ByteArray) {
  combine(
    nibbles(pa(), 0, 1),
    sparse_merkle_16(4, leaf(pa(), 2, value), 10, leaf(pb(), 2, vb)),
  )
}

fn a_and_b() {
  a_and_b_with(va)
}

fn only_b() {
  leaf(pb(), 0, vb)
}

fn honest_leaf_step() {
  mpf.Leaf { skip: 1, key: pb(), value: blake2b_256(vb) }
}

// Nibble 0 of the neighbour's key moved from 6 to 7. The including root reads
// only nibbles 1.. of the key, so membership still holds.
fn tampered_leaf_step() {
  mpf.Leaf {
    skip: 1,
    key: bytearray.concat(#"7a", bytearray.drop(pb(), 1)),
    value: blake2b_256(vb),
  }
}

// The split claimed at nibble 2 instead of 1: nibble 2 differs too, so the
// branching-nibble check alone accepts it.
fn inflated_leaf_step() {
  mpf.Leaf { skip: 2, key: pb(), value: blake2b_256(vb) }
}

// The leaf of b re-read as a branch whose prefix is the leaf's own suffix.
fn masquerading_fork_step() {
  mpf.Fork {
    skip: 1,
    neighbor: mpf.Neighbor {
      nibble: 10,
      prefix: suffix(pb(), 2),
      root: blake2b_256(vb),
    },
  }
}

test mpf_terminal_leaf_with_a_skip_mutates_to_the_honest_roots() {
  let steps = [honest_leaf_step()]
  and {
    nibble(pa(), 0) == nibble(pb(), 0),
    nibble(pa(), 1) != nibble(pb(), 1),
    proof.delete_root_paired_fold(a_and_b(), a, va, steps) == Some(only_b()),
    proof.delete_root_two_fold(a_and_b(), a, va, steps) == Some(only_b()),
    proof.delete_root(a_and_b(), a, va, steps) == Some(only_b()),
    proof.insert_root_paired_fold(only_b(), a, va, steps) == Some(a_and_b()),
    proof.insert_root_two_fold(only_b(), a, va, steps) == Some(a_and_b()),
    proof.insert_root(only_b(), a, va, steps) == Some(a_and_b()),
    proof.update_root_paired_fold(a_and_b(), a, va, va_next, steps) == Some(
      a_and_b_with(va_next),
    ),
    proof.update_root_two_fold(a_and_b(), a, va, va_next, steps) == Some(
      a_and_b_with(va_next),
    ),
    proof.update_root(a_and_b(), a, va, va_next, steps) == Some(
      a_and_b_with(va_next),
    ),
  }
}

test mpf_terminal_leaf_key_tamper_is_refused_on_delete() {
  let steps = [tampered_leaf_step()]
  and {
    proof.has(a_and_b(), a, va, steps),
    proof.delete_root_paired_fold(a_and_b(), a, va, steps) == None,
    proof.delete_root_two_fold(a_and_b(), a, va, steps) == None,
    proof.delete_root(a_and_b(), a, va, steps) == None,
  }
}

test mpf_terminal_leaf_skip_inflation_is_refused_on_insert() {
  let steps = [inflated_leaf_step()]
  and {
    !proof.does_not_have(only_b(), a, steps),
    proof.insert_root_paired_fold(only_b(), a, va, steps) == None,
    proof.insert_root_two_fold(only_b(), a, va, steps) == None,
    proof.insert_root(only_b(), a, va, steps) == None,
  }
}

test mpf_terminal_fork_masquerade_is_refused_on_delete() {
  let steps = [masquerading_fork_step()]
  and {
    proof.has(a_and_b(), a, va, steps),
    proof.delete_root_paired_fold(a_and_b(), a, va, steps) == None,
    proof.delete_root_two_fold(a_and_b(), a, va, steps) == None,
    proof.delete_root(a_and_b(), a, va, steps) == None,
  }
}

// ## A three-leaf trie whose terminal neighbour sits at an odd cursor

fn b_and_c() {
  combine(#"", sparse_merkle_16(6, leaf(pb(), 1, vb), 15, leaf(pc(), 1, vc)))
}

fn a_b_and_c() {
  combine(
    #"",
    sparse_merkle_16(
      6,
      combine(
        #"",
        sparse_merkle_16(4, leaf(pa(), 2, va), 10, leaf(pb(), 2, vb)),
      ),
      15,
      leaf(pc(), 1, vc),
    ),
  )
}

fn a_and_c() {
  combine(#"", sparse_merkle_16(6, leaf(pa(), 1, va), 15, leaf(pc(), 1, vc)))
}

fn c_step() {
  mpf.Leaf { skip: 0, key: pc(), value: blake2b_256(vc) }
}

fn odd_cursor_leaf_step(path: ByteArray, value: ByteArray) {
  mpf.Leaf { skip: 0, key: path, value: blake2b_256(value) }
}

// A leaf at odd cursor 1 re-read as a branch at nibble 0 whose prefix is the
// leaf's suffix without its 0x00 marker: the collapse of this fork equals the
// true leaf hash, so a predecessor check against the true root accepts it.
fn odd_cursor_fork_masquerade(path: ByteArray, value: ByteArray) {
  mpf.Fork {
    skip: 0,
    neighbor: mpf.Neighbor {
      nibble: 0,
      prefix: bytearray.drop(suffix(path, 1), 1),
      root: blake2b_256(value),
    },
  }
}

test mpf_terminal_leaf_at_an_odd_cursor_mutates_to_the_honest_roots() {
  let steps = [c_step(), odd_cursor_leaf_step(pb(), vb)]
  and {
    proof.does_not_have(b_and_c(), a, steps),
    proof.insert_root_paired_fold(b_and_c(), a, va, steps) == Some(a_b_and_c()),
    proof.insert_root_two_fold(b_and_c(), a, va, steps) == Some(a_b_and_c()),
    proof.insert_root(b_and_c(), a, va, steps) == Some(a_b_and_c()),
    proof.delete_root_paired_fold(a_b_and_c(), a, va, steps) == Some(b_and_c()),
    proof.delete_root_two_fold(a_b_and_c(), a, va, steps) == Some(b_and_c()),
    proof.delete_root(a_b_and_c(), a, va, steps) == Some(b_and_c()),
  }
}

test mpf_terminal_fork_masquerade_is_refused_on_insert() {
  let steps = [c_step(), odd_cursor_fork_masquerade(pb(), vb)]
  and {
    !proof.does_not_have(b_and_c(), a, steps),
    proof.insert_root_paired_fold(b_and_c(), a, va, steps) == None,
    proof.insert_root_two_fold(b_and_c(), a, va, steps) == None,
    proof.insert_root(b_and_c(), a, va, steps) == None,
  }
}

test mpf_terminal_fork_masquerade_cannot_prove_a_present_key_absent() {
  and {
    proof.has(a_and_c(), a, va, [c_step()]),
    !proof.does_not_have(
      a_and_c(),
      a,
      [c_step(), odd_cursor_fork_masquerade(pa(), va)],
    ),
  }
}

// ## The terminal fork's own branching nibble must lie inside the path

// ## A longer skip: the check spans whole bytes and a trailing odd nibble

// path(d) = 6 4 2 8 6 ...: it shares nibbles 0..2 with path(a) = 6 4 2 2 0 ...
// and differs at nibbles 3 and 4.
const d = #"000071"

const vd = #"0d"

fn pd() {
  blake2b_256(d)
}

fn a_and_d() {
  combine(
    nibbles(pa(), 0, 3),
    sparse_merkle_16(2, leaf(pa(), 4, va), 8, leaf(pd(), 4, vd)),
  )
}

fn only_d() {
  leaf(pd(), 0, vd)
}

fn d_step(skip: Int, key: ByteArray) {
  mpf.Leaf { skip, key, value: blake2b_256(vd) }
}

// The byte string with nibble `position` moved to the next nibble value.
fn with_nibble_changed(bytes: ByteArray, position: Int) -> ByteArray {
  let offset = position / 2
  let old = builtin.index_bytearray(bytes, offset)
  let replacement = if position % 2 == 0 {
    ( old / 16 + 1 ) % 16 * 16 + old % 16
  } else {
    old / 16 * 16 + ( old + 1 ) % 16
  }
  bytearray.concat(
    bytearray.take(bytes, offset),
    bytearray.push(bytearray.drop(bytes, offset + 1), replacement),
  )
}

test mpf_terminal_leaf_tamper_is_refused_across_a_longer_skip() {
  let honest = [d_step(3, pd())]
  let first_nibble_moved = [d_step(3, with_nibble_changed(pd(), 0))]
  let last_nibble_moved = [d_step(3, with_nibble_changed(pd(), 2))]
  let inflated = [d_step(4, pd())]
  and {
    nibbles(pa(), 0, 3) == nibbles(pd(), 0, 3),
    nibble(pa(), 3) != nibble(pd(), 3),
    nibble(pa(), 4) != nibble(pd(), 4),
    proof.delete_root_paired_fold(a_and_d(), a, va, honest) == Some(only_d()),
    proof.insert_root_paired_fold(only_d(), a, va, honest) == Some(a_and_d()),
    proof.has(a_and_d(), a, va, first_nibble_moved),
    proof.has(a_and_d(), a, va, last_nibble_moved),
    proof.delete_root_paired_fold(a_and_d(), a, va, first_nibble_moved) == None,
    proof.delete_root_paired_fold(a_and_d(), a, va, last_nibble_moved) == None,
    proof.delete_root(a_and_d(), a, va, first_nibble_moved) == None,
    proof.delete_root(a_and_d(), a, va, last_nibble_moved) == None,
    proof.insert_root_paired_fold(only_d(), a, va, inflated) == None,
    proof.insert_root_two_fold(only_d(), a, va, inflated) == None,
    fold_steps(a, va, first_nibble_moved) == None,
    fold_steps(a, va, last_nibble_moved) == None,
    fold_steps(a, va, inflated) == None,
  }
}

// ## The two predicates at their edges

// Every span [cursor, next_cursor - 1) with either end odd or even, against a
// key that differs from the path at exactly one nibble: the leaf check holds
// exactly when that nibble lies outside the span.
test mpf_terminal_leaf_check_compares_exactly_the_skipped_nibbles() {
  let path = pa()
  list.all(
    list.range(0, 9),
    fn(position) {
      let key = with_nibble_changed(path, position)
      list.all(
        list.range(0, 4),
        fn(cursor) {
          list.all(
            list.range(cursor + 1, cursor + 7),
            fn(next_cursor) {
              proof.terminal_leaf_neighbor_is_canonical(
                path,
                key,
                cursor,
                next_cursor,
              ) == !(cursor <= position && position < next_cursor - 1)
            },
          )
        },
      )
    },
  )
}

// A terminal fork prefix is accepted exactly when every byte is a nibble and
// the neighbour's own branching nibble, at `next_cursor + length`, is at most
// nibble 63.
test mpf_terminal_fork_check_bounds_and_types_the_prefix() {
  let root = blake2b_256(#"0e")
  let accepts = fn(next_cursor, prefix) {
    proof.terminal_fork_neighbor_is_canonical(
      next_cursor,
      mpf.Neighbor { nibble: 0, prefix, root },
    )
  }
  and {
    accepts(1, #""),
    accepts(63, #""),
    !accepts(64, #""),
    accepts(32, builtin.replicate_byte(31, 15)),
    !accepts(32, builtin.replicate_byte(32, 15)),
    accepts(1, builtin.replicate_byte(32, 0)),
    !accepts(1, #"10"),
    !accepts(1, #"ff"),
    !accepts(1, #"000010"),
    !accepts(
      1,
      #"0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0fff",
    ),
  }
}

// A fork at cursor 0 with skip 31 whose neighbour prefix is `prefix_length`
// nibbles long, and the roots on either side of it.
fn deep_fork(prefix_length: Int) -> (ByteArray, ByteArray, mpf.ProofStep) {
  let neighbor_nibble = ( nibble(pa(), 31) + 1 ) % 16
  let neighbor = mpf.Neighbor {
    nibble: neighbor_nibble,
    prefix: builtin.replicate_byte(prefix_length, 0),
    root: blake2b_256(#"0d"),
  }
  let including = combine(
    nibbles(pa(), 0, 31),
    sparse_merkle_16(
      nibble(pa(), 31),
      leaf(pa(), 32, va),
      neighbor_nibble,
      combine(neighbor.prefix, neighbor.root),
    ),
  )
  let excluding = combine(
    bytearray.concat(
      nibbles(pa(), 0, 31),
      bytearray.push(neighbor.prefix, neighbor_nibble),
    ),
    neighbor.root,
  )
  (including, excluding, mpf.Fork { skip: 31, neighbor })
}

test mpf_terminal_fork_prefix_may_end_at_the_last_nibble() {
  let (including, excluding, step) = deep_fork(31)
  and {
    proof.delete_root_paired_fold(including, a, va, [step]) == Some(excluding),
    proof.insert_root_paired_fold(excluding, a, va, [step]) == Some(including),
  }
}

test mpf_terminal_fork_prefix_past_the_path_is_refused() {
  let (including, excluding, step) = deep_fork(32)
  and {
    proof.has(including, a, va, [step]),
    proof.delete_root_paired_fold(including, a, va, [step]) == None,
    proof.delete_root_two_fold(including, a, va, [step]) == None,
    proof.delete_root(including, a, va, [step]) == None,
    proof.insert_root_paired_fold(excluding, a, va, [step]) == None,
    proof.insert_root_two_fold(excluding, a, va, [step]) == None,
    proof.insert_root(excluding, a, va, [step]) == None,
  }
}

// ## The resumable fold binds the same terminal frame

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

// Folds a proof's frames deepest first, returning the completed control or
// None at the first refused frame.
fn fold_steps(
  key: ByteArray,
  value: ByteArray,
  steps: List<mpf.ProofStep>,
) -> Option<fold.ProofFoldControlV1> {
  let frames = build_frames(steps, 0, 0)
  let hashes = list.map(frames, fold.proof_frame_leaf_hash_v1)
  expect Some(last) = list.last(frames)
  let descriptor = fold.ProofDescriptorV1 {
    version: 1,
    frame_count: list.length(frames),
    terminal_cursor: last.next_cursor,
    peaks: merkle.build_frontier(hashes).peaks,
  }
  expect Some(initial) = fold.initial_fold_control_v1(key, value, descriptor)
  fold_back(key, descriptor, initial, list.reverse(frames), hashes)
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

// The two-frame proofs here have exactly one sibling per frame: the other
// frame's hash.
fn fold_back(
  key: ByteArray,
  descriptor: fold.ProofDescriptorV1,
  control: fold.ProofFoldControlV1,
  frames: List<fold.ProofFrameV1>,
  hashes: List<ByteArray>,
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
        _ -> fail @"fold_back: at most two frames"
      }
      when
        fold.fold_proof_frame_v1(key, descriptor, control, this, siblings)
      is {
        Some(next) -> fold_back(key, descriptor, next, rest, hashes)
        None -> None
      }
    }
  }
}

test mpf_fold_terminal_leaf_with_a_skip_gives_the_honest_roots() {
  expect Some(single) = fold_steps(a, va, [honest_leaf_step()])
  expect Some(odd) = fold_steps(
    a,
    va,
    [c_step(), odd_cursor_leaf_step(pb(), vb)],
  )
  and {
    fold.fold_is_complete_v1(single),
    single.including_root == a_and_b(),
    single.excluding_root == only_b(),
    fold.fold_is_complete_v1(odd),
    odd.including_root == a_b_and_c(),
    odd.excluding_root == b_and_c(),
  }
}

test mpf_fold_refuses_a_tampered_terminal_leaf_key() {
  fold_steps(a, va, [tampered_leaf_step()]) == None
}

test mpf_fold_refuses_an_inflated_terminal_leaf_skip() {
  fold_steps(a, va, [inflated_leaf_step()]) == None
}

test mpf_fold_refuses_a_terminal_fork_masquerade() {
  and {
    fold_steps(a, va, [masquerading_fork_step()]) == None,
    fold_steps(a, va, [c_step(), odd_cursor_fork_masquerade(pb(), vb)]) == None,
  }
}

test mpf_fold_bounds_the_terminal_fork_prefix() {
  let (including, excluding, inside) = deep_fork(31)
  let (_, _, outside) = deep_fork(32)
  expect Some(accepted) = fold_steps(a, va, [inside])
  and {
    accepted.including_root == including,
    accepted.excluding_root == excluding,
    fold_steps(a, va, [outside]) == None,
  }
}
