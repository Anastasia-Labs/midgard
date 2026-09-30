//// The terminal step of an MPF proof names the one neighbour that a mutation
//// reads two ways: beside the proven path in the including root, and as the
//// node the path collapses into in the excluding root. These tests pin a
//// terminal Leaf or Fork neighbour to its honest shape in every total
//// mutation and in the resumable fold, in both directions: a tampered terminal
//// Leaf key or skip, or a leaf re-read as a terminal Fork, is refused; the
//// honest proofs still give the honest roots. A deletion's terminal Branch
//// must keep two other children: a Branch standing in for a node with a single
//// other child is refused, and a lone neighbour group is opened to show it
//// holds two.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, nibbles, suffix}
use aiken/merkle_patricia_forestry/merkling.{
  null_hash, null_hash_2, null_hash_4, null_hash_8, sparse_merkle_16,
}
use aiken/primitive/bytearray
use midgard/mpf_branch_opening_fixture as node
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
    proof.delete_root_paired_fold(a_and_b(), a, va, steps, #"") == Some(
      only_b(),
    ),
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
    proof.delete_root_paired_fold(a_and_b(), a, va, steps, #"") == None,
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
    proof.delete_root_paired_fold(a_and_b(), a, va, steps, #"") == None,
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
    proof.delete_root_paired_fold(a_b_and_c(), a, va, steps, #"") == Some(
      b_and_c(),
    ),
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
    proof.delete_root_paired_fold(a_and_d(), a, va, honest, #"") == Some(
      only_d(),
    ),
    proof.insert_root_paired_fold(only_d(), a, va, honest) == Some(a_and_d()),
    proof.has(a_and_d(), a, va, first_nibble_moved),
    proof.has(a_and_d(), a, va, last_nibble_moved),
    proof.delete_root_paired_fold(a_and_d(), a, va, first_nibble_moved, #"") == None,
    proof.delete_root_paired_fold(a_and_d(), a, va, last_nibble_moved, #"") == None,
    proof.insert_root_paired_fold(only_d(), a, va, inflated) == None,
    proof.insert_root_two_fold(only_d(), a, va, inflated) == None,
    fold_steps(a, va, first_nibble_moved, None) == None,
    fold_steps(a, va, last_nibble_moved, None) == None,
    fold_steps(a, va, inflated, None) == None,
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
    proof.delete_root_paired_fold(including, a, va, [step], #"") == Some(
      excluding,
    ),
    proof.insert_root_paired_fold(excluding, a, va, [step]) == Some(including),
  }
}

test mpf_terminal_fork_prefix_past_the_path_is_refused() {
  let (including, excluding, step) = deep_fork(32)
  and {
    proof.has(including, a, va, [step]),
    proof.delete_root_paired_fold(including, a, va, [step], #"") == None,
    proof.insert_root_paired_fold(excluding, a, va, [step]) == None,
    proof.insert_root_two_fold(excluding, a, va, [step]) == None,
    proof.insert_root(excluding, a, va, [step]) == None,
  }
}

// ## A terminal Fork does not re-read as a terminal Leaf
//
// At an odd next cursor a leaf's suffix is `00 ‖ nibble ‖ key bytes`. A fork
// neighbour whose prefix has that length and opens with nibble 0 re-reads as
// the leaf whose key packs the prefix and whose value is the fork's root: the
// including root is the same, the collapse is not.

// The fork beside `a` at nibble 2: a 32-nibble prefix opening with 0, so its
// own branch sits at nibble 35.
const deep_neighbour_slot = 7

fn deep_neighbour_prefix() -> ByteArray {
  bytearray.push(builtin.replicate_byte(31, 3), 0)
}

fn deep_neighbour_root() -> ByteArray {
  blake2b_256(#"0d")
}

fn deep_neighbour_pre() -> ByteArray {
  combine(
    nibbles(pa(), 0, 2),
    sparse_merkle_16(
      nibble(pa(), 2),
      leaf(pa(), 3, va),
      deep_neighbour_slot,
      combine(deep_neighbour_prefix(), deep_neighbour_root()),
    ),
  )
}

fn deep_neighbour_post() -> ByteArray {
  combine(
    bytearray.concat(
      nibbles(pa(), 0, 2),
      bytearray.push(deep_neighbour_prefix(), deep_neighbour_slot),
    ),
    deep_neighbour_root(),
  )
}

fn deep_neighbour_fork() -> mpf.ProofStep {
  mpf.Fork {
    skip: 2,
    neighbor: mpf.Neighbor {
      nibble: deep_neighbour_slot,
      prefix: deep_neighbour_prefix(),
      root: deep_neighbour_root(),
    },
  }
}

fn deep_neighbour_leaf_reread() -> mpf.ProofStep {
  let prefix = deep_neighbour_prefix()
  let key = bytearray.drop(prefix, 2)
    |> bytearray.push(
        deep_neighbour_slot * 16 + builtin.index_bytearray(prefix, 1),
      )
    |> bytearray.push(builtin.index_bytearray(pa(), 0))
  mpf.Leaf { skip: 2, key, value: deep_neighbour_root() }
}

// The premise: both steps prove `a` a member of the same root.
test mpf_terminal_fork_re_read_as_a_leaf_proves_membership() {
  and {
    nibble(pa(), 2) != deep_neighbour_slot,
    proof.has(deep_neighbour_pre(), a, va, [deep_neighbour_fork()]),
    proof.has(deep_neighbour_pre(), a, va, [deep_neighbour_leaf_reread()]),
  }
}

test mpf_terminal_fork_re_read_as_a_leaf_is_refused_on_delete() {
  expect Some(folded) = fold_steps(a, va, [deep_neighbour_fork()], Some(#""))
  and {
    proof.delete_root_paired_fold(
      deep_neighbour_pre(),
      a,
      va,
      [deep_neighbour_fork()],
      #"",
    ) == Some(deep_neighbour_post()),
    folded.including_root == deep_neighbour_pre(),
    folded.excluding_root == deep_neighbour_post(),
    proof.delete_root_paired_fold(
      deep_neighbour_pre(),
      a,
      va,
      [deep_neighbour_leaf_reread()],
      #"",
    ) == None,
    fold_steps(a, va, [deep_neighbour_leaf_reread()], Some(#"")) == None,
  }
}

// A leaf whose key tail is all nibbles is refused exactly where a fork could
// stand in for it: at an odd next cursor from 3, where the suffix is at most
// 32 bytes, to 59, where the fork's own branch still lies inside the path. A
// key tail with any byte of 16 or more is never refused.
test mpf_terminal_leaf_check_refuses_exactly_the_fork_readings() {
  let nibble_key = builtin.replicate_byte(32, 0)
  let other_key = bytearray.concat(builtin.replicate_byte(31, 0), #"10")
  list.all(
    list.range(1, 64),
    fn(next_cursor) {
      and {
        proof.terminal_leaf_neighbor_is_canonical(
          nibble_key,
          nibble_key,
          next_cursor - 1,
          next_cursor,
        ) == !and {
          next_cursor % 2 == 1,
          next_cursor >= 3,
          next_cursor <= 59,
        },
        proof.terminal_leaf_neighbor_is_canonical(
          other_key,
          other_key,
          next_cursor - 1,
          next_cursor,
        ),
      }
    },
  )
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
  deletion_opening: Option<ByteArray>,
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
  fold_back(
    key,
    descriptor,
    initial,
    list.reverse(frames),
    hashes,
    deletion_opening,
  )
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
        _ -> fail @"fold_back: at most two frames"
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

test mpf_fold_terminal_leaf_with_a_skip_gives_the_honest_roots() {
  expect Some(single) = fold_steps(a, va, [honest_leaf_step()], None)
  expect Some(odd) = fold_steps(
    a,
    va,
    [c_step(), odd_cursor_leaf_step(pb(), vb)],
    None,
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
  fold_steps(a, va, [tampered_leaf_step()], None) == None
}

test mpf_fold_refuses_an_inflated_terminal_leaf_skip() {
  fold_steps(a, va, [inflated_leaf_step()], None) == None
}

test mpf_fold_refuses_a_terminal_fork_masquerade() {
  and {
    fold_steps(a, va, [masquerading_fork_step()], None) == None,
    fold_steps(a, va, [c_step(), odd_cursor_fork_masquerade(pb(), vb)], None) == None,
  }
}

test mpf_fold_bounds_the_terminal_fork_prefix() {
  let (including, excluding, inside) = deep_fork(31)
  let (_, _, outside) = deep_fork(32)
  expect Some(accepted) = fold_steps(a, va, [inside], None)
  and {
    accepted.including_root == including,
    accepted.excluding_root == excluding,
    fold_steps(a, va, [outside], None) == None,
  }
}

// ## A deletion's terminal Branch keeps two other children
//
// Each trie below is a single branch at the root whose children are leaves,
// one per slot, keyed by `node.slot_key`. Deleting the leaf in slot `me`
// through a terminal Branch leaves the other children in place; the successor
// is canonical only when at least two of them remain. When they all sit in one
// neighbour group, the group is opened down to the level where it splits.

fn slot_path(slot: Int) -> ByteArray {
  blake2b_256(node.slot_key(slot))
}

fn slot_children(slots: List<Int>) -> List<ByteArray> {
  node.children(
    list.map(
      slots,
      fn(slot) { Pair(slot, leaf(slot_path(slot), 1, node.slot_key(slot))) },
    ),
  )
}

// The honest root of the trie holding one leaf in each of `slots`.
fn slots_root(slots: List<Int>) -> ByteArray {
  when slots is {
    [slot] -> leaf(slot_path(slot), 0, node.slot_key(slot))
    _ -> combine(#"", node.merkle(slot_children(slots)))
  }
}

fn branch_step(slots: List<Int>, me: Int) -> mpf.ProofStep {
  mpf.Branch { skip: 0, neighbors: node.neighbors(slot_children(slots), me) }
}

fn honest_opening(slots: List<Int>, me: Int) -> ByteArray {
  node.opening(slot_children(slots), me)
}

fn delete_slot(
  slots: List<Int>,
  me: Int,
  opening: ByteArray,
) -> Option<ByteArray> {
  proof.delete_root_paired_fold(
    slots_root([me, ..slots]),
    node.slot_key(me),
    node.slot_key(me),
    [branch_step([me, ..slots], me)],
    opening,
  )
}

// The honest delete of `me` from a trie whose other children are `others`
// gives the honest root of `others`.
fn honest_delete_holds(others: List<Int>, me: Int) -> Bool {
  delete_slot(others, me, honest_opening([me, ..others], me)) == Some(
    slots_root(others),
  )
}

fn fold_delete_slot(
  others: List<Int>,
  me: Int,
  deletion_opening: Option<ByteArray>,
) -> Option<fold.ProofFoldControlV1> {
  fold_steps(
    node.slot_key(me),
    node.slot_key(me),
    [branch_step([me, ..others], me)],
    deletion_opening,
  )
}

test mpf_terminal_branch_slot_keys_sit_in_their_slots() {
  list.all(list.range(0, 15), fn(slot) { nibble(slot_path(slot), 0) == slot })
}

// The premise: a Branch naming the lone other child of a two-child node is a
// valid membership proof, and the root it would leave behind is a branch with
// one child, which no honest trie holds.
test mpf_terminal_branch_masquerade_proves_membership_of_the_deleted_key() {
  list.all(
    [4, 6, 1, 12],
    fn(other) {
      let slots = [5, other]
      and {
        proof.has(
          slots_root(slots),
          node.slot_key(5),
          node.slot_key(5),
          [branch_step(slots, 5)],
        ),
        combine(#"", node.merkle(slot_children([other]))) != slots_root([other]),
      }
    },
  )
}

// With `me` = 5 the lone neighbour sits in n1 (slot 4), n2 (slot 6), n4
// (slot 1) or n8 (slot 12).
test mpf_terminal_branch_masquerade_is_refused_on_delete() {
  list.all(
    [4, 6, 1, 12],
    fn(other) {
      and {
        delete_slot([other], 5, #"") == None,
        delete_slot([other], 5, honest_opening([5, other], 5)) == None,
      }
    },
  )
}

test mpf_terminal_branch_with_only_n1_is_refused_with_any_opening() {
  let children = slot_children([4, 5])
  let forged = bytearray.concat(
    leaf(slot_path(4), 1, node.slot_key(4)),
    leaf(slot_path(4), 1, node.slot_key(4)),
  )
  and {
    delete_slot([4], 5, #"") == None,
    delete_slot([4], 5, forged) == None,
    !proof.terminal_branch_keeps_two_children(
      node.neighbors(children, 5),
      forged,
    ),
  }
}

test mpf_terminal_branch_with_no_other_child_is_refused() {
  let neighbors = bytearray.concat(
    null_hash_8,
    bytearray.concat(null_hash_4, bytearray.concat(null_hash_2, null_hash)),
  )
  and {
    node.neighbors(slot_children([5]), 5) == neighbors,
    !proof.terminal_branch_keeps_two_children(neighbors, #""),
    proof.delete_root_paired_fold(
      combine(#"", node.merkle(slot_children([5]))),
      node.slot_key(5),
      node.slot_key(5),
      [mpf.Branch { skip: 0, neighbors }],
      #"",
    ) == None,
  }
}

// Every depth a lone group can be opened at, with the non-empty part on the
// left and on the right, for `me` in the lower half (5) and the upper (10).
test mpf_terminal_branch_lone_group_is_opened_at_every_depth() {
  and {
    // n2 of 5 is 6..7: split at depth 0.
    honest_delete_holds([6, 7], 5),
    // n4 of 5 is 0..3: depth 0, then depth 1 on either side.
    honest_delete_holds([1, 3], 5),
    honest_delete_holds([0, 1], 5),
    honest_delete_holds([2, 3], 5),
    // n8 of 5 is 8..15: depth 0, depth 1 and depth 2 on either side.
    honest_delete_holds([8, 12], 5),
    honest_delete_holds([8, 10], 5),
    honest_delete_holds([12, 14], 5),
    honest_delete_holds([8, 9], 5),
    honest_delete_holds([14, 15], 5),
    honest_delete_holds([10, 11], 5),
    honest_delete_holds([12, 13], 5),
    // n8 of 10 is 0..7, n4 of 10 is 12..15, n2 of 10 is 8..9.
    honest_delete_holds([0, 7], 10),
    honest_delete_holds([6, 7], 10),
    honest_delete_holds([13, 14], 10),
    honest_delete_holds([8, 9], 10),
    // More than two children in the lone group.
    honest_delete_holds([8, 9, 15], 5),
    honest_delete_holds([0, 1, 2, 3], 5),
  }
}

test mpf_terminal_branch_opening_depths_are_as_derived() {
  and {
    bytearray.length(honest_opening([5, 6, 7], 5)) == 64,
    bytearray.take(honest_opening([5, 0, 1], 5), 1) == #"00",
    bytearray.take(honest_opening([5, 2, 3], 5), 1) == #"01",
    bytearray.take(honest_opening([5, 8, 9], 5), 2) == #"0000",
    bytearray.take(honest_opening([5, 14, 15], 5), 2) == #"0101",
    bytearray.take(honest_opening([5, 10, 11], 5), 2) == #"0001",
    bytearray.take(honest_opening([5, 12, 13], 5), 2) == #"0100",
    honest_opening([5, 4, 12], 5) == #"",
  }
}

// Two or more non-empty groups need no opening: whatever is sent is ignored.
test mpf_terminal_branch_with_two_groups_ignores_the_opening() {
  and {
    honest_delete_holds([4, 12], 5),
    honest_delete_holds([0, 6, 9], 5),
    delete_slot([4, 12], 5, #"deadbeef") == Some(slots_root([4, 12])),
    delete_slot([1, 6], 5, bytearray.concat(null_hash, null_hash)) == Some(
      slots_root([1, 6]),
    ),
  }
}

// The lone n8 group of 5 holds slots 8 and 9. Each opening below either stops
// at a single child or does not describe the group.
test mpf_terminal_branch_refuses_a_tampered_opening() {
  let others = [8, 9]
  let children = slot_children([5, ..others])
  let h8 = leaf(slot_path(8), 1, node.slot_key(8))
  let h9 = leaf(slot_path(9), 1, node.slot_key(9))
  let pair = bytearray.concat(h8, h9)
  let honest = bytearray.concat(#"0000", pair)
  let refused = fn(opening) { delete_slot(others, 5, opening) == None }
  and {
    honest_opening([5, ..others], 5) == honest,
    delete_slot(others, 5, honest) == Some(slots_root(others)),
    // Stops at depth 0 or 1, where one half is empty.
    refused(
      bytearray.concat(
        node.merkle(list.take(list.drop(children, 8), 4)),
        null_hash_4,
      ),
    ),
    refused(
      bytearray.concat(#"00", bytearray.concat(combine(h8, h9), null_hash_2)),
    ),
    // Halves that do not hash back to the group.
    refused(bytearray.concat(#"0000", bytearray.concat(h9, h8))),
    refused(bytearray.concat(#"0000", bytearray.concat(h8, h8))),
    // A side byte pointing the wrong way.
    refused(bytearray.concat(#"0100", pair)),
    refused(bytearray.concat(#"0001", pair)),
    // A depth that leaves no level to split: n8 opens at depth 2 at most.
    refused(bytearray.concat(#"000000", pair)),
    // Not a whole opening.
    refused(#""),
    refused(bytearray.drop(honest, 1)),
    refused(pair),
    refused(bytearray.concat(honest, #"00")),
  }
}

// A null half is refused even where it would hash back: a lone group holding
// one child, opened to that child and an empty slot, with the empty slot on
// either side.
test mpf_terminal_branch_refuses_an_opening_with_a_null_half() {
  let h8 = leaf(slot_path(8), 1, node.slot_key(8))
  let h9 = leaf(slot_path(9), 1, node.slot_key(9))
  let right_null = bytearray.concat(#"0000", bytearray.concat(h8, null_hash))
  let left_null = bytearray.concat(#"0000", bytearray.concat(null_hash, h9))
  and {
    delete_slot([8], 5, right_null) == None,
    !proof.terminal_branch_keeps_two_children(
      node.neighbors(slot_children([5, 8]), 5),
      right_null,
    ),
    delete_slot([9], 5, left_null) == None,
    !proof.terminal_branch_keeps_two_children(
      node.neighbors(slot_children([5, 9]), 5),
      left_null,
    ),
  }
}

// An opening cannot split below a single slot. A lone child whose own hash is
// the combine of two 32-byte values (an inner node with a 32-nibble prefix,
// say) hashes back to its group when read as a split one level further down,
// but it is still one child. Shown for a lone n2, n4 and n8 group of 5.
test mpf_terminal_branch_refuses_an_opening_below_a_single_slot() {
  let left = blake2b_256(#"01")
  let right = blake2b_256(#"02")
  let lone = combine(left, right)
  let halves = bytearray.concat(left, right)
  let refused = fn(slot, sides) {
    !proof.terminal_branch_keeps_two_children(
      node.neighbors(node.children([Pair(5, left), Pair(slot, lone)]), 5),
      bytearray.concat(sides, halves),
    )
  }
  and {
    refused(6, #"00"),
    refused(0, #"0000"),
    refused(8, #"000000"),
  }
}

// The resumable fold binds the rule on a deletion only: an insertion folds
// the same frames without it, since its excluding side is the authenticated
// predecessor.
test mpf_fold_terminal_branch_binds_deletion_only() {
  expect Some(honest) = fold_delete_slot(
    [8, 9],
    5,
    Some(honest_opening([5, 8, 9], 5)),
  )
  expect Some(two_groups) = fold_delete_slot([4, 12], 5, Some(#""))
  expect Some(inserted) = fold_delete_slot([12], 5, None)
  and {
    honest.including_root == slots_root([5, 8, 9]),
    honest.excluding_root == slots_root([8, 9]),
    two_groups.excluding_root == slots_root([4, 12]),
    fold_delete_slot([8, 9], 5, Some(#"")) == None,
    fold_delete_slot([12], 5, Some(#"")) == None,
    fold_delete_slot([4], 5, Some(#"")) == None,
    inserted.including_root == slots_root([5, 12]),
  }
}

// A vector the off-chain twin pins byte for byte: slot 5 deleted from a root
// branch whose other children are slots 10 and 11, the lone n8 group opened at
// depth 2 (left, then right).
test mpf_terminal_branch_opening_cross_language_vector() {
  honest_opening([5, 10, 11], 5) == #"000123f26c647a6f86b97488cf39e51aff07382b82473f0bc7078927eecaa0f1bd8152e855108067f344c569011ef098276c4fed20f7d705dfcb063d4052a3a20080"
}

// A deletion's non-terminal Branch is not held to the rule: the other
// children of the root below sit in one n8 group (slots 8 and 9), and the
// proof opens no group for it. Slot 5 holds a branch of `node.slot_key(5)`
// and #"000a", so deleting the first collapses that branch to a leaf. The
// roots and the neighbours are pinned to the off-chain reference trie.
fn two_frame_root_children(five: ByteArray) -> List<ByteArray> {
  node.children(
    [
      Pair(5, five),
      Pair(8, leaf(slot_path(8), 1, node.slot_key(8))),
      Pair(9, leaf(slot_path(9), 1, node.slot_key(9))),
    ],
  )
}

test mpf_fold_exempts_a_non_terminal_branch_from_the_two_children_rule() {
  let k = node.slot_key(5)
  let k2 = #"000a"
  let sub_branch = combine(
    #"",
    node.merkle(
      node.children(
        [
          Pair(nibble(blake2b_256(k), 1), leaf(blake2b_256(k), 2, k)),
          Pair(nibble(blake2b_256(k2), 1), leaf(blake2b_256(k2), 2, k2)),
        ],
      ),
    ),
  )
  let before = combine(#"", node.merkle(two_frame_root_children(sub_branch)))
  let after = combine(
    #"",
    node.merkle(two_frame_root_children(leaf(blake2b_256(k2), 1, k2))),
  )
  let neighbors = node.neighbors(two_frame_root_children(sub_branch), 5)
  let steps = [
    mpf.Branch { skip: 0, neighbors },
    mpf.Leaf { skip: 0, key: blake2b_256(k2), value: blake2b_256(k2) },
  ]
  expect Some(done) = fold_steps(k, k, steps, Some(#""))
  and {
    nibble(blake2b_256(k2), 0) == 5,
    !proof.terminal_branch_keeps_two_children(neighbors, #""),
    proof.delete_root_paired_fold(before, k, k, steps, #"") == Some(after),
    fold.fold_is_complete_v1(done),
    done.including_root == before,
    done.excluding_root == after,
    before == #"8973842c15e9d348695f901360031115e25e2cc5a16b07bc895c68b6ab06ef0c",
    after == #"20b8ca8282f99ed4d9e2614e624bed5a8686df0f314b178ed25a873c5e37d05b",
    neighbors == #"3d02058fedc23b15b37613b8e6a7b636fcce05e681da425e4c1bc46945c5e2b885c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000",
  }
}
