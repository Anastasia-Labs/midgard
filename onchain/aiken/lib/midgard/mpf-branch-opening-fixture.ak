//// Test support for the terminal-Branch deletion rule
//// (`mpf_proof_v1.terminal_branch_keeps_two_children`): a branch node built
//// from its sixteen child hashes, the neighbour groups a Branch step names for
//// one of those children, and the group opening an honest prover sends. The
//// opening is built here from the children, independently of the verifier.

use aiken/collection/list
use aiken/merkle_patricia_forestry/helpers.{combine}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray

/// A key whose path (`blake2b_256(key)`) starts with nibble `slot`.
pub fn slot_key(slot: Int) -> ByteArray {
  when slot is {
    0 -> #"00"
    1 -> #"42"
    2 -> #"19"
    3 -> #"0f"
    4 -> #"0b"
    5 -> #"0a"
    6 -> #"04"
    7 -> #"1f"
    8 -> #"07"
    9 -> #"0e"
    10 -> #"13"
    11 -> #"02"
    12 -> #"0d"
    13 -> #"16"
    14 -> #"01"
    15 -> #"05"
    _ -> fail @"slot_key: slot out of range"
  }
}

/// The sixteen child hashes of a node, `null_hash` where a slot is empty.
pub fn children(entries: List<Pair<Int, ByteArray>>) -> List<ByteArray> {
  list.map(
    list.range(0, 15),
    fn(slot) {
      when list.find(entries, fn(entry) { entry.1st == slot }) is {
        Some(entry) -> entry.2nd
        None -> null_hash
      }
    },
  )
}

fn pairs(hashes: List<ByteArray>) -> List<ByteArray> {
  when hashes is {
    [left, right, ..rest] -> [combine(left, right), ..pairs(rest)]
    _ -> []
  }
}

/// The merkle root of a power-of-two run of child hashes.
pub fn merkle(hashes: List<ByteArray>) -> ByteArray {
  when hashes is {
    [hash] -> hash
    _ -> merkle(pairs(hashes))
  }
}

fn run(hashes: List<ByteArray>, start: Int, count: Int) -> List<ByteArray> {
  list.take(list.drop(hashes, start), count)
}

fn has_child(hashes: List<ByteArray>, start: Int, count: Int) -> Bool {
  list.any(run(hashes, start, count), fn(hash) { hash != null_hash })
}

// The first slot of each neighbour group of `me`, in the order n8, n4, n2, n1.
fn group_starts(me: Int) -> List<Pair<Int, Int>> {
  [
    Pair(
      if me < 8 {
        8
      } else {
        0
      },
      8,
    ),
    Pair(
      me / 8 * 8 + if me % 8 < 4 {
        4
      } else {
        0
      },
      4,
    ),
    Pair(
      me / 4 * 4 + if me % 4 < 2 {
        2
      } else {
        0
      },
      2,
    ),
    Pair(
      if me % 2 == 0 {
        me + 1
      } else {
        me - 1
      },
      1,
    ),
  ]
}

/// The 128-byte `neighbors` of a Branch step for child `me`: n8 ‖ n4 ‖ n2 ‖ n1.
pub fn neighbors(hashes: List<ByteArray>, me: Int) -> ByteArray {
  list.foldr(
    group_starts(me),
    #"",
    fn(group, rest) {
      bytearray.concat(merkle(run(hashes, group.1st, group.2nd)), rest)
    },
  )
}

fn open_group(
  hashes: List<ByteArray>,
  start: Int,
  size: Int,
  sides: ByteArray,
) -> ByteArray {
  let half = size / 2
  let left = has_child(hashes, start, half)
  let right = has_child(hashes, start + half, half)
  if left && right {
    sides
      |> bytearray.concat(merkle(run(hashes, start, half)))
      |> bytearray.concat(merkle(run(hashes, start + half, half)))
  } else if half == 1 {
    #""
  } else if left {
    open_group(hashes, start, half, bytearray.concat(sides, #"00"))
  } else {
    open_group(hashes, start + half, half, bytearray.concat(sides, #"01"))
  }
}

/// The opening an honest prover sends with a terminal Branch for child `me`:
/// empty unless exactly one neighbour group is non-empty and it spans two or
/// more slots, then that group opened down to its first split.
pub fn opening(hashes: List<ByteArray>, me: Int) -> ByteArray {
  let occupied = list.filter(
    group_starts(me),
    fn(group) { has_child(hashes, group.1st, group.2nd) },
  )
  when occupied is {
    [group] ->
      if group.2nd > 1 {
        open_group(hashes, group.1st, group.2nd, #"")
      } else {
        #""
      }
    _ -> #""
  }
}
