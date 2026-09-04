use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}

/// The canonical V1 compact MMR frontier carries only occupied binary peaks. It still
/// covers the validation machine's full `2^32 - 1` count envelope, but small
/// transaction-local lists reveal only a handful of 32-byte hashes on L1.
pub const maximum_leaf_count: Int = 4_294_967_295

const branch_domain =
  #"4d69646761726456616c69646174696f6e4d65726b6c654272616e63685631"

const frontier_domain =
  #"4d69646761726456616c69646174696f6e4d65726b6c6546726f6e746965725631"

pub type FrontierPeak {
  height: Int,
  hash: ByteArray,
}

pub type BuiltFrontier {
  count: Int,
  peaks: List<FrontierPeak>,
}

pub fn hash_branch(left: ByteArray, right: ByteArray) -> ByteArray {
  expect bytearray.length(left) == 32
  expect bytearray.length(right) == 32
  branch_domain
    |> bytearray.concat(left)
    |> bytearray.concat(right)
    |> blake2b_256
}

pub fn empty_frontier() -> List<FrontierPeak> {
  []
}

fn encode_peaks(peaks: List<FrontierPeak>) -> ByteArray {
  when peaks is {
    [] -> #""
    [peak, ..rest] ->
      #"82"
        |> bytearray.concat(cbor.serialise(peak.height))
        |> bytearray.concat(encode_definite_bytes(peak.hash))
        |> bytearray.concat(encode_peaks(rest))
  }
}

pub fn encode_frontier(peaks: List<FrontierPeak>) -> ByteArray {
  encode_definite_array_header(list.length(peaks))
    |> bytearray.concat(encode_peaks(peaks))
}

fn expected_peaks_are_well_formed(
  remaining_count: Int,
  height: Int,
  peaks: List<FrontierPeak>,
) -> Bool {
  if remaining_count == 0 {
    peaks == []
  } else if remaining_count % 2 == 1 {
    when peaks is {
      [] -> False
      [peak, ..rest] -> and {
          peak.height == height,
          bytearray.length(peak.hash) == 32,
          expected_peaks_are_well_formed(remaining_count / 2, height + 1, rest),
        }
    }
  } else {
    expected_peaks_are_well_formed(remaining_count / 2, height + 1, peaks)
  }
}

pub fn frontier_is_well_formed(count: Int, peaks: List<FrontierPeak>) -> Bool {
  and {
    count >= 0,
    count <= maximum_leaf_count,
    expected_peaks_are_well_formed(count, 0, peaks),
  }
}

pub fn frontier_commitment(count: Int, peaks: List<FrontierPeak>) -> ByteArray {
  expect frontier_is_well_formed(count, peaks)
  frontier_domain
    |> bytearray.concat(cbor.serialise(count))
    |> bytearray.concat(encode_frontier(peaks))
    |> blake2b_256
}

fn append_carry(
  old_count: Int,
  height: Int,
  carry: ByteArray,
  peaks: List<FrontierPeak>,
) -> List<FrontierPeak> {
  if old_count % 2 == 0 {
    [FrontierPeak { height, hash: carry }, ..peaks]
  } else {
    expect [left, ..rest] = peaks
    expect left.height == height
    append_carry(old_count / 2, height + 1, hash_branch(left.hash, carry), rest)
  }
}

pub fn append_leaf(
  count: Int,
  peaks: List<FrontierPeak>,
  leaf_hash: ByteArray,
) -> List<FrontierPeak> {
  expect count < maximum_leaf_count
  expect bytearray.length(leaf_hash) == 32
  expect frontier_is_well_formed(count, peaks)
  let next = append_carry(count, 0, leaf_hash, peaks)
  expect frontier_is_well_formed(count + 1, next)
  next
}

fn append_all(
  leaves: List<ByteArray>,
  count: Int,
  peaks: List<FrontierPeak>,
) -> BuiltFrontier {
  when leaves is {
    [] -> BuiltFrontier { count, peaks }
    [leaf, ..rest] ->
      append_all(rest, count + 1, append_leaf(count, peaks, leaf))
  }
}

pub fn build_frontier(leaves: List<ByteArray>) -> BuiltFrontier {
  append_all(leaves, 0, empty_frontier())
}

fn power_of_two(exponent: Int) -> Int {
  if exponent <= 0 {
    1
  } else {
    2 * power_of_two(exponent - 1)
  }
}

fn highest_bit(value: Int, height: Int) -> Int {
  if value < 2 {
    height
  } else {
    highest_bit(value / 2, height + 1)
  }
}

type PeakLocation {
  height: Int,
  local_index: Int,
}

fn locate_peak(
  count: Int,
  leaf_index: Int,
  height: Int,
  offset: Int,
) -> PeakLocation {
  expect height >= 0
  let size = power_of_two(height)
  if count / size % 2 == 1 {
    if leaf_index < offset + size {
      PeakLocation { height, local_index: leaf_index - offset }
    } else {
      locate_peak(count, leaf_index, height - 1, offset + size)
    }
  } else {
    locate_peak(count, leaf_index, height - 1, offset)
  }
}

fn peak_hash_at(peaks: List<FrontierPeak>, expected_height: Int) -> ByteArray {
  expect [peak, ..rest] = peaks
  if peak.height == expected_height {
    peak.hash
  } else {
    peak_hash_at(rest, expected_height)
  }
}

fn fold_membership_path(
  local_index: Int,
  current: ByteArray,
  siblings: List<ByteArray>,
) -> ByteArray {
  when siblings is {
    [] -> current
    [sibling, ..rest] -> {
      expect bytearray.length(sibling) == 32
      let parent = if local_index % 2 == 0 {
        hash_branch(current, sibling)
      } else {
        hash_branch(sibling, current)
      }
      fold_membership_path(local_index / 2, parent, rest)
    }
  }
}

pub fn verify_membership(
  count: Int,
  peaks: List<FrontierPeak>,
  leaf_index: Int,
  leaf_hash: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  if and {
    frontier_is_well_formed(count, peaks),
    leaf_index >= 0,
    leaf_index < count,
    bytearray.length(leaf_hash) == 32,
  } {
    let location = locate_peak(count, leaf_index, highest_bit(count, 0), 0)
    and {
      list.length(siblings) == location.height,
      fold_membership_path(location.local_index, leaf_hash, siblings) == peak_hash_at(
        peaks,
        location.height,
      ),
    }
  } else {
    False
  }
}
