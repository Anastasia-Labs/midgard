//// The canonical Data grammar of `canonical_plutus_data_v1`, read as a
//// resumable stream: a verdict-only reader for spans too long for one
//// transaction.
////
//// `canonical_plutus_data_v1.is_canonical_plutus_data_v1` walks one span
//// recursively in a single call. This module takes the same grammar one node
//// at a time over a window of the span, so a caller can stop after any node
//// and resume in a later transaction. The open containers are an explicit
//// stack; between transactions only its hash chain is stored, and the next
//// transaction reveals as many frames as it needs.
////
//// The two readers must agree on every span: the shared canonical Data
//// corpus (`cek-data-canonical-corpus-v1-golden.test.ak`) checks the one-shot
//// predicate, this reader and the streamed traverse on every vector.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_v1.{
  byte_at, data_bytes_chunk, generic_constr_tag, large_constr_tag_base, major_of,
  max_large_constr_alternative, max_small_constr_alternative,
  min_bignum_magnitude_bytes, min_generic_constr_alternative,
  negative_bignum_tag, positive_bignum_tag, scan_head, small_constr_tag_base,
}

/// The most bytes one node reads past its first byte's offset: a bignum
/// `c2 58 40` plus a 64-byte magnitude. A window that does not reach the end of
/// the span stops before any node that starts within this many bytes of the
/// window's end, so every refusal is a fact about the span, never about where
/// the window was cut.
pub const max_node_read_bytes: Int = 67

const break_byte: Int = 0xff

const indefinite_additional_info: Int = 31

/// One open container.
pub type StreamFrameV1 {
  /// An indefinite list (`9f`), also a constructor's fields. `started` once
  /// an item has begun, since `9f ff` is not canonical.
  ListFrame { started: Bool }
  /// A definite map with `remaining` keys and values still to read.
  MapFrame { remaining: Int }
  /// An indefinite byte string (`5f`) after `seen` full 64-byte chunks.
  /// `leading` holds for a bignum magnitude whose first chunk must not start
  /// with a zero byte.
  ChunksFrame { seen: Int, leading: Bool }
}

/// Where a bounded run stopped.
pub type StreamOutcomeV1 {
  /// The span is one canonical Data item, exactly.
  StreamCanonical
  /// The span is not canonical Data.
  StreamNonCanonical
  /// Undecided after `nodes` nodes: resume at `offset` (window-relative) with
  /// the open containers `frames` above the unrevealed `tail_root`.
  StreamPaused { offset: Int, frames: List<StreamFrameV1>, nodes: Int }
}

/// The empty stack's hash chain.
pub const empty_stack_root: ByteArray = #""

/// The hash chain of `frames` (top first) above `tail_root`.
pub fn stack_root_v1(
  frames: List<StreamFrameV1>,
  tail_root: ByteArray,
) -> ByteArray {
  when frames is {
    [] -> tail_root
    [frame, ..rest] ->
      blake2b_256(
        bytearray.concat(cbor.serialise(frame), stack_root_v1(rest, tail_root)),
      )
  }
}

/// Reads up to `budget` nodes of the span from `offset` in `bytes`.
///
/// `bytes` is a window of the span's carrier, already cut at the span's end
/// when `full`; otherwise the span continues past the window. `root_start` is
/// the window-relative offset of the span's first byte (negative once the
/// window has moved past it). `frames` are the revealed open containers, top
/// first, above `tail_root`.
pub fn run_v1(
  bytes: ByteArray,
  full: Bool,
  root_start: Int,
  offset: Int,
  frames: List<StreamFrameV1>,
  tail_root: ByteArray,
  budget: Int,
) -> StreamOutcomeV1 {
  run(
    bytes,
    full,
    bytearray.length(bytes),
    root_start,
    offset,
    frames,
    tail_root,
    budget,
    0,
  )
}

fn run(
  bytes: ByteArray,
  full: Bool,
  limit: Int,
  root_start: Int,
  offset: Int,
  frames: List<StreamFrameV1>,
  tail_root: ByteArray,
  budget: Int,
  nodes: Int,
) -> StreamOutcomeV1 {
  let closed = and {
    frames == [],
    tail_root == empty_stack_root,
  }
  if and {
    closed,
    offset > root_start,
  } {
    // The root item is complete: the span is canonical exactly when nothing
    // follows it.
    if and {
      full,
      offset == limit,
    } {
      StreamCanonical
    } else {
      StreamNonCanonical
    }
  } else if or {
    nodes >= budget,
    and {
      !full,
      offset + max_node_read_bytes > limit,
    },
  } {
    StreamPaused { offset, frames, nodes }
  } else {
    let next = when frames is {
      [] ->
        if closed {
          Some(item(bytes, limit, offset, []))
        } else {
          // The open containers are below the revealed frames.
          None
        }
      [MapFrame { remaining }, ..rest] ->
        if remaining == 0 {
          Some(Some(Pair(offset, rest)))
        } else {
          Some(
            item(
              bytes,
              limit,
              offset,
              [MapFrame { remaining: remaining - 1 }, ..rest],
            ),
          )
        }
      [ListFrame { started }, ..rest] ->
        when byte_at(bytes, offset) is {
          None -> Some(None)
          Some(first) ->
            if first == break_byte {
              if started {
                Some(Some(Pair(offset + 1, rest)))
              } else {
                Some(None)
              }
            } else {
              Some(
                item(
                  bytes,
                  limit,
                  offset,
                  [ListFrame { started: True }, ..rest],
                ),
              )
            }
        }
      [ChunksFrame { seen, leading }, ..rest] ->
        Some(chunk(bytes, limit, offset, seen, leading, rest))
    }
    when next is {
      None -> StreamPaused { offset, frames, nodes }
      Some(None) -> StreamNonCanonical
      Some(Some(Pair(next_offset, next_frames))) ->
        run(
          bytes,
          full,
          limit,
          root_start,
          next_offset,
          next_frames,
          tail_root,
          budget,
          nodes + 1,
        )
    }
  }
}

/// One item starting at `offset`: a leaf is read whole, a container pushes
/// its frame.
fn item(
  bytes: ByteArray,
  limit: Int,
  offset: Int,
  frames: List<StreamFrameV1>,
) -> Option<Pair<Int, List<StreamFrameV1>>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) -> {
      let major = major_of(first)
      if or {
        major == 0,
        major == 1,
      } {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, _value)) -> Some(Pair(next, frames))
        }
      } else if major == 2 {
        if first % 32 == indefinite_additional_info {
          Some(
            Pair(
              offset + 1,
              [ChunksFrame { seen: 0, leading: False }, ..frames],
            ),
          )
        } else {
          when scan_head(bytes, offset) is {
            None -> None
            Some(Pair(next, length)) ->
              if and {
                length <= data_bytes_chunk,
                next + length <= limit,
              } {
                Some(Pair(next + length, frames))
              } else {
                None
              }
          }
        }
      } else if major == 4 {
        list_open(bytes, offset, frames)
      } else if major == 5 {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, entries)) ->
            if entries == 0 {
              Some(Pair(next, frames))
            } else {
              Some(Pair(next, [MapFrame { remaining: 2 * entries }, ..frames]))
            }
        }
      } else if and {
        major == 6,
        first % 32 != indefinite_additional_info,
      } {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, tag)) ->
            if or {
              and {
                tag >= small_constr_tag_base,
                tag <= small_constr_tag_base + max_small_constr_alternative,
              },
              and {
                tag >= large_constr_tag_base,
                tag <= large_constr_tag_base + max_large_constr_alternative - 7,
              },
            } {
              list_open(bytes, next, frames)
            } else if tag == generic_constr_tag {
              generic_constr(bytes, next, frames)
            } else if or {
              tag == positive_bignum_tag,
              tag == negative_bignum_tag,
            } {
              magnitude(bytes, limit, next, frames)
            } else {
              None
            }
        }
      } else {
        None
      }
    }
  }
}

/// A list or constructor-field list: exactly `80` when empty, else `9f`.
fn list_open(
  bytes: ByteArray,
  offset: Int,
  frames: List<StreamFrameV1>,
) -> Option<Pair<Int, List<StreamFrameV1>>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first == 0x80 {
        Some(Pair(offset + 1, frames))
      } else if first == 0x9f {
        Some(Pair(offset + 1, [ListFrame { started: False }, ..frames]))
      } else {
        None
      }
  }
}

/// `d8 66` then `82 ‖ uint(alternative ≥ 128) ‖ fields`.
fn generic_constr(
  bytes: ByteArray,
  offset: Int,
  frames: List<StreamFrameV1>,
) -> Option<Pair<Int, List<StreamFrameV1>>> {
  if byte_at(bytes, offset) != Some(0x82) {
    None
  } else {
    when byte_at(bytes, offset + 1) is {
      None -> None
      Some(alternative_head) ->
        if major_of(alternative_head) != 0 {
          None
        } else {
          when scan_head(bytes, offset + 1) is {
            None -> None
            Some(Pair(next, alternative)) ->
              if alternative >= min_generic_constr_alternative {
                list_open(bytes, next, frames)
              } else {
                None
              }
          }
        }
    }
  }
}

/// A bignum magnitude: a definite byte string of 9 to 64 bytes, or a chunked
/// one, with no leading zero byte.
fn magnitude(
  bytes: ByteArray,
  limit: Int,
  offset: Int,
  frames: List<StreamFrameV1>,
) -> Option<Pair<Int, List<StreamFrameV1>>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if major_of(first) != 2 {
        None
      } else if first % 32 == indefinite_additional_info {
        Some(
          Pair(offset + 1, [ChunksFrame { seen: 0, leading: True }, ..frames]),
        )
      } else {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, length)) ->
            if and {
              length >= min_bignum_magnitude_bytes,
              length <= data_bytes_chunk,
              next + length <= limit,
              byte_at(bytes, next) != Some(0),
            } {
              Some(Pair(next + length, frames))
            } else {
              None
            }
        }
      }
  }
}

/// One chunk of an indefinite byte string: a full 64-byte chunk, a shorter
/// final chunk with its break, or the break after at least two full chunks.
fn chunk(
  bytes: ByteArray,
  limit: Int,
  offset: Int,
  seen: Int,
  leading: Bool,
  rest: List<StreamFrameV1>,
) -> Option<Pair<Int, List<StreamFrameV1>>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first == break_byte {
        if seen >= 2 {
          Some(Pair(offset + 1, rest))
        } else {
          None
        }
      } else if major_of(first) != 2 {
        None
      } else {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, length)) ->
            if length == data_bytes_chunk {
              if and {
                next + length <= limit,
                !leading || byte_at(bytes, next) != Some(0),
              } {
                Some(
                  Pair(
                    next + length,
                    [ChunksFrame { seen: seen + 1, leading: False }, ..rest],
                  ),
                )
              } else {
                None
              }
            } else if and {
              length > 0,
              length < data_bytes_chunk,
              seen >= 1,
              byte_at(bytes, next + length) == Some(break_byte),
            } {
              Some(Pair(next + length + 1, rest))
            } else {
              None
            }
        }
      }
  }
}
