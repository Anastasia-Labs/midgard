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
  data_bytes_chunk, generic_constr_tag, large_constr_tag_base,
  max_large_constr_alternative, max_small_constr_alternative,
  min_bignum_magnitude_bytes, min_generic_constr_alternative,
  negative_bignum_tag, positive_bignum_tag, small_constr_tag_base,
}

/// The most bytes one node reads past its first byte's offset: a bignum
/// `c2 58 40` plus a 64-byte magnitude. A window that does not reach the end of
/// the span stops before any node that starts within this many bytes of the
/// window's end, so every refusal is a fact about the span, never about where
/// the window was cut.
pub const max_node_read_bytes: Int = 67

const break_byte: Int = 0xff

const indefinite_bytes_byte: Int = 0x5f

const empty_list_byte: Int = 0x80

const indefinite_list_byte: Int = 0x9f

const generic_constr_pair_byte: Int = 0x82

const last_small_constr_tag: Int =
  small_constr_tag_base + max_small_constr_alternative

const last_large_constr_tag: Int =
  large_constr_tag_base + max_large_constr_alternative - 7

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
  let limit = bytearray.length(bytes)
  run(
    bytes,
    full,
    limit,
    if full {
      limit
    } else {
      limit - max_node_read_bytes
    },
    root_start,
    tail_root == empty_stack_root,
    budget,
    offset,
    frames,
    0,
  )
}

/// Where the next node starts once the current one has been read.
type Next =
  fn(Int, List<StreamFrameV1>) -> StreamOutcomeV1

fn run(
  bytes: ByteArray,
  full: Bool,
  limit: Int,
  last_start: Int,
  root_start: Int,
  tail_closed: Bool,
  budget: Int,
  offset: Int,
  frames: List<StreamFrameV1>,
  nodes: Int,
) -> StreamOutcomeV1 {
  let next: Next = fn(next_offset, next_frames) {
    run(
      bytes,
      full,
      limit,
      last_start,
      root_start,
      tail_closed,
      budget,
      next_offset,
      next_frames,
      nodes + 1,
    )
  }
  let paused = or {
    nodes >= budget,
    offset > last_start,
  }
  when frames is {
    [] ->
      if !tail_closed {
        // The open containers are below the revealed frames.
        StreamPaused { offset, frames, nodes }
      } else if offset > root_start {
        // The root item is complete: the span is canonical exactly when
        // nothing follows it.
        if and {
          full,
          offset == limit,
        } {
          StreamCanonical
        } else {
          StreamNonCanonical
        }
      } else if paused {
        StreamPaused { offset, frames, nodes }
      } else {
        item(bytes, limit, offset, frames, next)
      }
    [frame, ..rest] ->
      if paused {
        StreamPaused { offset, frames, nodes }
      } else {
        when frame is {
          MapFrame { remaining } ->
            if remaining == 0 {
              next(offset, rest)
            } else {
              item(
                bytes,
                limit,
                offset,
                [MapFrame { remaining: remaining - 1 }, ..rest],
                next,
              )
            }
          ListFrame { started } ->
            if offset >= limit {
              StreamNonCanonical
            } else if bytearray.at(bytes, offset) == break_byte {
              if started {
                next(offset + 1, rest)
              } else {
                StreamNonCanonical
              }
            } else if started {
              item(bytes, limit, offset, frames, next)
            } else {
              item(
                bytes,
                limit,
                offset,
                [ListFrame { started: True }, ..rest],
                next,
              )
            }
          ChunksFrame { seen, leading } ->
            chunk(bytes, limit, offset, seen, leading, rest, next)
        }
      }
  }
}

/// A canonical head at `offset`, whose first byte is `first`: the offset past
/// it and its argument, given to `then`.
fn head(
  bytes: ByteArray,
  limit: Int,
  offset: Int,
  first: Int,
  then: fn(Int, Int) -> StreamOutcomeV1,
) -> StreamOutcomeV1 {
  let additional = first % 32
  if additional <= 23 {
    then(offset + 1, additional)
  } else if additional == 24 {
    if offset + 1 >= limit {
      StreamNonCanonical
    } else {
      let value = bytearray.at(bytes, offset + 1)
      if value >= 24 {
        then(offset + 2, value)
      } else {
        StreamNonCanonical
      }
    }
  } else if additional == 25 {
    wide_head(bytes, limit, offset, 2, 0x100, then)
  } else if additional == 26 {
    wide_head(bytes, limit, offset, 4, 0x10000, then)
  } else if additional == 27 {
    wide_head(bytes, limit, offset, 8, 0x100000000, then)
  } else {
    StreamNonCanonical
  }
}

/// A head whose argument is the `width` bytes after its first byte, and
/// canonical only when at least `least`.
fn wide_head(
  bytes: ByteArray,
  limit: Int,
  offset: Int,
  width: Int,
  least: Int,
  then: fn(Int, Int) -> StreamOutcomeV1,
) -> StreamOutcomeV1 {
  let end = offset + width
  if end >= limit {
    StreamNonCanonical
  } else {
    let value = bytearray.to_int_big_endian(
      bytearray.slice(bytes, offset + 1, end),
    )
    if value >= least {
      then(end + 1, value)
    } else {
      StreamNonCanonical
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
  next: Next,
) -> StreamOutcomeV1 {
  if offset >= limit {
    StreamNonCanonical
  } else {
    let first = bytearray.at(bytes, offset)
    let major = first / 32
    if major <= 1 {
      head(bytes, limit, offset, first, fn(past, _value) { next(past, frames) })
    } else if major == 2 {
      if first == indefinite_bytes_byte {
        next(offset + 1, [ChunksFrame { seen: 0, leading: False }, ..frames])
      } else {
        head(
          bytes,
          limit,
          offset,
          first,
          fn(past, length) {
            if and {
              length <= data_bytes_chunk,
              past + length <= limit,
            } {
              next(past + length, frames)
            } else {
              StreamNonCanonical
            }
          },
        )
      }
    } else if major == 4 {
      list_open(bytes, limit, offset, frames, next)
    } else if major == 5 {
      head(
        bytes,
        limit,
        offset,
        first,
        fn(past, entries) {
          if entries == 0 {
            next(past, frames)
          } else {
            next(past, [MapFrame { remaining: 2 * entries }, ..frames])
          }
        },
      )
    } else if major == 6 {
      head(
        bytes,
        limit,
        offset,
        first,
        fn(past, tag) {
          if or {
            and {
              tag >= small_constr_tag_base,
              tag <= last_small_constr_tag,
            },
            and {
              tag >= large_constr_tag_base,
              tag <= last_large_constr_tag,
            },
          } {
            list_open(bytes, limit, past, frames, next)
          } else if tag == generic_constr_tag {
            generic_constr(bytes, limit, past, frames, next)
          } else if or {
            tag == positive_bignum_tag,
            tag == negative_bignum_tag,
          } {
            magnitude(bytes, limit, past, frames, next)
          } else {
            StreamNonCanonical
          }
        },
      )
    } else {
      StreamNonCanonical
    }
  }
}

/// A list or constructor-field list: exactly `80` when empty, else `9f`.
fn list_open(
  bytes: ByteArray,
  limit: Int,
  offset: Int,
  frames: List<StreamFrameV1>,
  next: Next,
) -> StreamOutcomeV1 {
  if offset >= limit {
    StreamNonCanonical
  } else {
    let first = bytearray.at(bytes, offset)
    if first == empty_list_byte {
      next(offset + 1, frames)
    } else if first == indefinite_list_byte {
      next(offset + 1, [ListFrame { started: False }, ..frames])
    } else {
      StreamNonCanonical
    }
  }
}

/// `d8 66` then `82 ‖ uint(alternative ≥ 128) ‖ fields`.
fn generic_constr(
  bytes: ByteArray,
  limit: Int,
  offset: Int,
  frames: List<StreamFrameV1>,
  next: Next,
) -> StreamOutcomeV1 {
  if offset + 1 >= limit {
    StreamNonCanonical
  } else if bytearray.at(bytes, offset) != generic_constr_pair_byte {
    StreamNonCanonical
  } else {
    let alternative_head = bytearray.at(bytes, offset + 1)
    if alternative_head / 32 != 0 {
      StreamNonCanonical
    } else {
      head(
        bytes,
        limit,
        offset + 1,
        alternative_head,
        fn(past, alternative) {
          if alternative >= min_generic_constr_alternative {
            list_open(bytes, limit, past, frames, next)
          } else {
            StreamNonCanonical
          }
        },
      )
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
  next: Next,
) -> StreamOutcomeV1 {
  if offset >= limit {
    StreamNonCanonical
  } else {
    let first = bytearray.at(bytes, offset)
    if first / 32 != 2 {
      StreamNonCanonical
    } else if first == indefinite_bytes_byte {
      next(offset + 1, [ChunksFrame { seen: 0, leading: True }, ..frames])
    } else {
      head(
        bytes,
        limit,
        offset,
        first,
        fn(past, length) {
          if and {
            length >= min_bignum_magnitude_bytes,
            length <= data_bytes_chunk,
            past + length <= limit,
            bytearray.at(bytes, past) != 0,
          } {
            next(past + length, frames)
          } else {
            StreamNonCanonical
          }
        },
      )
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
  next: Next,
) -> StreamOutcomeV1 {
  if offset >= limit {
    StreamNonCanonical
  } else {
    let first = bytearray.at(bytes, offset)
    if first == break_byte {
      if seen >= 2 {
        next(offset + 1, rest)
      } else {
        StreamNonCanonical
      }
    } else if first / 32 != 2 {
      StreamNonCanonical
    } else {
      head(
        bytes,
        limit,
        offset,
        first,
        fn(past, length) {
          if length == data_bytes_chunk {
            if and {
              past + length <= limit,
              !leading || bytearray.at(bytes, past) != 0,
            } {
              next(
                past + length,
                [ChunksFrame { seen: seen + 1, leading: False }, ..rest],
              )
            } else {
              StreamNonCanonical
            }
          } else if and {
            length > 0,
            length < data_bytes_chunk,
            seen >= 1,
            past + length < limit,
            bytearray.at(bytes, past + length) == break_byte,
          } {
            next(past + length + 1, rest)
          } else {
            StreamNonCanonical
          }
        },
      )
    }
  }
}
