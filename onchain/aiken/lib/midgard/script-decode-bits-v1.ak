//// Bit reader and streaming sinks for the script decode walk
//// (`script_decode_v1`).
////
//// Positions are bit offsets into a Flat buffer, read most-significant bit
//// first. The window is the Flat bytes followed by one zero byte, so a read of
//// up to eight bits never indexes past it; callers check a read against the
//// Flat length before making it.

use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_blob_frontier_v1.{CekBlobFrontierV1}
use midgard/cek_data_v1.{DataSummaryV1, IntegerData}
use midgard/cek_proof_v1

/// 2^e for 0 <= e <= 16.
pub fn pow2_small(e: Int) -> Int {
  if e < 8 {
    builtin.index_bytearray(#[1, 2, 4, 8, 16, 32, 64, 128], e)
  } else if e < 16 {
    256 * builtin.index_bytearray(#[1, 2, 4, 8, 16, 32, 64, 128], e - 8)
  } else {
    65536
  }
}

/// The `width` bits (at most 8) starting at bit `position`.
pub fn read_bits(window: ByteArray, position: Int, width: Int) -> Int {
  let index = position / 8
  let word = builtin.index_bytearray(window, index) * 256 + builtin.index_bytearray(
    window,
    index + 1,
  )
  word / pow2_small(16 - position % 8 - width) % pow2_small(width)
}

/// The number of significant bits of a byte (0 for 0).
pub fn bit_length8(byte: Int) -> Int {
  if byte >= 16 {
    if byte >= 64 {
      if byte >= 128 {
        8
      } else {
        7
      }
    } else if byte >= 32 {
      6
    } else {
      5
    }
  } else if byte >= 4 {
    if byte >= 8 {
      4
    } else {
      3
    }
  } else if byte >= 2 {
    2
  } else if byte >= 1 {
    1
  } else {
    0
  }
}

/// The position of the first set bit at or after `position` within the first
/// `flat_length` bytes of the window, if any.
pub fn next_set_bit(
  window: ByteArray,
  position: Int,
  flat_length: Int,
) -> Option<Int> {
  let index = position / 8
  if index >= flat_length {
    None
  } else {
    let masked = builtin.index_bytearray(window, index) % pow2_small(
      8 - position % 8,
    )
    if masked > 0 {
      Some(index * 8 + 8 - bit_length8(masked))
    } else {
      when first_nonzero_byte(window, index + 1, flat_length) is {
        None -> None
        Some(found) ->
          Some(
            found * 8 + 8 - bit_length8(builtin.index_bytearray(window, found)),
          )
      }
    }
  }
}

fn first_nonzero_byte(window: ByteArray, low: Int, high: Int) -> Option<Int> {
  if low >= high {
    None
  } else if builtin.index_bytearray(window, low) > 0 {
    Some(low)
  } else if builtin.count_set_bits(
    builtin.slice_bytearray(low, high - low, window),
  ) == 0 {
    None
  } else {
    Some(search_nonzero(window, low, high))
  }
}

/// A nonzero byte lies in [low, high); the first one.
fn search_nonzero(window: ByteArray, low: Int, high: Int) -> Int {
  if high - low == 1 {
    low
  } else {
    let middle = ( low + high ) / 2
    if builtin.count_set_bits(
      builtin.slice_bytearray(low, middle - low, window),
    ) > 0 {
      search_nonzero(window, low, middle)
    } else {
      search_nonzero(window, middle, high)
    }
  }
}

// ## Blob sink

/// A byte stream committed with the bounded blob root: full 4,095-byte chunks
/// enter the frontier and the rest waits in `pending`.
pub type BlobSink {
  frontier: CekBlobFrontierV1,
  pending: ByteArray,
}

pub fn empty_sink() -> BlobSink {
  BlobSink { frontier: cek_blob_frontier_v1.empty_frontier_v1(), pending: #"" }
}

pub fn sink_length(sink: BlobSink) -> Int {
  sink.frontier.byte_length + bytearray.length(sink.pending)
}

pub fn sink_push(sink: BlobSink, bytes: ByteArray) -> BlobSink {
  flush_sink(sink.frontier, bytearray.concat(sink.pending, bytes))
}

fn flush_sink(frontier: CekBlobFrontierV1, pending: ByteArray) -> BlobSink {
  if bytearray.length(pending) >= cek_proof_v1.max_blob_chunk_bytes {
    expect Some(next) = cek_blob_frontier_v1.append_chunk_v1(
      frontier,
      builtin.slice_bytearray(0, cek_proof_v1.max_blob_chunk_bytes, pending),
    )
    flush_sink(next, bytearray.drop(pending, cek_proof_v1.max_blob_chunk_bytes))
  } else {
    BlobSink { frontier, pending }
  }
}

/// The blob root of everything pushed; equal to `bounded_blob_root_v1` of the
/// whole stream.
pub fn sink_root(sink: BlobSink) -> ByteArray {
  let frontier = if bytearray.length(sink.pending) > 0 || sink.frontier.count == 0 {
    expect Some(appended) = cek_blob_frontier_v1.append_chunk_v1(
      sink.frontier,
      sink.pending,
    )
    appended
  } else {
    sink.frontier
  }
  expect Some(root) = cek_blob_frontier_v1.finalize_v1(frontier)
  root
}

/// The semantic summary of a byte string.
pub fn bytes_summary(length: Int, root: ByteArray) -> DataSummaryV1 {
  cek_data_v1.bytes_data_summary_v1(length, root)
}

/// The semantic summary of a byte string short enough for one blob chunk.
pub fn short_bytes_summary(bytes: ByteArray) -> DataSummaryV1 {
  cek_data_v1.bytes_data_summary_v1(
    bytearray.length(bytes),
    cek_proof_v1.hash_blob_chunk_v1(bytes),
  )
}

/// The semantic summary of an integer whose canonical CBOR fits one chunk.
pub fn integer_summary(value: Int) -> DataSummaryV1 {
  let encoded = cbor.serialise(value)
  let cbor_length = bytearray.length(encoded)
  let memory = 4 + cek_data_v1.integer_memory_size_v1(value)
  DataSummaryV1 {
    root: cek_data_v1.hash_data_node_v1(
      IntegerData {
        cbor_root: cek_proof_v1.hash_blob_chunk_v1(encoded),
        cbor_length,
        memory,
      },
    ),
    cbor_length,
    memory,
  }
}

// ## Magnitude sink

/// A big-endian integer magnitude streamed most-significant byte first, for
/// the integer `m` (positive) or `-1 - m` (negative). Leading zero bytes are
/// dropped. Up to 64 significant bytes are kept; past that the canonical CBOR
/// (a tag, an indefinite byte string of 64-byte chunks, a break) streams into
/// a blob sink, with what the memory size needs.
pub type MagSink {
  MagShort { negative: Bool, magnitude: ByteArray }
  MagLong {
    negative: Bool,
    sink: BlobSink,
    part: ByteArray,
    total: Int,
    first: Int,
    ones: Bool,
  }
}

pub fn mag_new(negative: Bool) -> MagSink {
  MagShort { negative, magnitude: #"" }
}

fn strip_zeros(bytes: ByteArray) -> ByteArray {
  if bytearray.length(bytes) == 0 {
    bytes
  } else if builtin.index_bytearray(bytes, 0) == 0 {
    strip_zeros(bytearray.drop(bytes, 1))
  } else {
    bytes
  }
}

fn all_ones(bytes: ByteArray) -> Bool {
  builtin.count_set_bits(bytes) == 8 * bytearray.length(bytes)
}

fn flush_magnitude(sink: BlobSink, part: ByteArray) -> (BlobSink, ByteArray) {
  if bytearray.length(part) >= 64 {
    flush_magnitude(
      sink_push(
        sink,
        bytearray.concat(#"5840", builtin.slice_bytearray(0, 64, part)),
      ),
      bytearray.drop(part, 64),
    )
  } else {
    (sink, part)
  }
}

fn long_push(
  negative: Bool,
  sink: BlobSink,
  part: ByteArray,
  total: Int,
  first: Int,
  ones: Bool,
  bytes: ByteArray,
) -> MagSink {
  let (next_sink, next_part) = flush_magnitude(
    sink,
    bytearray.concat(part, bytes),
  )
  MagLong {
    negative,
    sink: next_sink,
    part: next_part,
    total: total + bytearray.length(bytes),
    first,
    ones: ones && all_ones(bytes),
  }
}

pub fn mag_push(mag: MagSink, bytes: ByteArray) -> MagSink {
  when mag is {
    MagShort { negative, magnitude } -> {
      let joined = if bytearray.length(magnitude) == 0 {
        strip_zeros(bytes)
      } else {
        bytearray.concat(magnitude, bytes)
      }
      if bytearray.length(joined) <= 64 {
        MagShort { negative, magnitude: joined }
      } else {
        let tag = if negative {
          #"c35f"
        } else {
          #"c25f"
        }
        long_push(
          negative,
          sink_push(empty_sink(), tag),
          builtin.slice_bytearray(0, 1, joined),
          1,
          builtin.index_bytearray(joined, 0),
          True,
          bytearray.drop(joined, 1),
        )
      }
    }
    MagLong { negative, sink, part, total, first, ones } ->
      long_push(negative, sink, part, total, first, ones, bytes)
  }
}

/// The semantic summary of the integer and its memory size in words.
pub fn mag_finish(mag: MagSink) -> (DataSummaryV1, Int) {
  when mag is {
    MagShort { negative, magnitude } -> {
      let m = if bytearray.length(magnitude) == 0 {
        0
      } else {
        builtin.bytearray_to_integer(True, magnitude)
      }
      let value = if negative {
        -1 - m
      } else {
        m
      }
      (integer_summary(value), cek_data_v1.integer_memory_size_v1(value))
    }
    MagLong { negative, sink, part, total, first, ones } -> {
      let remainder = bytearray.length(part)
      let tail = if remainder == 0 {
        #"ff"
      } else if remainder < 24 {
        builtin.cons_bytearray(64 + remainder, bytearray.concat(part, #"ff"))
      } else {
        builtin.cons_bytearray(
          88,
          builtin.cons_bytearray(remainder, bytearray.concat(part, #"ff")),
        )
      }
      let closed = sink_push(sink, tail)
      let cbor_length = sink_length(closed)
      let first_bits = bit_length8(first)
      // |-1 - m| = m + 1 gains a bit when m is all one bits.
      let carry = if negative && ones && first == pow2_small(first_bits) - 1 {
        1
      } else {
        0
      }
      let words = ( 8 * ( total - 1 ) + first_bits + carry - 1 ) / 64 + 1
      let memory = 4 + words
      (
        DataSummaryV1 {
          root: cek_data_v1.hash_data_node_v1(
            IntegerData { cbor_root: sink_root(closed), cbor_length, memory },
          ),
          cbor_length,
          memory,
        },
        words,
      )
    }
  }
}

// ## UTF-8

/// UTF-8 validation state: 0 between characters, -1 after an invalid byte,
/// otherwise `need * 65536 + low * 256 + high`, where `need` continuation
/// bytes remain and the next one must lie in [low, high].
pub fn utf8_push(state: Int, bytes: ByteArray) -> Int {
  let length = bytearray.length(bytes)
  if state == 0 && builtin.count_set_bits(
    bytearray.and_bytes(bytes, builtin.replicate_byte(length, 128), False),
  ) == 0 {
    0
  } else {
    utf8_bytes(state, bytes, 0, length)
  }
}

fn utf8_bytes(state: Int, bytes: ByteArray, index: Int, length: Int) -> Int {
  if state < 0 || index >= length {
    state
  } else {
    utf8_bytes(
      utf8_byte(state, builtin.index_bytearray(bytes, index)),
      bytes,
      index + 1,
      length,
    )
  }
}

fn utf8_byte(state: Int, byte: Int) -> Int {
  if state == 0 {
    if byte < 128 {
      0
    } else if byte < 194 {
      -1
    } else if byte < 224 {
      65536 + 128 * 256 + 191
    } else if byte == 224 {
      2 * 65536 + 160 * 256 + 191
    } else if byte == 237 {
      2 * 65536 + 128 * 256 + 159
    } else if byte < 240 {
      2 * 65536 + 128 * 256 + 191
    } else if byte == 240 {
      3 * 65536 + 144 * 256 + 191
    } else if byte < 244 {
      3 * 65536 + 128 * 256 + 191
    } else if byte == 244 {
      3 * 65536 + 128 * 256 + 143
    } else {
      -1
    }
  } else {
    let need = state / 65536
    if byte < state / 256 % 256 || byte > state % 256 {
      -1
    } else if need == 1 {
      0
    } else {
      ( need - 1 ) * 65536 + 128 * 256 + 191
    }
  }
}
