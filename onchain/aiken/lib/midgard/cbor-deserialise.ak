//// Decodes Plutus Data CBOR exactly as the Cardano ledger does.
////
//// Two entries share one reader and differ in one parameter: the most bytes a
//// definite byte string, or one chunk of an indefinite byte string, may hold.
////
//// - The Data entry, `deserialise` and `expect_deserialise`, decodes exactly
////   the Plutus Data CBOR the Cardano ledger accepts (`decodeData` in
////   plutus-core), to the same value, and refuses everything else. Use it for
////   anything claimed to be Plutus Data: datums, redeemers, script contexts,
////   CEK constants and Data nodes.
//// - The envelope entry, `deserialise_envelope` and
////   `expect_deserialise_envelope`, applies the same rules without the 64-byte
////   bound. Use it only for Midgard-internal wire whose writers emit long byte
////   strings definite: work witnesses, controls, proof carriage, CEK program
////   material and discovery bitmaps.
////
//// The rules:
////
//// - The input is exactly one item: trailing bytes are refused.
//// - Heads may be non-minimal. Additional information 28 to 30 is refused, and
////   31 (indefinite) only opens a byte string, a list or a map.
//// - An integer is a major-type-0 or major-type-1 head, or a bignum: the
////   one-byte tag head `c2` (positive) or `c3` (negative) over a byte string,
////   big-endian, the empty string being 0 or -1. Tags 2 and 3 written with a
////   longer head are unknown tags.
//// - A byte string is definite, or indefinite with chunks that are each a
////   definite byte string. Under the Data entry a definite string, and every
////   chunk, holds at most 64 bytes, bignum magnitudes included.
//// - Lists and maps are definite or indefinite. Map entries keep their order,
////   duplicate keys included.
//// - A constructor is tag 121 to 127 (index 0 to 6) or 1280 to 1400 (index 7
////   to 127) over a list, or tag 102 over a two-element array, definite or
////   indefinite, of a major-type-0 index and a list of fields. Any other tag,
////   and any of these tags over anything else, is refused.
//// - Text strings, simple values, floats and a misplaced break are refused.
////
//// Both variants stop at the first refused read: `deserialise` returns `None`
//// there and `expect_deserialise` fails the script there. So a refused input
//// that declares a long list or map costs no more than the bytes it holds.

use aiken/builtin.{
  append_bytearray, b_data, bytearray_to_integer, choose_data, constr_data,
  i_data, index_bytearray, length_of_bytearray, list_data, map_data,
  slice_bytearray,
}

const token_begin_bytes = 0x5f

const token_begin_list = 0x9f

const token_begin_map = 0xbf

const token_break = 0xff

/// The Cardano ledger's bound on a definite byte string or one chunk.
const data_bytes_bound = 64

// The envelope entry bounds a byte string by the length of the whole input.
// No byte string inside the input can be longer, so that is no bound at all.

/// `Some(data)` when `bytes` is exactly one item of Plutus Data CBOR that the
/// Cardano ledger accepts, and `None` otherwise.
pub fn deserialise(bytes: ByteArray) -> Option<Data> {
  maybe_decode(bytes, data_bytes_bound)
}

/// `deserialise` without the 64-byte bound on a byte string or chunk, for
/// Midgard-internal wire only.
pub fn deserialise_envelope(bytes: ByteArray) -> Option<Data> {
  maybe_decode(bytes, length_of_bytearray(bytes))
}

fn maybe_decode(bytes: ByteArray, bound: Int) -> Option<Data> {
  let length = length_of_bytearray(bytes)
  maybe_item(
    bytes,
    length,
    bound,
    0,
    fn(data, offset) {
      if offset == length {
        Some(data)
      } else {
        None
      }
    },
  )
}

type MaybeDecode =
  fn(ByteArray, Int, Int, Int, fn(Data, Int) -> Option<Data>) -> Option<Data>

fn maybe_item(
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  return: fn(Data, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else {
    let next = index_bytearray(bytes, offset)
    let offset = offset + 1
    if next < 0x18 {
      return(i_data(next), offset)
    } else if next < 0x20 {
      maybe_argument(
        bytes,
        length,
        offset,
        next,
        fn(n, offset) { return(i_data(n), offset) },
      )
    } else if next < 0x40 {
      maybe_argument(
        bytes,
        length,
        offset,
        next - 0x20,
        fn(n, offset) { return(i_data(-1 - n), offset) },
      )
    } else if next == 0x58 {
      if offset >= length {
        None
      } else {
        let width = index_bytearray(bytes, offset)
        if width > bound {
          None
        } else {
          return(
            b_data(slice_bytearray(offset + 1, width, bytes)),
            offset + 1 + width,
          )
        }
      }
    } else if next < 0x80 {
      maybe_byte_string(
        bytes,
        length,
        bound,
        offset,
        next,
        fn(b, offset) { return(b_data(b), offset) },
      )
    } else if next < 0x98 {
      maybe_sized_items(
        maybe_item,
        bytes,
        length,
        bound,
        offset,
        next - 0x80,
        fn(xs, offset) { return(list_data(xs), offset) },
      )
    } else if next < 0xa0 {
      maybe_list(
        maybe_item,
        bytes,
        length,
        bound,
        offset,
        next,
        fn(xs, offset) { return(list_data(xs), offset) },
      )
    } else if next < 0xc0 {
      if next == token_begin_map {
        maybe_entries(
          maybe_item,
          bytes,
          length,
          bound,
          offset,
          fn(xs, offset) { return(map_data(xs), offset) },
        )
      } else {
        maybe_argument(
          bytes,
          length,
          offset,
          next - 0xa0,
          fn(size, offset) {
            maybe_sized_entries(
              maybe_item,
              bytes,
              length,
              bound,
              offset,
              size,
              fn(xs, offset) { return(map_data(xs), offset) },
            )
          },
        )
      }
    } else if next == 0xc2 || next == 0xc3 {
      if offset >= length {
        None
      } else {
        maybe_byte_string(
          bytes,
          length,
          bound,
          offset + 1,
          index_bytearray(bytes, offset),
          fn(magnitude, offset) {
            let n = bytearray_to_integer(True, magnitude)
            return(
              i_data(
                if next == 0xc2 {
                  n
                } else {
                  -1 - n
                },
              ),
              offset,
            )
          },
        )
      }
    } else {
      maybe_argument(
        bytes,
        length,
        offset,
        next - 0xc0,
        fn(tag, offset) {
          maybe_constr(maybe_item, bytes, length, bound, offset, tag, return)
        },
      )
    }
  }
}

// A constructor's index from its compact tag, or -1 for any other tag.
fn compact_index(tag: Int) -> Int {
  if tag < 128 {
    if tag < 121 {
      -1
    } else {
      tag - 121
    }
  } else if tag < 1280 || tag > 1400 {
    -1
  } else {
    tag - 1273
  }
}

fn maybe_constr(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  tag: Int,
  return: fn(Data, Int) -> Option<Data>,
) -> Option<Data> {
  let ix = compact_index(tag)
  if ix >= 0 {
    maybe_fields(
      decode,
      bytes,
      length,
      bound,
      offset,
      fn(xs, offset) { return(constr_data(ix, xs), offset) },
    )
  } else if tag != 102 || offset >= length {
    None
  } else {
    let head = index_bytearray(bytes, offset)
    let indefinite = head == token_begin_list
    maybe_argument(
      bytes,
      length,
      offset + 1,
      if indefinite {
        2
      } else {
        head - 0x80
      },
      fn(size, offset) {
        if size != 2 || offset >= length {
          None
        } else {
          maybe_argument(
            bytes,
            length,
            offset + 1,
            index_bytearray(bytes, offset),
            fn(ix, offset) {
              maybe_fields(
                decode,
                bytes,
                length,
                bound,
                offset,
                fn(xs, offset) {
                  if !indefinite {
                    return(constr_data(ix, xs), offset)
                  } else if offset >= length {
                    None
                  } else if index_bytearray(bytes, offset) == token_break {
                    return(constr_data(ix, xs), offset + 1)
                  } else {
                    None
                  }
                },
              )
            },
          )
        }
      },
    )
  }
}

fn maybe_fields(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  return: fn(List<Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else {
    maybe_list(
      decode,
      bytes,
      length,
      bound,
      offset + 1,
      index_bytearray(bytes, offset),
      return,
    )
  }
}

// `head` is the list's initial byte. One below 0x80 gives a negative size,
// which no input reaches, so the read runs out of input and refuses.
fn maybe_list(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  head: Int,
  return: fn(List<Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if head == token_begin_list {
    maybe_items(decode, bytes, length, bound, offset, return)
  } else {
    maybe_argument(
      bytes,
      length,
      offset,
      head - 0x80,
      fn(size, offset) {
        maybe_sized_items(decode, bytes, length, bound, offset, size, return)
      },
    )
  }
}

// A head's argument. `header` is its additional information; a negative one
// is returned as is, and every caller refuses it or never completes on it.
fn maybe_argument(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  header: Int,
  return: fn(Int, Int) -> Option<Data>,
) -> Option<Data> {
  if header < 24 {
    return(header, offset)
  } else if offset >= length {
    None
  } else if header == 24 {
    return(index_bytearray(bytes, offset), offset + 1)
  } else if header < 28 {
    let width = index_bytearray(#"020408", header - 25)
    return(
      bytearray_to_integer(True, slice_bytearray(offset, width, bytes)),
      offset + width,
    )
  } else {
    None
  }
}

// `head` is the byte string's initial byte; one below 0x40 is refused.
fn maybe_byte_string(
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  head: Int,
  return: fn(ByteArray, Int) -> Option<Data>,
) -> Option<Data> {
  if head == token_begin_bytes {
    maybe_chunks(bytes, length, bound, offset, #"", return)
  } else {
    maybe_definite_bytes(bytes, length, bound, offset, head, return)
  }
}

// A string that runs past the end of the input is sliced short, but the
// position then lies past the end, so every later read and the final check
// refuse.
fn maybe_definite_bytes(
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  head: Int,
  return: fn(ByteArray, Int) -> Option<Data>,
) -> Option<Data> {
  if head < 0x40 {
    None
  } else {
    maybe_argument(
      bytes,
      length,
      offset,
      head - 0x40,
      fn(width, offset) {
        if width > bound {
          None
        } else {
          return(slice_bytearray(offset, width, bytes), offset + width)
        }
      },
    )
  }
}

fn maybe_chunks(
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  read: ByteArray,
  return: fn(ByteArray, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else {
    let head = index_bytearray(bytes, offset)
    let offset = offset + 1
    if head == token_break {
      return(read, offset)
    } else {
      maybe_definite_bytes(
        bytes,
        length,
        bound,
        offset,
        head,
        fn(chunk, offset) {
          maybe_chunks(
            bytes,
            length,
            bound,
            offset,
            append_bytearray(read, chunk),
            return,
          )
        },
      )
    }
  }
}

fn maybe_items(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  return: fn(List<Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else if index_bytearray(bytes, offset) == token_break {
    return([], offset + 1)
  } else {
    decode(
      bytes,
      length,
      bound,
      offset,
      fn(x, offset) {
        maybe_items(
          decode,
          bytes,
          length,
          bound,
          offset,
          fn(xs, offset) { return([x, ..xs], offset) },
        )
      },
    )
  }
}

fn maybe_sized_items(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  size: Int,
  return: fn(List<Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if size == 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      length,
      bound,
      offset,
      fn(x, offset) {
        maybe_sized_items(
          decode,
          bytes,
          length,
          bound,
          offset,
          size - 1,
          fn(xs, offset) { return([x, ..xs], offset) },
        )
      },
    )
  }
}

fn maybe_entries(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  return: fn(Pairs<Data, Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else if index_bytearray(bytes, offset) == token_break {
    return([], offset + 1)
  } else {
    decode(
      bytes,
      length,
      bound,
      offset,
      fn(k, offset) {
        decode(
          bytes,
          length,
          bound,
          offset,
          fn(v, offset) {
            maybe_entries(
              decode,
              bytes,
              length,
              bound,
              offset,
              fn(xs, offset) { return([Pair(k, v), ..xs], offset) },
            )
          },
        )
      },
    )
  }
}

fn maybe_sized_entries(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  bound: Int,
  offset: Int,
  size: Int,
  return: fn(Pairs<Data, Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if size == 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      length,
      bound,
      offset,
      fn(k, offset) {
        decode(
          bytes,
          length,
          bound,
          offset,
          fn(v, offset) {
            maybe_sized_entries(
              decode,
              bytes,
              length,
              bound,
              offset,
              size - 1,
              fn(xs, offset) { return([Pair(k, v), ..xs], offset) },
            )
          },
        )
      },
    )
  }
}

// ## Failing variant
//
// The same reads, with a refused read failing the script on the spot. So most
// end-of-input checks fall away: a read past the end fails in
// `index_bytearray`, and a byte string or a 2-, 4- or 8-byte argument that
// runs past the end leaves the position past the end for good, so the next
// read, or the final check that the whole input was read, fails. A text
// string (major type 3) is read as a byte string head, and a float or simple
// value (major type 7) as a tag head; both have additional information 28 or
// more relative to that major type, which `argument` refuses.
//
// Script size: the reads are written for size, since every script that
// decodes links them. A head's argument is read through `argument`, and the
// 2-, 4- or 8-byte width comes from a table, at the cost of a call or an index
// per item. The commonest heads are read in line instead, in both variants:
// an integer below 24, and a byte string or chunk with a one-byte length
// (`58`). `deserialise` also reads a list of fewer than 24 items in line.

/// Continues with the `data` that `deserialise(bytes)` returns as
/// `Some(data)`, and fails the script where it returns `None`. Use it with
/// backpassing, `let data <- cbor_deserialise.expect_deserialise(bytes)`. The
/// decoding happens at that point, on every path, like an `expect` does:
/// `data` is used twice in `expect_decoded`, once to pick `then` (every arm of
/// `choose_data` is `then`) and once as its argument. Without that, the
/// compiler may move a value used once into the branch that uses it, so the
/// decoding, and its failure, would no longer happen on every path.
pub fn expect_deserialise(bytes: ByteArray, then: fn(Data) -> r) -> r {
  expect_decoded(decode(bytes, data_bytes_bound), then)
}

/// `expect_deserialise` without the 64-byte bound on a byte string or chunk,
/// for Midgard-internal wire only.
pub fn expect_deserialise_envelope(bytes: ByteArray, then: fn(Data) -> r) -> r {
  expect_decoded(decode(bytes, length_of_bytearray(bytes)), then)
}

fn expect_decoded(data: Data, then: fn(Data) -> r) -> r {
  choose_data(data, then, then, then, then, then)(data)
}

fn decode(bytes: ByteArray, bound: Int) -> Data {
  item(
    bytes,
    bound,
    0,
    fn(data, offset) {
      expect offset == length_of_bytearray(bytes)
      data
    },
  )
}

type Decode =
  fn(ByteArray, Int, Int, fn(Data, Int) -> Data) -> Data

fn item(
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  return: fn(Data, Int) -> Data,
) -> Data {
  let next = index_bytearray(bytes, offset)
  let offset = offset + 1
  if next < 0x18 {
    return(i_data(next), offset)
  } else if next < 0x20 {
    argument(bytes, offset, next, fn(n, offset) { return(i_data(n), offset) })
  } else if next < 0x40 {
    argument(
      bytes,
      offset,
      next - 0x20,
      fn(n, offset) { return(i_data(-1 - n), offset) },
    )
  } else if next == 0x58 {
    let width = index_bytearray(bytes, offset)
    expect width <= bound
    return(
      b_data(slice_bytearray(offset + 1, width, bytes)),
      offset + 1 + width,
    )
  } else if next < 0x80 {
    byte_string(
      bytes,
      bound,
      offset,
      next,
      fn(b, offset) { return(b_data(b), offset) },
    )
  } else if next < 0xa0 {
    list(
      item,
      bytes,
      bound,
      offset,
      next,
      fn(xs, offset) { return(list_data(xs), offset) },
    )
  } else if next < 0xc0 {
    if next == token_begin_map {
      entries(
        item,
        bytes,
        bound,
        offset,
        fn(xs, offset) { return(map_data(xs), offset) },
      )
    } else {
      argument(
        bytes,
        offset,
        next - 0xa0,
        fn(size, offset) {
          sized_entries(
            item,
            bytes,
            bound,
            offset,
            size,
            fn(xs, offset) { return(map_data(xs), offset) },
          )
        },
      )
    }
  } else if next == 0xc2 || next == 0xc3 {
    byte_string(
      bytes,
      bound,
      offset + 1,
      index_bytearray(bytes, offset),
      fn(magnitude, offset) {
        let n = bytearray_to_integer(True, magnitude)
        return(
          i_data(
            if next == 0xc2 {
              n
            } else {
              -1 - n
            },
          ),
          offset,
        )
      },
    )
  } else {
    argument(
      bytes,
      offset,
      next - 0xc0,
      fn(tag, offset) { constr(item, bytes, bound, offset, tag, return) },
    )
  }
}

fn constr(
  decode: Decode,
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  tag: Int,
  return: fn(Data, Int) -> Data,
) -> Data {
  let ix = compact_index(tag)
  if ix >= 0 {
    fields(
      decode,
      bytes,
      bound,
      offset,
      fn(xs, offset) { return(constr_data(ix, xs), offset) },
    )
  } else {
    expect tag == 102
    let head = index_bytearray(bytes, offset)
    let indefinite = head == token_begin_list
    argument(
      bytes,
      offset + 1,
      if indefinite {
        2
      } else {
        head - 0x80
      },
      fn(size, offset) {
        expect size == 2
        argument(
          bytes,
          offset + 1,
          index_bytearray(bytes, offset),
          fn(ix, offset) {
            fields(
              decode,
              bytes,
              bound,
              offset,
              fn(xs, offset) {
                return(
                  constr_data(ix, xs),
                  if indefinite {
                    expect index_bytearray(bytes, offset) == token_break
                    offset + 1
                  } else {
                    offset
                  },
                )
              },
            )
          },
        )
      },
    )
  }
}

fn fields(
  decode: Decode,
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  return: fn(List<Data>, Int) -> Data,
) -> Data {
  list(decode, bytes, bound, offset + 1, index_bytearray(bytes, offset), return)
}

fn list(
  decode: Decode,
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  head: Int,
  return: fn(List<Data>, Int) -> Data,
) -> Data {
  if head == token_begin_list {
    items(decode, bytes, bound, offset, return)
  } else {
    argument(
      bytes,
      offset,
      head - 0x80,
      fn(size, offset) {
        sized_items(decode, bytes, bound, offset, size, return)
      },
    )
  }
}

fn argument(
  bytes: ByteArray,
  offset: Int,
  header: Int,
  return: fn(Int, Int) -> Data,
) -> Data {
  if header < 24 {
    return(header, offset)
  } else if header == 24 {
    return(index_bytearray(bytes, offset), offset + 1)
  } else {
    // 2, 4 or 8 bytes for 25, 26 or 27; out of range, so failing, for more.
    let width = index_bytearray(#"020408", header - 25)
    return(
      bytearray_to_integer(True, slice_bytearray(offset, width, bytes)),
      offset + width,
    )
  }
}

fn byte_string(
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  head: Int,
  return: fn(ByteArray, Int) -> Data,
) -> Data {
  if head == token_begin_bytes {
    chunks(bytes, bound, offset, #"", return)
  } else {
    definite_bytes(bytes, bound, offset, head, return)
  }
}

fn definite_bytes(
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  head: Int,
  return: fn(ByteArray, Int) -> Data,
) -> Data {
  expect head >= 0x40
  argument(
    bytes,
    offset,
    head - 0x40,
    fn(width, offset) {
      expect width <= bound
      return(slice_bytearray(offset, width, bytes), offset + width)
    },
  )
}

fn chunks(
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  read: ByteArray,
  return: fn(ByteArray, Int) -> Data,
) -> Data {
  let head = index_bytearray(bytes, offset)
  let offset = offset + 1
  if head == token_break {
    return(read, offset)
  } else if head == 0x58 {
    let width = index_bytearray(bytes, offset)
    expect width <= bound
    chunks(
      bytes,
      bound,
      offset + 1 + width,
      append_bytearray(read, slice_bytearray(offset + 1, width, bytes)),
      return,
    )
  } else {
    definite_bytes(
      bytes,
      bound,
      offset,
      head,
      fn(chunk, offset) {
        chunks(bytes, bound, offset, append_bytearray(read, chunk), return)
      },
    )
  }
}

fn items(
  decode: Decode,
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  return: fn(List<Data>, Int) -> Data,
) -> Data {
  if index_bytearray(bytes, offset) == token_break {
    return([], offset + 1)
  } else {
    decode(
      bytes,
      bound,
      offset,
      fn(x, offset) {
        items(
          decode,
          bytes,
          bound,
          offset,
          fn(xs, offset) { return([x, ..xs], offset) },
        )
      },
    )
  }
}

fn sized_items(
  decode: Decode,
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  size: Int,
  return: fn(List<Data>, Int) -> Data,
) -> Data {
  if size == 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      bound,
      offset,
      fn(x, offset) {
        sized_items(
          decode,
          bytes,
          bound,
          offset,
          size - 1,
          fn(xs, offset) { return([x, ..xs], offset) },
        )
      },
    )
  }
}

fn entries(
  decode: Decode,
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  return: fn(Pairs<Data, Data>, Int) -> Data,
) -> Data {
  if index_bytearray(bytes, offset) == token_break {
    return([], offset + 1)
  } else {
    decode(
      bytes,
      bound,
      offset,
      fn(k, offset) {
        decode(
          bytes,
          bound,
          offset,
          fn(v, offset) {
            entries(
              decode,
              bytes,
              bound,
              offset,
              fn(xs, offset) { return([Pair(k, v), ..xs], offset) },
            )
          },
        )
      },
    )
  }
}

fn sized_entries(
  decode: Decode,
  bytes: ByteArray,
  bound: Int,
  offset: Int,
  size: Int,
  return: fn(Pairs<Data, Data>, Int) -> Data,
) -> Data {
  if size == 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      bound,
      offset,
      fn(k, offset) {
        decode(
          bytes,
          bound,
          offset,
          fn(v, offset) {
            sized_entries(
              decode,
              bytes,
              bound,
              offset,
              size - 1,
              fn(xs, offset) { return([Pair(k, v), ..xs], offset) },
            )
          },
        )
      },
    )
  }
}
