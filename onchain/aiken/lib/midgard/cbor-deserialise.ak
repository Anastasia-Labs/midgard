//// A cheaper drop-in for the standard library's `aiken/cbor.deserialise`.
////
//// The standard library threads every read through closures that return a
//// `Pair<Data, Int>`, which costs about 23k memory units per decoded item.
//// This module performs the same reads, in the same order, with the same
//// arithmetic, in continuation-passing style, so no pair is built per item.
////
//// Equivalence with `aiken/cbor.deserialise`, which the tests in
//// `cbor-deserialise.test.ak` check differentially:
////
//// - The position is the offset read so far, `length - cursor` in the
////   library's terms. "No input left" (`0 >= cursor`) is `offset >= length`,
////   and the final `consumed != 0` is `offset != length`. Every byte read is
////   `index_bytearray(bytes, offset)`, so a read before the start of the
////   input (possible after a negative chunk width moves the position back)
////   fails the script exactly where the library's `bytearray.at` does.
//// - Once the library hits a failure its position is -1 for good: every
////   later read fails at once and the result is `None`. So this module
////   returns `None` at the first failure instead of carrying it on.
//// - Each accepted item is built with the same builtin from the same values,
////   so every `Some` result is the same `Data`.
//// - Library quirks are kept on purpose: a byte string, or a 2-, 4- or
////   8-byte integer argument, that would start at the end of the input fails
////   even when its width is 0; a tag reads the next byte before it refuses
////   tag 102; a tag's field count is read from any next byte as
////   `byte - 128`; and a chunk header inside an indefinite byte string is
////   read from any byte as `byte - 64`.

use aiken/builtin.{
  append_bytearray, b_data, bytearray_to_integer, choose_data, constr_data,
  i_data, index_bytearray, length_of_bytearray, list_data, map_data,
  slice_bytearray, un_b_data,
}

const token_begin_bytes = 0x5f

const token_begin_list = 0x9f

const token_begin_map = 0xbf

const token_break = 0xff

/// `Some(data)` when `bytes` is exactly one item that `aiken/cbor.deserialise`
/// accepts, with the same `data`; `None` whenever that function returns
/// `None`.
pub fn deserialise(bytes: ByteArray) -> Option<Data> {
  let length = length_of_bytearray(bytes)
  if length == 0 {
    None
  } else {
    maybe_item(
      bytes,
      length,
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
}

type MaybeDecode =
  fn(ByteArray, Int, Int, fn(Data, Int) -> Option<Data>) -> Option<Data>

fn maybe_item(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  return: fn(Data, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else {
    let next = index_bytearray(bytes, offset)
    let offset = offset + 1
    if next < 0x80 {
      if next < 0x40 {
        if next < 0x18 {
          return(i_data(next), offset)
        } else if next < 0x20 {
          maybe_argument(
            bytes,
            length,
            offset,
            next,
            fn(i, offset) { return(i_data(i), offset) },
          )
        } else if next < 0x38 {
          return(i_data(0x1f - next), offset)
        } else {
          maybe_argument(
            bytes,
            length,
            offset,
            next - 0x20,
            fn(i, offset) { return(i_data(-i - 1), offset) },
          )
        }
      } else if next < 0x58 {
        maybe_byte_string(
          bytes,
          length,
          offset,
          next - 0x40,
          fn(b, offset) { return(b_data(b), offset) },
        )
      } else if next == token_begin_bytes {
        maybe_chunks(
          bytes,
          length,
          offset,
          fn(b, offset) { return(b_data(b), offset) },
        )
      } else if next < 0x60 {
        maybe_argument(
          bytes,
          length,
          offset,
          next - 0x40,
          fn(width, offset) {
            maybe_byte_string(
              bytes,
              length,
              offset,
              width,
              fn(b, offset) { return(b_data(b), offset) },
            )
          },
        )
      } else {
        None
      }
    } else if next < 0xc0 {
      if next < 0xa0 {
        if next == token_begin_list {
          maybe_items(
            maybe_item,
            bytes,
            length,
            offset,
            fn(xs, offset) { return(list_data(xs), offset) },
          )
        } else {
          maybe_argument(
            bytes,
            length,
            offset,
            next - 0x80,
            fn(size, offset) {
              maybe_sized_items(
                maybe_item,
                bytes,
                length,
                offset,
                size,
                fn(xs, offset) { return(list_data(xs), offset) },
              )
            },
          )
        }
      } else if next == token_begin_map {
        maybe_entries(
          maybe_item,
          bytes,
          length,
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
              offset,
              size,
              fn(xs, offset) { return(map_data(xs), offset) },
            )
          },
        )
      }
    } else if next < 0xe0 {
      maybe_argument(
        bytes,
        length,
        offset,
        next - 0xc0,
        fn(tag, offset) {
          maybe_constr(maybe_item, bytes, length, offset, tag, return)
        },
      )
    } else {
      None
    }
  }
}

fn maybe_constr(
  decode: MaybeDecode,
  bytes: ByteArray,
  length: Int,
  offset: Int,
  tag: Int,
  return: fn(Data, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else {
    let next = index_bytearray(bytes, offset)
    let offset = offset + 1
    if tag == 102 {
      None
    } else {
      let ix = if tag >= 1280 {
        tag - 1273
      } else {
        tag - 121
      }
      if next == token_begin_list {
        maybe_items(
          decode,
          bytes,
          length,
          offset,
          fn(xs, offset) { return(constr_data(ix, xs), offset) },
        )
      } else {
        maybe_argument(
          bytes,
          length,
          offset,
          next - 0x80,
          fn(size, offset) {
            maybe_sized_items(
              decode,
              bytes,
              length,
              offset,
              size,
              fn(xs, offset) { return(constr_data(ix, xs), offset) },
            )
          },
        )
      }
    }
  }
}

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
  } else if header == 25 {
    return(
      bytearray_to_integer(True, slice_bytearray(offset, 2, bytes)),
      offset + 2,
    )
  } else if header == 26 {
    return(
      bytearray_to_integer(True, slice_bytearray(offset, 4, bytes)),
      offset + 4,
    )
  } else if header == 27 {
    return(
      bytearray_to_integer(True, slice_bytearray(offset, 8, bytes)),
      offset + 8,
    )
  } else {
    None
  }
}

fn maybe_byte_string(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  width: Int,
  return: fn(ByteArray, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else {
    return(slice_bytearray(offset, width, bytes), offset + width)
  }
}

fn maybe_chunks(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  return: fn(ByteArray, Int) -> Option<Data>,
) -> Option<Data> {
  if offset >= length {
    None
  } else {
    let next = index_bytearray(bytes, offset)
    let offset = offset + 1
    if next == token_break {
      return("", offset)
    } else {
      maybe_argument(
        bytes,
        length,
        offset,
        next - 0x40,
        fn(width, offset) {
          maybe_byte_string(
            bytes,
            length,
            offset,
            width,
            fn(chunk, offset) {
              maybe_chunks(
                bytes,
                length,
                offset,
                fn(rest, offset) {
                  return(append_bytearray(chunk, rest), offset)
                },
              )
            },
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
      offset,
      fn(x, offset) {
        maybe_items(
          decode,
          bytes,
          length,
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
  offset: Int,
  size: Int,
  return: fn(List<Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if size <= 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      length,
      offset,
      fn(x, offset) {
        maybe_sized_items(
          decode,
          bytes,
          length,
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
      offset,
      fn(k, offset) {
        decode(
          bytes,
          length,
          offset,
          fn(v, offset) {
            maybe_entries(
              decode,
              bytes,
              length,
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
  offset: Int,
  size: Int,
  return: fn(Pairs<Data, Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if size <= 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      length,
      offset,
      fn(k, offset) {
        decode(
          bytes,
          length,
          offset,
          fn(v, offset) {
            maybe_sized_entries(
              decode,
              bytes,
              length,
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
// `expect_deserialise(bytes)` returns `data` exactly when
// `expect Some(data) = cbor.deserialise(bytes)` succeeds, and fails the
// script wherever that `expect` fails. Since a failed read now fails the script
// on the spot, most end-of-input checks fall away:
//
// - A read past the end of the input fails in `index_bytearray`, where the
//   library returns `None`.
// - A byte string or a 2-, 4- or 8-byte argument that runs past the end is
//   sliced short, but the position then lies past the end for good: every later
//   read fails, and so does the final check that the whole input was read. The
//   position only moves back after a chunk header, which is itself a read.
// - The library also refuses a byte string that starts at the end of the input
//   when its width is 0 or less. That case reads nothing, so it keeps an
//   explicit check.

/// Continues with the `data` that `expect Some(data) = cbor.deserialise(bytes)`
/// binds; fails wherever that `expect` fails. Use it with backpassing,
/// `let data <- cbor_deserialise.expect_deserialise(bytes)`. The decoding
/// happens at that point, on every path, like the `expect` does: `data` is
/// used twice below, once to pick `then` (every arm of `choose_data` is
/// `then`) and once as its argument. Without that, the compiler may move a
/// value used once into the branch that uses it, so the decoding, and its
/// failure, would no longer happen on every path.
pub fn expect_deserialise(bytes: ByteArray, then: fn(Data) -> r) -> r {
  let data = decode(bytes)
  choose_data(data, then, then, then, then, then)(data)
}

fn decode(bytes: ByteArray) -> Data {
  item(
    bytes,
    0,
    fn(data, offset) {
      expect offset == length_of_bytearray(bytes)
      data
    },
  )
}

type Decode =
  fn(ByteArray, Int, fn(Data, Int) -> Data) -> Data

fn item(bytes: ByteArray, offset: Int, return: fn(Data, Int) -> Data) -> Data {
  let next = index_bytearray(bytes, offset)
  let offset = offset + 1
  if next < 0x80 {
    if next < 0x40 {
      if next < 0x18 {
        return(i_data(next), offset)
      } else if next < 0x20 {
        argument(
          bytes,
          offset,
          next,
          fn(i, offset) { return(i_data(i), offset) },
        )
      } else if next < 0x38 {
        return(i_data(0x1f - next), offset)
      } else {
        argument(
          bytes,
          offset,
          next - 0x20,
          fn(i, offset) { return(i_data(-1 - i), offset) },
        )
      }
    } else if next < 0x58 {
      byte_string(bytes, offset, next - 0x40, return)
    } else if next == 0x58 {
      byte_string(bytes, offset + 1, index_bytearray(bytes, offset), return)
    } else if next == token_begin_bytes {
      chunks(bytes, offset, return)
    } else if next < 0x60 {
      argument(
        bytes,
        offset,
        next - 0x40,
        fn(width, offset) { byte_string(bytes, offset, width, return) },
      )
    } else {
      fail
    }
  } else if next < 0xc0 {
    if next < 0xa0 {
      if next == token_begin_list {
        items(
          item,
          bytes,
          offset,
          fn(xs, offset) { return(list_data(xs), offset) },
        )
      } else {
        argument(
          bytes,
          offset,
          next - 0x80,
          fn(size, offset) {
            sized_items(
              item,
              bytes,
              offset,
              size,
              fn(xs, offset) { return(list_data(xs), offset) },
            )
          },
        )
      }
    } else if next == token_begin_map {
      entries(
        item,
        bytes,
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
            offset,
            size,
            fn(xs, offset) { return(map_data(xs), offset) },
          )
        },
      )
    }
  } else if next == 0xd8 {
    constr(item, bytes, offset + 1, index_bytearray(bytes, offset), return)
  } else if next < 0xe0 {
    argument(
      bytes,
      offset,
      next - 0xc0,
      fn(tag, offset) { constr(item, bytes, offset, tag, return) },
    )
  } else {
    fail
  }
}

fn constr(
  decode: Decode,
  bytes: ByteArray,
  offset: Int,
  tag: Int,
  return: fn(Data, Int) -> Data,
) -> Data {
  let next = index_bytearray(bytes, offset)
  let offset = offset + 1
  if tag == 102 {
    fail
  } else {
    let ix = if tag >= 1280 {
      tag - 1273
    } else {
      tag - 121
    }
    if next == token_begin_list {
      items(
        decode,
        bytes,
        offset,
        fn(xs, offset) { return(constr_data(ix, xs), offset) },
      )
    } else if next == 0x80 {
      return(constr_data(ix, []), offset)
    } else {
      argument(
        bytes,
        offset,
        next - 0x80,
        fn(size, offset) {
          sized_items(
            decode,
            bytes,
            offset,
            size,
            fn(xs, offset) { return(constr_data(ix, xs), offset) },
          )
        },
      )
    }
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
    let width = if header == 25 {
      2
    } else if header == 26 {
      4
    } else if header == 27 {
      8
    } else {
      fail
    }
    return(
      bytearray_to_integer(True, slice_bytearray(offset, width, bytes)),
      offset + width,
    )
  }
}

fn byte_string(
  bytes: ByteArray,
  offset: Int,
  width: Int,
  return: fn(Data, Int) -> Data,
) -> Data {
  if width > 0 {
    return(b_data(slice_bytearray(offset, width, bytes)), offset + width)
  } else {
    expect offset < length_of_bytearray(bytes)
    return(b_data(slice_bytearray(offset, width, bytes)), offset + width)
  }
}

fn chunks(bytes: ByteArray, offset: Int, return: fn(Data, Int) -> Data) -> Data {
  let next = index_bytearray(bytes, offset)
  let offset = offset + 1
  if next == token_break {
    return(b_data(""), offset)
  } else {
    argument(
      bytes,
      offset,
      next - 0x40,
      fn(width, offset) {
        byte_string(
          bytes,
          offset,
          width,
          fn(chunk, offset) {
            chunks(
              bytes,
              offset,
              fn(rest, offset) {
                return(
                  b_data(append_bytearray(un_b_data(chunk), un_b_data(rest))),
                  offset,
                )
              },
            )
          },
        )
      },
    )
  }
}

fn items(
  decode: Decode,
  bytes: ByteArray,
  offset: Int,
  return: fn(List<Data>, Int) -> Data,
) -> Data {
  if index_bytearray(bytes, offset) == token_break {
    return([], offset + 1)
  } else {
    decode(
      bytes,
      offset,
      fn(x, offset) {
        items(
          decode,
          bytes,
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
  offset: Int,
  size: Int,
  return: fn(List<Data>, Int) -> Data,
) -> Data {
  if size <= 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      offset,
      fn(x, offset) {
        sized_items(
          decode,
          bytes,
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
  offset: Int,
  return: fn(Pairs<Data, Data>, Int) -> Data,
) -> Data {
  if index_bytearray(bytes, offset) == token_break {
    return([], offset + 1)
  } else {
    decode(
      bytes,
      offset,
      fn(k, offset) {
        decode(
          bytes,
          offset,
          fn(v, offset) {
            entries(
              decode,
              bytes,
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
  offset: Int,
  size: Int,
  return: fn(Pairs<Data, Data>, Int) -> Data,
) -> Data {
  if size <= 0 {
    return([], offset)
  } else {
    decode(
      bytes,
      offset,
      fn(k, offset) {
        decode(
          bytes,
          offset,
          fn(v, offset) {
            sized_entries(
              decode,
              bytes,
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
