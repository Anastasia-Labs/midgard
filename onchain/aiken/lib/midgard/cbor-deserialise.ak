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
  append_bytearray, b_data, bytearray_to_integer, constr_data, i_data,
  index_bytearray, length_of_bytearray, list_data, map_data, slice_bytearray,
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
    decode_data(
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

fn decode_data(
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
    let major_type = next / 32
    if major_type <= 2 {
      if major_type == 0 {
        decode_uint(
          bytes,
          length,
          offset,
          next,
          fn(i, offset) { return(i_data(i), offset) },
        )
      } else if major_type == 1 {
        decode_uint(
          bytes,
          length,
          offset,
          next - 32,
          fn(i, offset) { return(i_data(-i - 1), offset) },
        )
      } else if next == token_begin_bytes {
        decode_chunks(
          bytes,
          length,
          offset,
          fn(b, offset) { return(b_data(b), offset) },
        )
      } else {
        decode_bytes(
          bytes,
          length,
          offset,
          next - 64,
          fn(b, offset) { return(b_data(b), offset) },
        )
      }
    } else if major_type == 6 {
      decode_uint(
        bytes,
        length,
        offset,
        next - 192,
        fn(tag, offset) {
          if offset >= length {
            None
          } else {
            let next = index_bytearray(bytes, offset)
            let offset = offset + 1
            if tag == 102 {
              None
            } else {
              let ix = if tag >= 1280 {
                tag - 1280 + 7
              } else {
                tag - 121
              }
              if next == token_begin_list {
                decode_indefinite_list(
                  bytes,
                  length,
                  offset,
                  fn(fields, offset) { return(constr_data(ix, fields), offset) },
                )
              } else {
                decode_uint(
                  bytes,
                  length,
                  offset,
                  next - 128,
                  fn(size, offset) {
                    decode_definite_list(
                      bytes,
                      length,
                      offset,
                      size,
                      fn(fields, offset) {
                        return(constr_data(ix, fields), offset)
                      },
                    )
                  },
                )
              }
            }
          }
        },
      )
    } else if major_type == 4 {
      if next == token_begin_list {
        decode_indefinite_list(
          bytes,
          length,
          offset,
          fn(xs, offset) { return(list_data(xs), offset) },
        )
      } else {
        decode_uint(
          bytes,
          length,
          offset,
          next - 128,
          fn(size, offset) {
            decode_definite_list(
              bytes,
              length,
              offset,
              size,
              fn(xs, offset) { return(list_data(xs), offset) },
            )
          },
        )
      }
    } else if major_type == 5 {
      if next == token_begin_map {
        decode_indefinite_map(
          bytes,
          length,
          offset,
          fn(xs, offset) { return(map_data(xs), offset) },
        )
      } else {
        decode_uint(
          bytes,
          length,
          offset,
          next - 160,
          fn(size, offset) {
            decode_definite_map(
              bytes,
              length,
              offset,
              size,
              fn(xs, offset) { return(map_data(xs), offset) },
            )
          },
        )
      }
    } else {
      None
    }
  }
}

fn decode_uint(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  header: Int,
  return: fn(Int, Int) -> Option<Data>,
) -> Option<Data> {
  if header < 24 {
    return(header, offset)
  } else if header == 24 {
    if offset >= length {
      None
    } else {
      return(index_bytearray(bytes, offset), offset + 1)
    }
  } else if header < 28 {
    let width = if header == 25 {
      2
    } else if header == 26 {
      4
    } else {
      8
    }
    if offset >= length {
      None
    } else {
      return(
        bytearray_to_integer(True, slice_bytearray(offset, width, bytes)),
        offset + width,
      )
    }
  } else {
    None
  }
}

fn decode_bytes(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  header: Int,
  return: fn(ByteArray, Int) -> Option<Data>,
) -> Option<Data> {
  decode_uint(
    bytes,
    length,
    offset,
    header,
    fn(width, offset) {
      if offset >= length {
        None
      } else {
        return(slice_bytearray(offset, width, bytes), offset + width)
      }
    },
  )
}

fn decode_chunks(
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
      decode_bytes(
        bytes,
        length,
        offset,
        next - 64,
        fn(chunk, offset) {
          decode_chunks(
            bytes,
            length,
            offset,
            fn(chunks, offset) {
              return(append_bytearray(chunk, chunks), offset)
            },
          )
        },
      )
    }
  }
}

fn decode_definite_list(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  size: Int,
  return: fn(List<Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if size <= 0 {
    return([], offset)
  } else {
    decode_data(
      bytes,
      length,
      offset,
      fn(elem, offset) {
        decode_definite_list(
          bytes,
          length,
          offset,
          size - 1,
          fn(elems, offset) { return([elem, ..elems], offset) },
        )
      },
    )
  }
}

fn decode_indefinite_list(
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
    decode_data(
      bytes,
      length,
      offset,
      fn(elem, offset) {
        decode_indefinite_list(
          bytes,
          length,
          offset,
          fn(elems, offset) { return([elem, ..elems], offset) },
        )
      },
    )
  }
}

fn decode_pair(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  return: fn(Pair<Data, Data>, Int) -> Option<Data>,
) -> Option<Data> {
  decode_data(
    bytes,
    length,
    offset,
    fn(k, offset) {
      decode_data(
        bytes,
        length,
        offset,
        fn(v, offset) { return(Pair(k, v), offset) },
      )
    },
  )
}

fn decode_definite_map(
  bytes: ByteArray,
  length: Int,
  offset: Int,
  size: Int,
  return: fn(Pairs<Data, Data>, Int) -> Option<Data>,
) -> Option<Data> {
  if size <= 0 {
    return([], offset)
  } else {
    decode_pair(
      bytes,
      length,
      offset,
      fn(elem, offset) {
        decode_definite_map(
          bytes,
          length,
          offset,
          size - 1,
          fn(elems, offset) { return([elem, ..elems], offset) },
        )
      },
    )
  }
}

fn decode_indefinite_map(
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
    decode_pair(
      bytes,
      length,
      offset,
      fn(elem, offset) {
        decode_indefinite_map(
          bytes,
          length,
          offset,
          fn(elems, offset) { return([elem, ..elems], offset) },
        )
      },
    )
  }
}
