//// A linear, resumable summariser over canonical Plutus Data bytes.
////
//// It reads one span left to right and yields the `DataSummaryV1` that
//// `cek_data_v1.semantic_data_summary_v1` gives the decoded value, or
//// `Unreadable` when the span is not in the image of `serialiseData`. The
//// grammar is the canonical one: minimal heads; integers in [-2^64, 2^64 - 1]
//// as major 0/1 and outside it as tag 2/3 over a magnitude of at least nine
//// bytes without a leading zero; byte strings definite up to 64 bytes and
//// indefinite above, in chunks of exactly 64 bytes and a final one of 1..64;
//// the empty list as `80` and every other list or constructor field list
//// indefinite; definite maps; constructor tags 121..127, 1280..1400 and 102
//// over a minimal index of at least 128; exactly one item ending at `end`.
////
//// A list's summary is a right fold over its items, so items are pushed on a
//// stack as they complete and folded, last first, when their container closes.
//// The stack lives in memory within one transaction. Between transactions only
//// its hash chain is kept (`Walk.stack_root`); the next transaction restores
//// the entries it will pop below its own pushes and the chain proves them.
////
//// Each step's outcome depends only on the walk and the span bytes. The
//// restored entries and the number of steps are witness choices: a wrong
//// restore fails, it never changes the outcome.

use aiken/builtin
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_v1.{
  DataSequenceSummaryV1, DataSummaryV1, IntegerData, hash_data_node_v1,
}

/// The container a walk is reading the items of.
pub type Frame {
  Root
  ListItems
  ConstrFields { constructor: Int }
  LargeConstrFields {
    index_root: ByteArray,
    index_cbor_length: Int,
    index_memory: Int,
  }
  MapEntries { remaining: Int }
}

/// A stack entry: a completed item, or the frame an open container suspended.
pub type Entry {
  Item { summary: DataSummaryV1 }
  Parent { frame: Frame }
}

/// The checkpoint a transaction leaves for the next one.
pub type Walk {
  offset: Int,
  frame: Frame,
  stack_root: ByteArray,
}

pub type Outcome {
  Walking { walk: Walk }
  Summarised { summary: DataSummaryV1 }
  Unreadable
}

/// The in-memory result of a run within one transaction. Provers use it to
/// learn the stack a checkpoint commits.
pub type Progress {
  Advanced { offset: Int, frame: Frame, stack: List<Entry> }
  Finished { summary: DataSummaryV1 }
  Refused
}

pub const empty_stack_root: ByteArray = #""

pub fn start(offset: Int) -> Walk {
  Walk { offset, frame: Root, stack_root: empty_stack_root }
}

fn u64(value: Int) -> ByteArray {
  bytearray.from_int_big_endian(value, 8)
}

fn encode_entry(entry: Entry) -> ByteArray {
  when entry is {
    Item { summary } -> {
      expect bytearray.length(summary.root) == 32
      #"00"
        |> bytearray.concat(summary.root)
        |> bytearray.concat(u64(summary.cbor_length))
        |> bytearray.concat(u64(summary.memory))
    }
    Parent { frame } ->
      when frame is {
        Root -> #"01"
        ListItems -> #"02"
        ConstrFields { constructor } ->
          #"03"
            |> bytearray.concat(bytearray.from_int_big_endian(constructor, 1))
        LargeConstrFields { index_root, index_cbor_length, index_memory } -> {
          expect bytearray.length(index_root) == 32
          #"04"
            |> bytearray.concat(index_root)
            |> bytearray.concat(u64(index_cbor_length))
            |> bytearray.concat(u64(index_memory))
        }
        MapEntries { remaining } -> #"05" |> bytearray.concat(u64(remaining))
      }
  }
}

/// The hash chain of `entries` (top first) above `base`. Every entry encoding
/// has a length fixed by its tag, and `base` is empty or a 32-byte root, so the
/// chain is injective.
pub fn chain(entries: List<Entry>, base: ByteArray) -> ByteArray {
  when entries is {
    [] -> base
    [entry, ..rest] ->
      blake2b_256(bytearray.concat(encode_entry(entry), chain(rest, base)))
  }
}

fn byte_at(bytes: ByteArray, at: Int, end: Int) -> Int {
  if at < end {
    builtin.index_bytearray(bytes, at)
  } else {
    -1
  }
}

fn argument(bytes: ByteArray, at: Int, size: Int, end: Int) -> Int {
  if at + size <= end {
    bytearray.to_int_big_endian(builtin.slice_bytearray(at, size, bytes))
  } else {
    -1
  }
}

/// A minimal CBOR head at `at`: (major, argument, next). An indefinite head
/// has argument -1.
fn head(bytes: ByteArray, at: Int, end: Int) -> Option<(Int, Int, Int)> {
  let first = byte_at(bytes, at, end)
  if first < 0 {
    None
  } else {
    let major = first / 32
    let info = first % 32
    if info < 24 {
      Some((major, info, at + 1))
    } else if info == 24 {
      let value = argument(bytes, at + 1, 1, end)
      if value >= 24 {
        Some((major, value, at + 2))
      } else {
        None
      }
    } else if info == 25 {
      let value = argument(bytes, at + 1, 2, end)
      if value >= 256 {
        Some((major, value, at + 3))
      } else {
        None
      }
    } else if info == 26 {
      let value = argument(bytes, at + 1, 4, end)
      if value >= 65536 {
        Some((major, value, at + 5))
      } else {
        None
      }
    } else if info == 27 {
      let value = argument(bytes, at + 1, 8, end)
      if value >= 4294967296 {
        Some((major, value, at + 9))
      } else {
        None
      }
    } else if info == 31 {
      Some((major, -1, at + 1))
    } else {
      None
    }
  }
}

fn chunks(
  bytes: ByteArray,
  at: Int,
  end: Int,
  content: ByteArray,
) -> Option<(ByteArray, Int)> {
  when head(bytes, at, end) is {
    Some((2, length, next)) ->
      if length >= 1 && length <= 64 && next + length <= end {
        let joined = bytearray.concat(
          content,
          builtin.slice_bytearray(next, length, bytes),
        )
        let after = next + length
        if byte_at(bytes, after, end) == 255 {
          Some((joined, after + 1))
        } else if length == 64 {
          chunks(bytes, after, end, joined)
        } else {
          None
        }
      } else {
        None
      }
    _ -> None
  }
}

/// A canonical byte string at `at`: its content and the offset after it.
fn byte_string(bytes: ByteArray, at: Int, end: Int) -> Option<(ByteArray, Int)> {
  when head(bytes, at, end) is {
    Some((2, -1, next)) ->
      when chunks(bytes, next, end, #"") is {
        Some((content, after)) ->
          if bytearray.length(content) > 64 {
            Some((content, after))
          } else {
            None
          }
        None -> None
      }
    Some((2, length, next)) ->
      if length <= 64 && next + length <= end {
        Some((builtin.slice_bytearray(next, length, bytes), next + length))
      } else {
        None
      }
    _ -> None
  }
}

fn unsigned_byte_size(value: Int) -> Int {
  if value < 256 {
    1
  } else {
    1 + unsigned_byte_size(value / 256)
  }
}

/// `4 + integer_memory_size` of an integer whose magnitude is `magnitude` for
/// a non-negative value and `magnitude + 1` for a negative one.
fn integer_memory(magnitude: Int) -> Int {
  4 + unsigned_byte_size(magnitude * 2)
}

fn integer_summary(
  bytes: ByteArray,
  at: Int,
  after: Int,
  magnitude: Int,
) -> DataSummaryV1 {
  let span = builtin.slice_bytearray(at, after - at, bytes)
  let memory = integer_memory(magnitude)
  DataSummaryV1 {
    root: hash_data_node_v1(
      IntegerData {
        cbor_root: cek_data_v1.bounded_blob_root_v1(span),
        cbor_length: after - at,
        memory,
      },
    ),
    cbor_length: after - at,
    memory,
  }
}

fn empty_fields_summary(frame: Frame) -> DataSummaryV1 {
  let empty = cek_data_v1.empty_data_list_summary_v1()
  when frame is {
    ConstrFields { constructor } ->
      cek_data_v1.small_constr_data_summary_v1(constructor, empty)
    LargeConstrFields { index_root, index_cbor_length, index_memory } ->
      cek_data_v1.large_constr_data_summary_from_cbor_v1(
        index_root,
        index_cbor_length,
        index_memory,
        empty,
      )
    _ -> fail
  }
}

fn pop_items(
  stack: List<Entry>,
  folded: DataSequenceSummaryV1,
) -> (DataSequenceSummaryV1, Frame, List<Entry>) {
  when stack is {
    [Item { summary }, ..rest] ->
      pop_items(rest, cek_data_v1.prepend_data_list_summary_v1(summary, folded))
    [Parent { frame }, ..rest] -> (folded, frame, rest)
    [] -> fail @"restore more of the stack"
  }
}

fn pop_pairs(
  stack: List<Entry>,
  folded: DataSequenceSummaryV1,
) -> (DataSequenceSummaryV1, Frame, List<Entry>) {
  when stack is {
    [Item { summary: value }, ..rest] ->
      when rest is {
        [Item { summary: key }, ..below] ->
          pop_pairs(
            below,
            cek_data_v1.prepend_data_pair_summary_v1(key, value, folded),
          )
        _ -> fail @"restore more of the stack"
      }
    [Parent { frame }, ..rest] -> (folded, frame, rest)
    [] -> fail @"restore more of the stack"
  }
}

/// `summary` is a completed item at `after`; hand it to `frame`.
fn complete(
  summary: DataSummaryV1,
  after: Int,
  frame: Frame,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  when frame is {
    Root ->
      if after == end {
        Finished(summary)
      } else {
        Refused
      }
    MapEntries { remaining } ->
      if remaining == 1 {
        let (entries, parent, rest) = pop_pairs(
          [Item(summary), ..stack],
          cek_data_v1.empty_data_pair_summary_v1(),
        )
        complete(
          cek_data_v1.map_data_summary_v1(entries),
          after,
          parent,
          rest,
          end,
        )
      } else {
        Advanced(
          after,
          MapEntries { remaining: remaining - 1 },
          [Item(summary), ..stack],
        )
      }
    _ -> Advanced(after, frame, [Item(summary), ..stack])
  }
}

fn close(frame: Frame, stack: List<Entry>, after: Int, end: Int) -> Progress {
  when stack is {
    // `9fff`: an indefinite list or field list is never empty.
    [Parent { .. }, ..] -> Refused
    _ ->
      when frame is {
        ListItems -> {
          let (items, parent, rest) = pop_items(
            stack,
            cek_data_v1.empty_data_list_summary_v1(),
          )
          complete(
            cek_data_v1.list_data_summary_v1(items),
            after,
            parent,
            rest,
            end,
          )
        }
        ConstrFields { constructor } -> {
          let (fields, parent, rest) = pop_items(
            stack,
            cek_data_v1.empty_data_list_summary_v1(),
          )
          complete(
            cek_data_v1.small_constr_data_summary_v1(constructor, fields),
            after,
            parent,
            rest,
            end,
          )
        }
        LargeConstrFields { index_root, index_cbor_length, index_memory } -> {
          let (fields, parent, rest) = pop_items(
            stack,
            cek_data_v1.empty_data_list_summary_v1(),
          )
          complete(
            cek_data_v1.large_constr_data_summary_from_cbor_v1(
              index_root,
              index_cbor_length,
              index_memory,
              fields,
            ),
            after,
            parent,
            rest,
            end,
          )
        }
        _ -> Refused
      }
  }
}

/// The field list of a constructor whose fields `opened` will read.
fn fields(
  bytes: ByteArray,
  at: Int,
  opened: Frame,
  frame: Frame,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  let first = byte_at(bytes, at, end)
  if first == 128 {
    complete(empty_fields_summary(opened), at + 1, frame, stack, end)
  } else if first == 159 {
    Advanced(at + 1, opened, [Parent(frame), ..stack])
  } else {
    Refused
  }
}

fn tagged(
  bytes: ByteArray,
  at: Int,
  next: Int,
  tag: Int,
  frame: Frame,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  if tag >= 121 && tag <= 127 {
    fields(
      bytes,
      next,
      ConstrFields { constructor: tag - 121 },
      frame,
      stack,
      end,
    )
  } else if tag >= 1280 && tag <= 1400 {
    fields(
      bytes,
      next,
      ConstrFields { constructor: tag - 1273 },
      frame,
      stack,
      end,
    )
  } else if tag == 102 {
    if byte_at(bytes, next, end) == 130 {
      when head(bytes, next + 1, end) is {
        Some((0, index, after)) ->
          if index >= 128 {
            let index_cbor = builtin.slice_bytearray(
              next + 1,
              after - next - 1,
              bytes,
            )
            fields(
              bytes,
              after,
              LargeConstrFields {
                index_root: cek_data_v1.bounded_blob_root_v1(index_cbor),
                index_cbor_length: after - next - 1,
                index_memory: integer_memory(index),
              },
              frame,
              stack,
              end,
            )
          } else {
            Refused
          }
        _ -> Refused
      }
    } else {
      Refused
    }
  } else if tag == 2 || tag == 3 {
    when byte_string(bytes, next, end) is {
      Some((magnitude, after)) ->
        if bytearray.length(magnitude) >= 9 && builtin.index_bytearray(
          magnitude,
          0,
        ) != 0 {
          complete(
            integer_summary(
              bytes,
              at,
              after,
              bytearray.to_int_big_endian(magnitude),
            ),
            after,
            frame,
            stack,
            end,
          )
        } else {
          Refused
        }
      None -> Refused
    }
  } else {
    Refused
  }
}

fn step(
  bytes: ByteArray,
  at: Int,
  frame: Frame,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  if byte_at(bytes, at, end) == 255 {
    close(frame, stack, at + 1, end)
  } else {
    when head(bytes, at, end) is {
      None -> Refused
      Some((major, value, next)) ->
        if major <= 1 {
          if value < 0 {
            Refused
          } else {
            complete(
              integer_summary(bytes, at, next, value),
              next,
              frame,
              stack,
              end,
            )
          }
        } else if major == 2 {
          when byte_string(bytes, at, end) is {
            Some((content, after)) ->
              complete(
                cek_data_v1.bytes_data_summary_v1(
                  bytearray.length(content),
                  cek_data_v1.bounded_blob_root_v1(content),
                ),
                after,
                frame,
                stack,
                end,
              )
            None -> Refused
          }
        } else if major == 4 {
          if value == -1 {
            Advanced(next, ListItems, [Parent(frame), ..stack])
          } else if value == 0 {
            complete(
              cek_data_v1.list_data_summary_v1(
                cek_data_v1.empty_data_list_summary_v1(),
              ),
              next,
              frame,
              stack,
              end,
            )
          } else {
            Refused
          }
        } else if major == 5 {
          if value == 0 {
            complete(
              cek_data_v1.map_data_summary_v1(
                cek_data_v1.empty_data_pair_summary_v1(),
              ),
              next,
              frame,
              stack,
              end,
            )
          } else if value > 0 && 2 * value <= end - next {
            Advanced(
              next,
              MapEntries { remaining: 2 * value },
              [Parent(frame), ..stack],
            )
          } else {
            Refused
          }
        } else if major == 6 {
          tagged(bytes, at, next, value, frame, stack, end)
        } else {
          Refused
        }
    }
  }
}

pub fn run(
  bytes: ByteArray,
  at: Int,
  frame: Frame,
  stack: List<Entry>,
  end: Int,
  steps: Int,
) -> Progress {
  if steps == 0 {
    Advanced(at, frame, stack)
  } else {
    when step(bytes, at, frame, stack, end) is {
      Advanced(next, next_frame, next_stack) ->
        run(bytes, next, next_frame, next_stack, end, steps - 1)
      other -> other
    }
  }
}

/// Advance `walk` over `bytes[walk.offset, end)` by up to `steps` items, heads
/// or closes. `restore` (top first) and `base` must chain to the walk's stack
/// root; they are the entries this run pops below its own pushes.
pub fn resume(
  walk: Walk,
  bytes: ByteArray,
  end: Int,
  restore: List<Entry>,
  base: ByteArray,
  steps: Int,
) -> Outcome {
  expect steps > 0
  expect chain(restore, base) == walk.stack_root
  when run(bytes, walk.offset, walk.frame, restore, end, steps) is {
    Advanced(offset, frame, stack) ->
      Walking(Walk { offset, frame, stack_root: chain(stack, base) })
    Finished(summary) -> Summarised(summary)
    Refused -> Unreadable
  }
}
