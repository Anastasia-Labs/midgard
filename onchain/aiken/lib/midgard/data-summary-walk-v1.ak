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
//// stack as they complete and folded, last first, after their container
//// closes: the `Folding` frame pops one item (or map entry) per step until it
//// reaches the suspended parent, so no step's cost grows with a container's
//// width. The stack lives in memory within one transaction. Between transactions only
//// its hash chain is kept (`Walk.stack_root`); the next transaction restores
//// the entries it will pop below its own pushes and the chain proves them.
////
//// Each step's outcome depends only on the walk and the span bytes. The
//// restored entries and the number of steps are witness choices: a wrong
//// restore fails, it never changes the outcome.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1.{DataSequenceSummaryV1, DataSummaryV1}

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
  /// `open` has closed; its items are being popped into `folded`, last first.
  Folding { open: Frame, folded: DataSequenceSummaryV1 }
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

/// The hash chain of `entries` (top first) above `base`, which is empty or a
/// 32-byte root. `serialiseData` encodings are self-delimiting, so the chain
/// is injective.
pub fn chain(entries: List<Entry>, base: ByteArray) -> ByteArray {
  when entries is {
    [] -> base
    [entry, ..rest] -> {
      let encoded: Data = entry
      builtin.blake2b_256(
        builtin.append_bytearray(
          builtin.serialise_data(encoded),
          chain(rest, base),
        ),
      )
    }
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
    } else if info <= 27 {
      // A 1-, 2-, 4- or 8-byte argument is minimal when it is at least 24 and
      // needs more than half of its width.
      let size = builtin.index_bytearray(#"01020408", info - 24)
      let value = argument(bytes, at + 1, size, end)
      if value >= 24 && 2 * bytearray.length(
        bytearray.from_int_big_endian(value, 0),
      ) > size {
        Some((major, value, at + 1 + size))
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
  // A full chunk followed by more input: the head `58 40` and 64 bytes.
  if at + 66 < end && builtin.slice_bytearray(at, 2, bytes) == #"5840" {
    let joined = bytearray.concat(
      content,
      builtin.slice_bytearray(at + 2, 64, bytes),
    )
    let after = at + 66
    if builtin.index_bytearray(bytes, after) == 255 {
      Some((joined, after + 1))
    } else {
      chunks(bytes, after, end, joined)
    }
  } else {
    last_chunk(bytes, at, end, content)
  }
}

/// The last chunk of a byte string, of 1 to 64 bytes, and its break.
fn last_chunk(
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
        // A full chunk reaches here only when it ends the input (`chunks`
        // takes every other), so it needs no continuation.
        if byte_at(bytes, after, end) == 255 {
          Some((joined, after + 1))
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

/// `4 +` the word size of a tag 2/3 integer from its magnitude bytes (without
/// a leading zero): tag 2 holds `|n|` and tag 3 holds `|n| - 1`.
fn magnitude_memory(tag: Int, magnitude: ByteArray) -> Int {
  4 + if tag == 2 {
    cek_data_v1.bytes_memory_size_v1(builtin.length_of_bytearray(magnitude))
  } else {
    cek_data_v1.integer_memory_size_v1(
      builtin.bytearray_to_integer(True, magnitude) + 1,
    )
  }
}

/// `4 +` the word size of a major 0/1 integer with CBOR argument `argument`:
/// `|n|` is the argument for major 0 and one more for major 1.
fn integer_memory(major: Int, argument: Int) -> Int {
  4 + cek_data_v1.integer_memory_size_v1(argument + major)
}

// ## Node summaries
//
// The `cek_data_v1` summary formulas, hashing each node preimage with one
// `serialiseData`. A preimage is a definite array of unsigned integers and
// 32-byte roots, and `serialiseData` of the same items as a list is the
// indefinite array `9f <items> ff`, so the preimage is the definite header
// followed by those item bytes. Every count, length and memory below is
// derived from a span of at most one bounded ledger output and every root is
// a blake2b_256 digest, so the encoders' uint32/uint64/hash-width checks hold
// by construction.

fn node_root(domain: ByteArray, fields: List<Data>, count: Int) -> ByteArray {
  let items = builtin.serialise_data(builtin.list_data(fields))
  builtin.blake2b_256(
    builtin.append_bytearray(
      domain,
      builtin.cons_bytearray(
        128 + count,
        builtin.slice_bytearray(
          1,
          builtin.length_of_bytearray(items) - 2,
          items,
        ),
      ),
    ),
  )
}

fn data_node(
  fields: List<Data>,
  count: Int,
  cbor_length: Int,
  memory: Int,
) -> DataSummaryV1 {
  DataSummaryV1 {
    root: node_root(cek_data_v1.data_node_domain, fields, count),
    cbor_length,
    memory,
  }
}

fn integer_leaf(cbor: ByteArray, memory: Int) -> DataSummaryV1 {
  let cbor_length = builtin.length_of_bytearray(cbor)
  data_node(
    [
      builtin.i_data(4),
      builtin.b_data(cek_data_v1.bounded_blob_root_v1(cbor)),
      builtin.i_data(cbor_length),
      builtin.i_data(memory),
    ],
    4,
    cbor_length,
    memory,
  )
}

fn bytes_leaf(content: ByteArray, cbor_length: Int) -> DataSummaryV1 {
  let length = builtin.length_of_bytearray(content)
  let memory = 4 + cek_data_v1.bytes_memory_size_v1(length)
  data_node(
    [
      builtin.i_data(5),
      builtin.b_data(cek_data_v1.bounded_blob_root_v1(content)),
      builtin.i_data(length),
      builtin.i_data(cbor_length),
      builtin.i_data(memory),
    ],
    5,
    cbor_length,
    memory,
  )
}

fn list_cbor_length(items: DataSequenceSummaryV1) -> Int {
  if items.length == 0 {
    1
  } else {
    2 + items.payload_cbor_length
  }
}

fn map_cbor_length(entries: DataSequenceSummaryV1) -> Int {
  let count = entries.length
  entries.payload_cbor_length + if count < 24 {
    1
  } else if count <= 255 {
    2
  } else if count <= 65535 {
    3
  } else {
    5
  }
}

/// A list, map or constructor node: `prefix` (its tag and, for a
/// constructor, the alternative) followed by its items' sequence summary.
fn composite(
  prefix: List<Data>,
  count: Int,
  items: DataSequenceSummaryV1,
  cbor_length: Int,
) -> DataSummaryV1 {
  let memory = 4 + items.memory
  data_node(
    list.concat(
      prefix,
      [
        builtin.i_data(items.length),
        builtin.b_data(items.root),
        builtin.i_data(cbor_length),
        builtin.i_data(memory),
      ],
    ),
    count,
    cbor_length,
    memory,
  )
}

/// A list or map node over `heads`, the summaries of one item or one
/// key/value pair, whose CBOR lengths and memories sum to `cbor_length` and
/// `memory`.
fn prepend(
  domain: ByteArray,
  count: Int,
  heads: List<Data>,
  cbor_length: Int,
  memory: Int,
  tail: DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  let length = tail.length + 1
  let payload_cbor_length = cbor_length + tail.payload_cbor_length
  let memory = memory + tail.memory
  DataSequenceSummaryV1 {
    root: node_root(
      domain,
      list.concat(
        heads,
        [
          builtin.b_data(tail.root),
          builtin.i_data(length),
          builtin.i_data(payload_cbor_length),
          builtin.i_data(memory),
        ],
      ),
      count,
    ),
    length,
    payload_cbor_length,
    memory,
  }
}

fn summary_fields(summary: DataSummaryV1) -> List<Data> {
  [
    builtin.b_data(summary.root),
    builtin.i_data(summary.cbor_length),
    builtin.i_data(summary.memory),
  ]
}

fn prepend_item(
  head: DataSummaryV1,
  tail: DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  prepend(
    cek_data_v1.data_list_node_domain,
    7,
    summary_fields(head),
    head.cbor_length,
    head.memory,
    tail,
  )
}

fn prepend_pair(
  key: DataSummaryV1,
  value: DataSummaryV1,
  tail: DataSequenceSummaryV1,
) -> DataSequenceSummaryV1 {
  prepend(
    cek_data_v1.data_pair_node_domain,
    10,
    list.concat(summary_fields(key), summary_fields(value)),
    key.cbor_length + value.cbor_length,
    key.memory + value.memory,
    tail,
  )
}

const no_items: DataSequenceSummaryV1 = cek_data_v1.empty_data_list_summary_v1()

const no_entries: DataSequenceSummaryV1 =
  cek_data_v1.empty_data_pair_summary_v1()

const empty_map: DataSummaryV1 = cek_data_v1.map_data_summary_v1(no_entries)

/// The summary of the closed container `open` over its folded items.
fn finish(open: Frame, folded: DataSequenceSummaryV1) -> DataSummaryV1 {
  when open is {
    ListItems ->
      composite([builtin.i_data(3)], 5, folded, list_cbor_length(folded))
    ConstrFields { constructor } ->
      composite(
        [builtin.i_data(0), builtin.i_data(constructor)],
        6,
        folded,
        list_cbor_length(folded) + if constructor <= 6 {
          2
        } else {
          3
        },
      )
    LargeConstrFields { index_root, index_cbor_length, index_memory } ->
      composite(
        [
          builtin.i_data(1),
          builtin.b_data(index_root),
          builtin.i_data(index_cbor_length),
          builtin.i_data(index_memory),
        ],
        8,
        folded,
        3 + index_cbor_length + list_cbor_length(folded),
      )
    MapEntries { .. } ->
      composite([builtin.i_data(2)], 5, folded, map_cbor_length(folded))
    _ -> fail
  }
}

/// One step of folding the closed container `open`: pop one item (or one
/// key/value pair), or reach the suspended parent and complete the container.
fn fold(
  open: Frame,
  folded: DataSequenceSummaryV1,
  at: Int,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  when stack is {
    [Parent { frame }, ..rest] ->
      // `9fff`: an indefinite list or field list is never empty. A map is
      // folded only after its last entry, so it never reaches here empty.
      if folded.length == 0 {
        Refused
      } else {
        complete(finish(open, folded), at, frame, rest, end)
      }
    [Item { summary }, ..rest] ->
      when open is {
        MapEntries { .. } ->
          // A map pushes its keys and values in pairs.
          when rest is {
            [Item { summary: key }, ..below] ->
              Advanced(
                at,
                Folding { open, folded: prepend_pair(key, summary, folded) },
                below,
              )
            _ -> fail @"restore more of the stack"
          }
        _ ->
          Advanced(
            at,
            Folding { open, folded: prepend_item(summary, folded) },
            rest,
          )
      }
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
        Advanced(
          after,
          Folding { open: MapEntries { remaining: 0 }, folded: no_entries },
          [Item(summary), ..stack],
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

fn close(frame: Frame, stack: List<Entry>, after: Int) -> Progress {
  when frame is {
    ListItems | ConstrFields { .. } | LargeConstrFields { .. } ->
      Advanced(after, Folding { open: frame, folded: no_items }, stack)
    _ -> Refused
  }
}

/// A list, or the field list of a constructor, whose items `opened` will read.
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
    complete(finish(opened, no_items), at + 1, frame, stack, end)
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
  if tag >= 121 && tag <= 127 || tag >= 1280 && tag <= 1400 {
    fields(
      bytes,
      next,
      ConstrFields {
        constructor: if tag <= 127 {
          tag - 121
        } else {
          tag - 1273
        },
      },
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
                index_memory: integer_memory(0, index),
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
            integer_leaf(
              builtin.slice_bytearray(at, after - at, bytes),
              magnitude_memory(tag, magnitude),
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
  when frame is {
    Folding { open, folded } -> fold(open, folded, at, stack, end)
    _ -> read(bytes, at, frame, stack, end)
  }
}

fn read(
  bytes: ByteArray,
  at: Int,
  frame: Frame,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  if byte_at(bytes, at, end) == 255 {
    close(frame, stack, at + 1)
  } else {
    when head(bytes, at, end) is {
      None -> Refused
      Some((major, value, next)) ->
        if major <= 1 {
          if value < 0 {
            Refused
          } else {
            complete(
              integer_leaf(
                builtin.slice_bytearray(at, next - at, bytes),
                integer_memory(major, value),
              ),
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
                bytes_leaf(content, after - at),
                after,
                frame,
                stack,
                end,
              )
            None -> Refused
          }
        } else if major == 4 {
          fields(bytes, at, ListItems, frame, stack, end)
        } else if major == 5 {
          if value == 0 {
            complete(empty_map, next, frame, stack, end)
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
  expect chain(restore, base) == walk.stack_root
  when run(bytes, walk.offset, walk.frame, restore, end, steps) is {
    Advanced(offset, frame, stack) ->
      Walking(Walk { offset, frame, stack_root: chain(stack, base) })
    Finished(summary) -> Summarised(summary)
    Refused -> Unreadable
  }
}
