//// A resumable reader over the canonical CBOR of a Plutus Data value that is
//// read through a sequence of windows, one per transaction, rather than held
//// whole.
////
//// It accepts exactly what `serialiseData` produces and yields the
//// `DataSummaryV1` that `cek_data_v1.semantic_data_summary_v1` gives the
//// decoded value. The grammar is `data_summary_walk_v1`'s: minimal heads;
//// integers in [-2^64, 2^64 - 1] as major 0/1 and outside it as tag 2/3 over a
//// magnitude of at least nine bytes without a leading zero; byte strings
//// definite up to 64 bytes and indefinite above, in chunks of exactly 64 bytes
//// and a final one of 1..64; the empty list as `80` and every other list or
//// constructor field list indefinite; definite maps; constructor tags
//// 121..127, 1280..1400 and 102 over a minimal index of at least 128; exactly
//// one item, ending where the bytes end.
////
//// Two things differ from `data_summary_walk_v1`, both because a window holds
//// only part of the value:
////
//// - A map's entry count is not checked against the bytes left in the window
////   (a map whose entries never arrive never completes).
//// - An indefinite byte string, and the indefinite magnitude of a tag 2/3
////   integer, is read by a `Leaf` frame over as many steps and windows as it
////   needs. Each step reads chunks until it holds one more 4,095-byte chunk of
////   the leaf's committed bytes (the content of a byte string, the whole CBOR
////   of an integer), hashes that chunk into the leaf's own flat-tree frontier
////   and keeps the rest pending. Every other item is read whole within one
////   step.
////
//// A step needs its bytes inside the window: a step that would read past the
//// window's end is refused, except that a `Leaf` step stops before a chunk
//// the window does not hold whole (and is refused if it read nothing). The
//// steps a hop runs are a witness choice, so the window split can only make a
//// hop fail; every step's outcome depends only on the frame, the stack and
//// the bytes it reads.
////
//// Leaf and value roots are flat-tree roots (`cek_staged_blob_v1`), which are
//// byte-identical to `cek_data_v1.bounded_blob_root_v1` wherever that is
//// defined.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1.{DataSequenceSummaryV1, DataSummaryV1}
use midgard/cek_staged_blob_v1.{
  branch_root_v1, chunk_bytes, chunk_root_v1, concat_balanced_v1,
}

// ## Flat-tree frontier

/// A complete subtree of the flat tree: its height, root and byte length.
pub type PeakV1 {
  height: Int,
  root: ByteArray,
  length: Int,
}

/// The chunks hashed so far, as the peaks of their flat tree, smallest first.
/// Every chunk but the last one appended is 4,095 bytes.
pub type FrontierV1 {
  count: Int,
  length: Int,
  peaks: List<PeakV1>,
}

pub const empty_frontier: FrontierV1 =
  FrontierV1 { count: 0, length: 0, peaks: [] }

fn merge(peaks: List<PeakV1>, node: PeakV1) -> List<PeakV1> {
  when peaks is {
    [left, ..rest] ->
      if left.height == node.height {
        let length = left.length + node.length
        merge(
          rest,
          PeakV1 {
            height: node.height + 1,
            root: branch_root_v1(left.root, node.root, length),
            length,
          },
        )
      } else {
        [node, ..peaks]
      }
    [] -> [node]
  }
}

/// Appends one chunk of 1 to 4,095 bytes. Every chunk before it must be full.
pub fn append_chunk(frontier: FrontierV1, chunk: ByteArray) -> FrontierV1 {
  let size = builtin.length_of_bytearray(chunk)
  expect size > 0
  expect frontier.length == frontier.count * chunk_bytes
  FrontierV1 {
    count: frontier.count + 1,
    length: frontier.length + size,
    peaks: merge(
      frontier.peaks,
      PeakV1 { height: 0, root: chunk_root_v1(chunk), length: size },
    ),
  }
}

/// Appends chunks in order.
pub fn append_chunks(
  frontier: FrontierV1,
  chunks: List<ByteArray>,
) -> FrontierV1 {
  when chunks is {
    [] -> frontier
    [chunk, ..rest] -> append_chunks(append_chunk(frontier, chunk), rest)
  }
}

fn aggregate(right: PeakV1, peaks: List<PeakV1>) -> ByteArray {
  when peaks is {
    [] -> right.root
    [left, ..rest] -> {
      let length = left.length + right.length
      aggregate(
        PeakV1 {
          height: left.height,
          root: branch_root_v1(left.root, right.root, length),
          length,
        },
        rest,
      )
    }
  }
}

/// The flat root of the appended chunks' concatenation. The left subtree of
/// the flat tree holds the largest power-of-two chunk count strictly below the
/// total, so folding the peaks from the smallest up gives its root.
pub fn frontier_root(frontier: FrontierV1) -> ByteArray {
  expect [smallest, ..rest] = frontier.peaks
  aggregate(smallest, rest)
}

// ## Frames and steps

/// The container a walk is reading the items of, or a leaf it is streaming.
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
  /// An indefinite byte string (`tag` 0) or the magnitude of a tag 2/3
  /// integer (`tag` 2 or 3) inside `parent`, read up to here. `leaf` holds the
  /// full chunks of its committed bytes and `pending` the rest: the content of
  /// a byte string, or the whole CBOR of an integer from its tag byte.
  /// `length` counts content bytes and `cbor_length` CBOR bytes, and `all_ff`
  /// is whether every content byte so far is 0xff.
  Leaf {
    parent: Frame,
    tag: Int,
    leaf: FrontierV1,
    pending: ByteArray,
    length: Int,
    cbor_length: Int,
    all_ff: Bool,
  }
}

/// A stack entry: a completed item, or the frame an open container suspended.
pub type Entry {
  Item { summary: DataSummaryV1 }
  Parent { frame: Frame }
}

/// The in-memory result of a run within one window.
pub type Progress {
  Advanced { offset: Int, frame: Frame, stack: List<Entry> }
  Finished { summary: DataSummaryV1 }
  Refused
}

pub const empty_stack_root: ByteArray = #""

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

/// A definite byte string of at most 64 bytes at `at`: its content and the
/// offset after it.
fn short_bytes(bytes: ByteArray, at: Int, end: Int) -> Option<(ByteArray, Int)> {
  when head(bytes, at, end) is {
    Some((2, length, next)) ->
      if length >= 0 && length <= 64 && next + length <= end {
        Some((builtin.slice_bytearray(next, length, bytes), next + length))
      } else {
        None
      }
    _ -> None
  }
}

// ## Node summaries
//
// The `cek_data_v1` summary formulas, hashing each node preimage with one
// `serialiseData`, exactly as `data_summary_walk_v1` does.

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

/// The summary of an integer whose CBOR has flat root `root`.
fn integer_leaf(root: ByteArray, cbor_length: Int, memory: Int) -> DataSummaryV1 {
  data_node(
    [
      builtin.i_data(4),
      builtin.b_data(root),
      builtin.i_data(cbor_length),
      builtin.i_data(memory),
    ],
    4,
    cbor_length,
    memory,
  )
}

/// The summary of a byte string of `length` bytes whose content has flat root
/// `root`, as `cek_data_v1.bytes_data_summary_v1` gives it.
pub fn bytes_leaf(
  root: ByteArray,
  length: Int,
  cbor_length: Int,
) -> DataSummaryV1 {
  let memory = 4 + cek_data_v1.bytes_memory_size_v1(length)
  data_node(
    [
      builtin.i_data(5),
      builtin.b_data(root),
      builtin.i_data(length),
      builtin.i_data(cbor_length),
      builtin.i_data(memory),
    ],
    5,
    cbor_length,
    memory,
  )
}

/// `4 +` the word size of a major 0/1 integer with CBOR argument `argument`:
/// `|n|` is the argument for major 0 and one more for major 1.
fn integer_memory(major: Int, argument: Int) -> Int {
  4 + cek_data_v1.integer_memory_size_v1(argument + major)
}

/// `4 +` the word size of a tag 2/3 integer whose magnitude has `length`
/// bytes without a leading zero: tag 2 holds `|n|` and tag 3 holds `|n| - 1`,
/// which needs one more byte than the magnitude only when every magnitude
/// byte is 0xff.
fn magnitude_memory(tag: Int, length: Int, all_ff: Bool) -> Int {
  4 + ( length + 7 ) / 8 + if tag == 3 && all_ff && length % 8 == 0 {
    1
  } else {
    0
  }
}

/// 64 bytes of 0xff.
const all_ones =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

fn is_all_ones(piece: ByteArray) -> Bool {
  piece == builtin.slice_bytearray(
    0,
    builtin.length_of_bytearray(piece),
    all_ones,
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

// ## Streaming leaves

/// How far one leaf step reads: the offset after the last chunk it takes (or
/// after the break), the content bytes it takes, and whether it read the
/// break.
type Reach {
  after: Int,
  got: Int,
  done: Bool,
}

/// Reads chunks from `at` while the leaf's committed bytes (`held` before the
/// first, counting content for a byte string and CBOR for an integer from
/// `start`) stay below one chunk. Stops before a chunk the window does not
/// hold whole, with a short final chunk counted whole only with its break.
/// `None` is a malformed chunk.
fn reach(
  bytes: ByteArray,
  at: Int,
  end: Int,
  tag: Int,
  start: Int,
  held: Int,
  got: Int,
) -> Option<Reach> {
  let first = byte_at(bytes, at, end)
  if first == 255 {
    Some(Reach { after: at + 1, got, done: True })
  } else if first < 0 {
    Some(Reach { after: at, got, done: False })
  } else {
    let size = if first == 0x58 {
      byte_at(bytes, at + 1, end)
    } else if first >= 0x41 && first <= 0x57 {
      first - 0x40
    } else {
      -2
    }
    let content = if first == 0x58 {
      at + 2
    } else {
      at + 1
    }
    if size == -1 {
      // The second head byte is past the window.
      Some(Reach { after: at, got, done: False })
    } else if size < 1 || size > 64 || first == 0x58 && size < 24 {
      None
    } else if size == 64 {
      if content + 64 > end {
        Some(Reach { after: at, got, done: False })
      } else {
        let after = content + 64
        let taken = got + 64
        let filled = if tag == 0 {
          held + taken
        } else {
          held + after - start
        }
        if filled >= chunk_bytes {
          Some(Reach { after, got: taken, done: False })
        } else {
          reach(bytes, after, end, tag, start, held, taken)
        }
      }
    } else if content + size >= end {
      // A short chunk is the last: it is taken with its break.
      Some(Reach { after: at, got, done: False })
    } else if builtin.index_bytearray(bytes, content + size) == 255 {
      Some(Reach { after: content + size + 1, got: got + size, done: True })
    } else {
      None
    }
  }
}

/// The content of the chunks in [at, to), in order.
fn contents(bytes: ByteArray, at: Int, to: Int) -> List<ByteArray> {
  if at >= to {
    []
  } else {
    let first = builtin.index_bytearray(bytes, at)
    if first == 255 {
      []
    } else if first == 0x58 {
      let size = builtin.index_bytearray(bytes, at + 1)
      [
        builtin.slice_bytearray(at + 2, size, bytes),
        ..contents(bytes, at + 2 + size, to)
      ]
    } else {
      let size = first - 0x40
      [
        builtin.slice_bytearray(at + 1, size, bytes),
        ..contents(bytes, at + 1 + size, to)
      ]
    }
  }
}

/// `pending` cut into the chunks it fills: the full ones, then, when `last`,
/// whatever is left. Returns the leaf after them and what stays pending.
fn flush(
  leaf: FrontierV1,
  pending: ByteArray,
  last: Bool,
) -> Pair<FrontierV1, ByteArray> {
  let size = builtin.length_of_bytearray(pending)
  if size >= chunk_bytes {
    flush(
      append_chunk(leaf, builtin.slice_bytearray(0, chunk_bytes, pending)),
      builtin.slice_bytearray(chunk_bytes, size - chunk_bytes, pending),
      last,
    )
  } else if last && size > 0 {
    Pair(append_chunk(leaf, pending), #"")
  } else {
    Pair(leaf, pending)
  }
}

/// One step of the leaf `tag` in `parent` at `at`: the chunks up to one more
/// committed chunk, or up to and including the break.
fn stream(
  bytes: ByteArray,
  at: Int,
  parent: Frame,
  tag: Int,
  leaf: FrontierV1,
  pending: ByteArray,
  length: Int,
  cbor_length: Int,
  all_ff: Bool,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  let held = builtin.length_of_bytearray(pending)
  when reach(bytes, at, end, tag, at, held, 0) is {
    None -> Refused
    Some(Reach { after, got, done }) ->
      if after == at {
        Refused
      } else {
        let pieces = if tag == 0 || all_ff || length == 0 {
          contents(bytes, at, after)
        } else {
          []
        }
        let first_ok = length > 0 || tag == 0 || when pieces is {
          [first, ..] -> builtin.index_bytearray(first, 0) != 0
          [] -> False
        }
        let all_ff = all_ff && list.all(pieces, is_all_ones)
        let added = if tag == 0 {
          concat_balanced_v1([pending, ..pieces])
        } else {
          builtin.append_bytearray(
            pending,
            builtin.slice_bytearray(at, after - at, bytes),
          )
        }
        let Pair(leaf, pending) = flush(leaf, added, done)
        let length = length + got
        let cbor_length = cbor_length + after - at
        if !first_ok {
          Refused
        } else if done {
          // An indefinite string is canonical only past 64 bytes.
          if length <= 64 {
            Refused
          } else {
            let root = frontier_root(leaf)
            complete(
              if tag == 0 {
                bytes_leaf(root, length, cbor_length)
              } else {
                integer_leaf(
                  root,
                  cbor_length,
                  magnitude_memory(tag, length, all_ff),
                )
              },
              after,
              parent,
              stack,
              end,
            )
          }
        } else {
          Advanced(
            after,
            Leaf { parent, tag, leaf, pending, length, cbor_length, all_ff },
            stack,
          )
        }
      }
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
            fields(
              bytes,
              after,
              LargeConstrFields {
                index_root: chunk_root_v1(
                  builtin.slice_bytearray(next + 1, after - next - 1, bytes),
                ),
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
    if byte_at(bytes, next, end) == 0x5f {
      stream(
        bytes,
        next + 1,
        frame,
        tag,
        empty_frontier,
        builtin.slice_bytearray(at, next + 1 - at, bytes),
        0,
        next + 1 - at,
        tag == 3,
        stack,
        end,
      )
    } else {
      when short_bytes(bytes, next, end) is {
        Some((magnitude, after)) -> {
          let length = builtin.length_of_bytearray(magnitude)
          if length >= 9 && builtin.index_bytearray(magnitude, 0) != 0 {
            complete(
              integer_leaf(
                chunk_root_v1(builtin.slice_bytearray(at, after - at, bytes)),
                after - at,
                magnitude_memory(tag, length, is_all_ones(magnitude)),
              ),
              after,
              frame,
              stack,
              end,
            )
          } else {
            Refused
          }
        }
        None -> Refused
      }
    }
  } else {
    Refused
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
                chunk_root_v1(builtin.slice_bytearray(at, next - at, bytes)),
                next - at,
                integer_memory(major, value),
              ),
              next,
              frame,
              stack,
              end,
            )
          }
        } else if major == 2 {
          if value < 0 {
            stream(
              bytes,
              next,
              frame,
              0,
              empty_frontier,
              #"",
              0,
              1,
              True,
              stack,
              end,
            )
          } else {
            when short_bytes(bytes, at, end) is {
              Some((content, after)) ->
                complete(
                  bytes_leaf(
                    chunk_root_v1(content),
                    builtin.length_of_bytearray(content),
                    after - at,
                  ),
                  after,
                  frame,
                  stack,
                  end,
                )
              None -> Refused
            }
          }
        } else if major == 4 {
          fields(bytes, at, ListItems, frame, stack, end)
        } else if major == 5 {
          if value == 0 {
            complete(empty_map, next, frame, stack, end)
          } else if value > 0 {
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

fn step(
  bytes: ByteArray,
  at: Int,
  frame: Frame,
  stack: List<Entry>,
  end: Int,
) -> Progress {
  when frame is {
    Folding { open, folded } -> fold(open, folded, at, stack, end)
    Leaf { parent, tag, leaf, pending, length, cbor_length, all_ff } ->
      stream(
        bytes,
        at,
        parent,
        tag,
        leaf,
        pending,
        length,
        cbor_length,
        all_ff,
        stack,
        end,
      )
    _ -> read(bytes, at, frame, stack, end)
  }
}

/// Up to `steps` steps over `bytes[at, end)` from `frame` with `stack`
/// (top first). Stops early when the value is summarised or refused.
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
