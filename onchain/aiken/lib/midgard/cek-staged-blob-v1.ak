//// Flat chunk-tree roots of byte strings of any length, and the binding of
//// operands staged in reference-input inline datums to those roots.
////
//// A byte string is cut into 4,095-byte chunks from offset 0 (only the last
//// chunk may be shorter; the empty string is one empty chunk). A one-chunk
//// string is committed by its chunk hash. Otherwise the left subtree holds the
//// largest power-of-two number of chunks strictly below the chunk count and the
//// right subtree the rest, recursively, and every branch commits the byte
//// length beneath it. Equivalently: pair the chunk hashes bottom-up, level by
//// level, carrying an odd last node up unchanged.
////
//// The roots are byte-identical to `cek_proof_v1.bounded_blob_root_v1` (up to
//// 9,215 bytes), `cek_data_v1.bounded_blob_root_v1` (up to 16,384 bytes) and the
//// streaming `cek_blob_frontier_v1` (any length). The encoding is written for
//// execution cost: constant CBOR headers for a full chunk and for the two
//// 32-byte child hashes, the branch length's CBOR unsigned integer built with
//// `cons_bytearray`, and branch lengths derived from tree position.
////
//// Under protocol version 11 the byte-string builtins other than
//// `lengthOfByteString` refuse an argument over 65,536 bytes, so a longer value
//// cannot be sliced or hashed whole. `flat_root_v1` therefore roots values up
//// to 65,536 bytes, and `flat_root_of_pieces_v1` roots the concatenation of
//// pieces of at most 65,536 bytes each without building it.
////
//// A staging output carries `Constr 0 [List [B chunk, ...]]` with one to three
//// chunks as its inline datum. An operand is a sequence of staging outputs,
//// named by output reference; its chunks, read in order, must have the
//// canonical shape above.

use aiken/builtin
use aiken/collection/list
use cardano/transaction.{InlineDatum, Input, OutputReference}

/// The chunk size of the flat tree.
pub const chunk_bytes = 4095

/// "MidgardCekBlobChunkV1".
const chunk_domain = #"4d69646761726443656b426c6f624368756e6b5631"

/// The chunk domain followed by the CBOR head of a 4,095-byte string.
const full_chunk_prefix = #"4d69646761726443656b426c6f624368756e6b5631590fff"

/// "MidgardCekBlobBranchV1", a three-item CBOR array head and the CBOR head of
/// the left 32-byte hash.
const branch_prefix = #"4d69646761726443656b426c6f624272616e63685631835820"

/// The CBOR head of the right 32-byte hash.
const hash_head = #"5820"

/// The hash of one chunk of at most 4,095 bytes.
pub fn chunk_root_v1(chunk: ByteArray) -> ByteArray {
  let length = builtin.length_of_bytearray(chunk)
  if length == chunk_bytes {
    builtin.blake2b_256(builtin.append_bytearray(full_chunk_prefix, chunk))
  } else {
    expect length < chunk_bytes
    let head = if length < 24 {
      builtin.cons_bytearray(64 + length, #"")
    } else if length < 256 {
      builtin.cons_bytearray(0x58, builtin.cons_bytearray(length, #""))
    } else {
      builtin.cons_bytearray(
        0x59,
        builtin.cons_bytearray(
          length / 256,
          builtin.cons_bytearray(length % 256, #""),
        ),
      )
    }
    builtin.blake2b_256(
      builtin.append_bytearray(
        builtin.append_bytearray(chunk_domain, head),
        chunk,
      ),
    )
  }
}

/// The canonical CBOR unsigned integer (major type 0) of `value`.
fn cbor_uint(value: Int) -> ByteArray {
  if value < 65536 {
    if value > 255 {
      builtin.cons_bytearray(
        0x19,
        builtin.cons_bytearray(
          value / 256,
          builtin.cons_bytearray(value % 256, #""),
        ),
      )
    } else if value > 23 {
      builtin.cons_bytearray(0x18, builtin.cons_bytearray(value, #""))
    } else {
      builtin.cons_bytearray(value, #"")
    }
  } else if value < 4294967296 {
    builtin.cons_bytearray(0x1a, builtin.integer_to_bytearray(True, 4, value))
  } else {
    builtin.cons_bytearray(0x1b, builtin.integer_to_bytearray(True, 8, value))
  }
}

/// The hash of a branch over two 32-byte child hashes covering `byte_length`
/// bytes. Both children must be 32-byte hashes.
pub fn branch_root_v1(
  left: ByteArray,
  right: ByteArray,
  byte_length: Int,
) -> ByteArray {
  builtin.blake2b_256(
    builtin.append_bytearray(
      builtin.append_bytearray(branch_prefix, left),
      builtin.append_bytearray(
        hash_head,
        builtin.append_bytearray(right, cbor_uint(byte_length)),
      ),
    ),
  )
}

/// Pairs one level of nodes. Each node at this level covers `width` bytes
/// (the last may cover fewer) and the first starts at byte `start`.
fn fold_level(
  nodes: List<ByteArray>,
  start: Int,
  width: Int,
  total: Int,
) -> List<ByteArray> {
  when nodes is {
    [left, right, ..rest] -> {
      let next = start + width + width
      let end = if next < total {
        next
      } else {
        total
      }
      [
        branch_root_v1(left, right, end - start),
        ..fold_level(rest, next, width, total)
      ]
    }
    _ -> nodes
  }
}

/// The root over chunk hashes whose chunks hold `total` bytes in all.
fn fold_tree(nodes: List<ByteArray>, width: Int, total: Int) -> ByteArray {
  when nodes is {
    [root] -> root
    _ -> fold_tree(fold_level(nodes, 0, width, total), width + width, total)
  }
}

fn slice_chunk_roots(
  bytes: ByteArray,
  offset: Int,
  total: Int,
) -> List<ByteArray> {
  if offset < total {
    [
      chunk_root_v1(builtin.slice_bytearray(offset, chunk_bytes, bytes)),
      ..slice_chunk_roots(bytes, offset + chunk_bytes, total)
    ]
  } else {
    []
  }
}

/// The flat root of a byte string of at most 65,536 bytes.
pub fn flat_root_v1(bytes: ByteArray) -> ByteArray {
  let total = builtin.length_of_bytearray(bytes)
  if total <= chunk_bytes {
    chunk_root_v1(bytes)
  } else {
    fold_tree(slice_chunk_roots(bytes, 0, total), chunk_bytes, total)
  }
}

fn full_chunk_root(chunk: ByteArray) -> ByteArray {
  builtin.blake2b_256(builtin.append_bytearray(full_chunk_prefix, chunk))
}

/// Chunk roots of `carry` followed by `pieces`, where `carry` holds
/// `carry_length` bytes, fewer than one chunk. Also returns the byte length.
fn piece_chunk_roots(
  pieces: List<ByteArray>,
  carry: ByteArray,
  carry_length: Int,
) -> Pair<List<ByteArray>, Int> {
  when pieces is {
    [] ->
      if carry_length > 0 {
        Pair([chunk_root_v1(carry)], carry_length)
      } else {
        Pair([], 0)
      }
    [piece, ..rest] -> {
      let size = builtin.length_of_bytearray(piece)
      let missing = chunk_bytes - carry_length
      if size < missing {
        piece_chunk_roots(
          rest,
          builtin.append_bytearray(carry, piece),
          carry_length + size,
        )
      } else {
        let Pair(roots, length) = piece_tail_roots(piece, missing, size, rest)
        Pair(
          [
            full_chunk_root(
              builtin.append_bytearray(
                carry,
                builtin.slice_bytearray(0, missing, piece),
              ),
            ),
            ..roots
          ],
          length + chunk_bytes,
        )
      }
    }
  }
}

/// Chunk roots of `piece` from `offset` on, then of the pieces after it.
fn piece_tail_roots(
  piece: ByteArray,
  offset: Int,
  size: Int,
  rest: List<ByteArray>,
) -> Pair<List<ByteArray>, Int> {
  let remaining = size - offset
  if remaining >= chunk_bytes {
    let Pair(roots, length) = piece_tail_roots(
      piece,
      offset + chunk_bytes,
      size,
      rest,
    )
    Pair(
      [
        full_chunk_root(builtin.slice_bytearray(offset, chunk_bytes, piece)),
        ..roots
      ],
      length + chunk_bytes,
    )
  } else {
    piece_chunk_roots(
      rest,
      builtin.slice_bytearray(offset, remaining, piece),
      remaining,
    )
  }
}

/// The flat root of the concatenation of `pieces`, each at most 65,536 bytes,
/// computed without building the concatenation. Result rooting uses it for
/// results longer than 65,536 bytes, such as an append of two staged operands.
pub fn flat_root_of_pieces_v1(pieces: List<ByteArray>) -> ByteArray {
  let Pair(roots, length) = piece_chunk_roots(pieces, #"", 0)
  when roots is {
    [] -> chunk_root_v1(#"")
    _ -> fold_tree(roots, chunk_bytes, length)
  }
}

/// The staging outputs a step reads, and which of them make up each operand.
/// `refs` lists output references in the order the ledger lists reference
/// inputs; `operands[k]` lists indices into `refs`, in chunk order.
pub type StagedRunsV1 {
  refs: List<OutputReference>,
  operands: List<List<Int>>,
}

/// A bound operand: its flat root, its byte length and its chunks in order.
pub type StagedOperandV1 {
  root: ByteArray,
  length: Int,
  chunks: List<ByteArray>,
}

fn run_chunks(input: Input) -> List<ByteArray> {
  expect InlineDatum(datum) = input.output.datum
  let Pair(constructor, fields) = builtin.un_constr_data(datum)
  expect constructor == 0
  expect [items] = fields
  when builtin.un_list_data(items) is {
    [first] -> [builtin.un_b_data(first)]
    [first, second] -> [builtin.un_b_data(first), builtin.un_b_data(second)]
    [first, second, third] ->
      [
        builtin.un_b_data(first),
        builtin.un_b_data(second),
        builtin.un_b_data(third),
      ]
    _ -> fail
  }
}

fn collect_from(
  inputs: List<Input>,
  wanted: OutputReference,
  rest: List<OutputReference>,
) -> List<List<ByteArray>> {
  expect [input, ..later] = inputs
  if input.output_reference == wanted {
    [run_chunks(input), ..collect_runs_v1(later, rest)]
  } else {
    collect_from(later, wanted, rest)
  }
}

/// The chunk runs of the staging outputs `refs` names, one list per reference,
/// in a single walk of `reference_inputs`. Every reference must be found, in
/// the order given; reference inputs that `refs` does not name are skipped.
pub fn collect_runs_v1(
  reference_inputs: List<Input>,
  refs: List<OutputReference>,
) -> List<List<ByteArray>> {
  when refs is {
    [] -> []
    [wanted, ..rest] -> collect_from(reference_inputs, wanted, rest)
  }
}

fn gather_runs(
  runs: List<List<ByteArray>>,
  indices: List<Int>,
) -> List<ByteArray> {
  when indices is {
    [] -> []
    [index, ..rest] -> {
      expect Some(run) = list.at(runs, index)
      list.concat(run, gather_runs(runs, rest))
    }
  }
}

/// Hashes the chunks of a multi-chunk operand: every chunk but the last is
/// full and the last holds at least one byte.
fn hash_operand_chunks(chunks: List<ByteArray>) -> Pair<List<ByteArray>, Int> {
  when chunks is {
    [last] -> {
      let length = builtin.length_of_bytearray(last)
      expect length > 0
      Pair([chunk_root_v1(last)], length)
    }
    [chunk, ..rest] -> {
      expect builtin.length_of_bytearray(chunk) == chunk_bytes
      let Pair(roots, length) = hash_operand_chunks(rest)
      Pair([full_chunk_root(chunk), ..roots], length + chunk_bytes)
    }
    [] -> fail
  }
}

/// Binds one operand from the runs `collect_runs_v1` returned: `indices`
/// selects its runs in chunk order. The chunks must have the canonical shape,
/// so the root is the flat root of their concatenation.
pub fn bind_operand_v1(
  runs: List<List<ByteArray>>,
  indices: List<Int>,
) -> StagedOperandV1 {
  let chunks = gather_runs(runs, indices)
  when chunks is {
    [only] ->
      StagedOperandV1 {
        root: chunk_root_v1(only),
        length: builtin.length_of_bytearray(only),
        chunks,
      }
    _ -> {
      let Pair(roots, length) = hash_operand_chunks(chunks)
      StagedOperandV1 {
        root: fold_tree(roots, chunk_bytes, length),
        length,
        chunks,
      }
    }
  }
}

fn concat_pairs(pieces: List<ByteArray>) -> List<ByteArray> {
  when pieces is {
    [left, right, ..rest] ->
      [builtin.append_bytearray(left, right), ..concat_pairs(rest)]
    _ -> pieces
  }
}

/// Concatenates byte strings in order, pairwise in a balanced tree, so each
/// byte is copied once per level rather than once per piece. Under protocol
/// version 11 every append argument is at most 65,536 bytes, so an operand
/// longer than that is used through its chunks rather than concatenated.
pub fn concat_balanced_v1(pieces: List<ByteArray>) -> ByteArray {
  when pieces is {
    [] -> #""
    [whole] -> whole
    _ -> concat_balanced_v1(concat_pairs(pieces))
  }
}
