use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes, slice_len,
}
use midgard/fraud_proofs/native_tx/preimages.{decode_canonical_map_header_at}
use midgard/validation_merkle_v1

const frame_domain = #"4d69646761726443656b446174615363616e4672616d655631"

const child_domain = #"4d69646761726443656b446174615363616e4368696c645631"

pub const root_frame: Int = 0

pub const constructor_frame: Int = 1

pub const list_frame: Int = 2

pub const map_frame: Int = 3

pub const max_revealed_data_leaf_bytes: Int = 9215

pub type DataScanFrameV1 {
  kind: Int,
  constructor: Int,
  tail: ByteArray,
  expected_children: Int,
  child_count: Int,
  child_peaks: List<validation_merkle_v1.FrontierPeak>,
  fold_cursor: Int,
  sequence: cek_data_v1.DataSequenceSummaryV1,
}

pub type RawDataLeafV1 {
  next_offset: Int,
  summary: cek_data_v1.DataSummaryV1,
}

pub type OpenedDataFrameV1 {
  next_offset: Int,
  frame: DataScanFrameV1,
}

pub type DataScanControlV1 {
  raw_hash: ByteArray,
  raw_length: Int,
  offset: Int,
  frame_root: ByteArray,
  frame_closed: Bool,
  result: cek_data_v1.DataSummaryV1,
}

fn encode_sequence(summary: cek_data_v1.DataSequenceSummaryV1) -> ByteArray {
  #"84"
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.length))
    |> bytearray.concat(cbor.serialise(summary.payload_cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn encode_summary(summary: cek_data_v1.DataSummaryV1) -> ByteArray {
  #"83"
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

pub fn empty_result_v1() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root: #"", cbor_length: 0, memory: 0 }
}

fn optional_summary_is_well_formed(summary: cek_data_v1.DataSummaryV1) -> Bool {
  if summary.root == #"" {
    and {
      summary.cbor_length == 0,
      summary.memory == 0,
    }
  } else {
    and {
      bytearray.length(summary.root) == 32,
      summary.cbor_length >= 0,
      summary.memory >= 0,
    }
  }
}

pub fn control_is_well_formed(control: DataScanControlV1) -> Bool {
  and {
    bytearray.length(control.raw_hash) == 32,
    control.raw_length > 0,
    control.raw_length <= max_revealed_data_leaf_bytes,
    control.offset >= 0,
    control.offset <= control.raw_length,
    or {
      control.frame_root == #"",
      bytearray.length(control.frame_root) == 32,
    },
    optional_summary_is_well_formed(control.result),
    if control.result.root != #"" {
      and {
        control.frame_root == #"",
        !control.frame_closed,
        control.offset == control.raw_length,
      }
    } else if control.frame_root == #"" {
      and {
        !control.frame_closed,
        control.offset == 0,
      }
    } else {
      True
    },
  }
}

pub fn encode_control_v1(control: DataScanControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(6)
    |> bytearray.concat(encode_definite_bytes(control.raw_hash))
    |> bytearray.concat(cbor.serialise(control.raw_length))
    |> bytearray.concat(cbor.serialise(control.offset))
    |> bytearray.concat(encode_definite_bytes(control.frame_root))
    |> bytearray.concat(cbor.serialise(control.frame_closed))
    |> bytearray.concat(encode_summary(control.result))
}

fn summary_from_data(data: Data) -> cek_data_v1.DataSummaryV1 {
  expect [root_data, cbor_length_data, memory_data] = builtin.un_list_data(data)
  cek_data_v1.DataSummaryV1 {
    root: builtin.un_b_data(root_data),
    cbor_length: builtin.un_i_data(cbor_length_data),
    memory: builtin.un_i_data(memory_data),
  }
}

fn bool_from_data(data: Data) -> Bool {
  let Pair(index, fields) = builtin.un_constr_data(data)
  expect fields == []
  if index == 0 {
    False
  } else {
    expect index == 1
    True
  }
}

pub fn control_from_cbor_v1(control_cbor: ByteArray) -> DataScanControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        raw_hash_data, raw_length_data, offset_data, frame_root_data,
        frame_closed_data, result_data,
      ]
    = builtin.un_list_data(data)
  let control = DataScanControlV1 {
    raw_hash: builtin.un_b_data(raw_hash_data),
    raw_length: builtin.un_i_data(raw_length_data),
    offset: builtin.un_i_data(offset_data),
    frame_root: builtin.un_b_data(frame_root_data),
    frame_closed: bool_from_data(frame_closed_data),
    result: summary_from_data(result_data),
  }
  expect control_is_well_formed(control)
  expect encode_control_v1(control) == control_cbor
  control
}

pub fn hash_control_v1(control: DataScanControlV1) -> ByteArray {
  encode_control_v1(control) |> blake2b_256
}

pub fn initial_control_v1(
  raw_hash: ByteArray,
  raw_length: Int,
) -> DataScanControlV1 {
  let control = DataScanControlV1 {
    raw_hash,
    raw_length,
    offset: 0,
    frame_root: #"",
    frame_closed: False,
    result: empty_result_v1(),
  }
  expect control_is_well_formed(control)
  control
}

fn raw_matches_control(control: DataScanControlV1, raw_cbor: ByteArray) -> Bool {
  and {
    control_is_well_formed(control),
    bytearray.length(raw_cbor) == control.raw_length,
    blake2b_256(raw_cbor) == control.raw_hash,
  }
}

pub fn frame_is_well_formed(frame: DataScanFrameV1) -> Bool {
  and {
    frame.kind >= root_frame,
    frame.kind <= map_frame,
    if frame.kind == constructor_frame {
      frame.constructor >= 0
    } else {
      frame.constructor == 0
    },
    or {
      frame.tail == #"",
      bytearray.length(frame.tail) == 32,
    },
    frame.expected_children >= 0,
    if frame.kind == root_frame {
      frame.expected_children == 1
    } else if frame.kind == map_frame {
      frame.expected_children % 2 == 0
    } else {
      True
    },
    frame.child_count >= 0,
    frame.child_count <= frame.expected_children,
    validation_merkle_v1.frontier_is_well_formed(
      frame.child_count,
      frame.child_peaks,
    ),
    frame.fold_cursor >= 0,
    if frame.kind == map_frame {
      frame.fold_cursor <= frame.expected_children / 2
    } else {
      frame.fold_cursor <= frame.expected_children
    },
    bytearray.length(frame.sequence.root) == 32,
    frame.sequence.length == frame.fold_cursor,
    frame.sequence.payload_cbor_length >= 0,
    frame.sequence.memory >= 0,
    if frame.fold_cursor > 0 {
      frame.child_count == frame.expected_children
    } else {
      True
    },
  }
}

pub fn hash_frame_v1(frame: DataScanFrameV1) -> ByteArray {
  expect frame_is_well_formed(frame)
  frame_domain
    |> bytearray.concat(encode_definite_array_header(8))
    |> bytearray.concat(cbor.serialise(frame.kind))
    |> bytearray.concat(cbor.serialise(frame.constructor))
    |> bytearray.concat(encode_definite_bytes(frame.tail))
    |> bytearray.concat(cbor.serialise(frame.expected_children))
    |> bytearray.concat(cbor.serialise(frame.child_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(frame.child_peaks))
    |> bytearray.concat(cbor.serialise(frame.fold_cursor))
    |> bytearray.concat(encode_sequence(frame.sequence))
    |> blake2b_256
}

pub fn child_leaf_hash_v1(
  child_index: Int,
  child: cek_data_v1.DataSummaryV1,
) -> ByteArray {
  expect child_index >= 0
  expect bytearray.length(child.root) == 32
  expect child.cbor_length >= 0
  expect child.memory >= 0
  child_domain
    |> bytearray.concat(cbor.serialise(child_index))
    |> bytearray.concat(encode_definite_bytes(child.root))
    |> bytearray.concat(cbor.serialise(child.cbor_length))
    |> bytearray.concat(cbor.serialise(child.memory))
    |> blake2b_256
}

pub fn append_child_v1(
  frame: DataScanFrameV1,
  child: cek_data_v1.DataSummaryV1,
) -> Option<DataScanFrameV1> {
  if and {
    frame_is_well_formed(frame),
    frame.fold_cursor == 0,
    frame.child_count < frame.expected_children,
  } {
    let next_count = frame.child_count + 1
    Some(
      DataScanFrameV1 {
        ..frame,
        child_count: next_count,
        child_peaks: validation_merkle_v1.append_leaf(
          frame.child_count,
          frame.child_peaks,
          child_leaf_hash_v1(frame.child_count, child),
        ),
      },
    )
  } else {
    None
  }
}

pub fn fold_list_child_v1(
  frame: DataScanFrameV1,
  child_index: Int,
  child: cek_data_v1.DataSummaryV1,
  siblings: List<ByteArray>,
) -> Option<DataScanFrameV1> {
  let expected_index = frame.expected_children - frame.fold_cursor - 1
  if and {
    frame_is_well_formed(frame),
    or {
      frame.kind == constructor_frame,
      frame.kind == list_frame,
    },
    frame.child_count == frame.expected_children,
    frame.fold_cursor < frame.expected_children,
    child_index == expected_index,
    validation_merkle_v1.verify_membership(
      frame.child_count,
      frame.child_peaks,
      child_index,
      child_leaf_hash_v1(child_index, child),
      siblings,
    ),
  } {
    Some(
      DataScanFrameV1 {
        ..frame,
        fold_cursor: frame.fold_cursor + 1,
        sequence: cek_data_v1.prepend_data_list_summary_v1(
          child,
          frame.sequence,
        ),
      },
    )
  } else {
    None
  }
}

pub fn fold_map_pair_v1(
  frame: DataScanFrameV1,
  pair_index: Int,
  key: cek_data_v1.DataSummaryV1,
  value: cek_data_v1.DataSummaryV1,
  key_siblings: List<ByteArray>,
  value_siblings: List<ByteArray>,
) -> Option<DataScanFrameV1> {
  let pair_count = frame.expected_children / 2
  let expected_pair_index = pair_count - frame.fold_cursor - 1
  let key_index = pair_index * 2
  let value_index = key_index + 1
  if and {
    frame_is_well_formed(frame),
    frame.kind == map_frame,
    frame.child_count == frame.expected_children,
    frame.fold_cursor < pair_count,
    pair_index == expected_pair_index,
    validation_merkle_v1.verify_membership(
      frame.child_count,
      frame.child_peaks,
      key_index,
      child_leaf_hash_v1(key_index, key),
      key_siblings,
    ),
    validation_merkle_v1.verify_membership(
      frame.child_count,
      frame.child_peaks,
      value_index,
      child_leaf_hash_v1(value_index, value),
      value_siblings,
    ),
  } {
    Some(
      DataScanFrameV1 {
        ..frame,
        fold_cursor: frame.fold_cursor + 1,
        sequence: cek_data_v1.prepend_data_pair_summary_v1(
          key,
          value,
          frame.sequence,
        ),
      },
    )
  } else {
    None
  }
}

pub fn finalized_summary_v1(
  frame: DataScanFrameV1,
  root_child: Option<cek_data_v1.DataSummaryV1>,
) -> Option<cek_data_v1.DataSummaryV1> {
  if !frame_is_well_formed(frame) {
    None
  } else if frame.kind == root_frame {
    expect Some(child) = root_child
    if and {
      frame.child_count == 1,
      frame.fold_cursor == 0,
      validation_merkle_v1.verify_membership(
        1,
        frame.child_peaks,
        0,
        child_leaf_hash_v1(0, child),
        [],
      ),
    } {
      Some(child)
    } else {
      None
    }
  } else if and {
    frame.kind == constructor_frame,
    frame.fold_cursor == frame.expected_children,
    root_child == None,
  } {
    Some(
      if frame.constructor <= 127 {
        cek_data_v1.small_constr_data_summary_v1(
          frame.constructor,
          frame.sequence,
        )
      } else {
        cek_data_v1.large_constr_data_summary_v1(
          frame.constructor,
          bounded_blob_root_v1(
            cbor.serialise(builtin.i_data(frame.constructor)),
          ),
          frame.sequence,
        )
      },
    )
  } else if and {
    frame.kind == list_frame,
    frame.fold_cursor == frame.expected_children,
    root_child == None,
  } {
    Some(cek_data_v1.list_data_summary_v1(frame.sequence))
  } else if and {
    frame.kind == map_frame,
    frame.fold_cursor == frame.expected_children / 2,
    root_child == None,
  } {
    Some(cek_data_v1.map_data_summary_v1(frame.sequence))
  } else {
    None
  }
}

fn raw_preimage_matches(raw_cbor: ByteArray, expected_hash: ByteArray) -> Bool {
  and {
    bytearray.length(expected_hash) == 32,
    blake2b_256(raw_cbor) == expected_hash,
  }
}

pub fn bounded_blob_root_v1(bytes: ByteArray) -> ByteArray {
  let length = bytearray.length(bytes)
  expect length <= max_revealed_data_leaf_bytes
  if length <= cek_proof_v1.max_blob_chunk_bytes {
    cek_proof_v1.hash_blob_chunk_v1(bytes)
  } else {
    let first = slice_len(bytes, 0, cek_proof_v1.max_blob_chunk_bytes)
    let remaining = length - cek_proof_v1.max_blob_chunk_bytes
    let second_length = if remaining <= cek_proof_v1.max_blob_chunk_bytes {
      remaining
    } else {
      cek_proof_v1.max_blob_chunk_bytes
    }
    let second = slice_len(
      bytes,
      cek_proof_v1.max_blob_chunk_bytes,
      second_length,
    )
    let left = cek_proof_v1.hash_blob_branch_v1(
      cek_proof_v1.hash_blob_chunk_v1(first),
      cek_proof_v1.hash_blob_chunk_v1(second),
      cek_proof_v1.max_blob_chunk_bytes + second_length,
    )
    if remaining <= cek_proof_v1.max_blob_chunk_bytes {
      left
    } else {
      let third_length = remaining - second_length
      let third = slice_len(
        bytes,
        cek_proof_v1.max_blob_chunk_bytes + second_length,
        third_length,
      )
      cek_proof_v1.hash_blob_branch_v1(
        left,
        cek_proof_v1.hash_blob_chunk_v1(third),
        length,
      )
    }
  }
}

fn data_is_small_leaf(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, True, True)
}

/// Authenticates one canonical integer or at-most-one-blob-chunk byte leaf at
/// the current raw-CBOR cursor. Larger scalar leaves use the streaming blob
/// submachine; this helper is never an admission cap on Data itself.
pub fn small_leaf_at_v1(
  raw_cbor: ByteArray,
  expected_hash: ByteArray,
  offset: Int,
  item_cbor: ByteArray,
) -> Option<RawDataLeafV1> {
  let item_length = bytearray.length(item_cbor)
  let next_offset = offset + item_length
  if and {
    raw_preimage_matches(raw_cbor, expected_hash),
    offset >= 0,
    item_length > 0,
    item_length <= cek_proof_v1.max_blob_chunk_bytes,
    next_offset <= bytearray.length(raw_cbor),
    slice_len(raw_cbor, offset, item_length) == item_cbor,
  } {
    when cbor.deserialise(item_cbor) is {
      None -> None
      Some(data) ->
        if and {
          data_is_small_leaf(data),
          cbor.serialise(data) == item_cbor,
          builtin.choose_data(
            data,
            True,
            True,
            True,
            True,
            bytearray.length(builtin.un_b_data(data)) <= cek_proof_v1.max_blob_chunk_bytes,
          ),
        } {
          Some(
            RawDataLeafV1 {
              next_offset,
              summary: cek_data_v1.semantic_data_summary_v1(data),
            },
          )
        } else {
          None
        }
    }
  } else {
    None
  }
}

/// Authenticates any canonical integer or byte-string Data leaf that fits the
/// independently revealed field envelope. Its blob root is the exact one-,
/// two-, or three-chunk CEK tree, so 4,095-byte material chunks never become
/// an arbitrary value-size cap.
pub fn revealed_leaf_at_v1(
  raw_cbor: ByteArray,
  expected_hash: ByteArray,
  offset: Int,
  item_length: Int,
) -> Option<RawDataLeafV1> {
  let next_offset = offset + item_length
  if and {
    raw_preimage_matches(raw_cbor, expected_hash),
    offset >= 0,
    item_length > 0,
    item_length <= max_revealed_data_leaf_bytes,
    next_offset <= bytearray.length(raw_cbor),
  } {
    let item_cbor = slice_len(raw_cbor, offset, item_length)
    when cbor.deserialise(item_cbor) is {
      None -> None
      Some(data) ->
        if cbor.serialise(data) != item_cbor {
          None
        } else {
          let summary = builtin.choose_data(
            data,
            None,
            None,
            None,
            Some(
              cek_data_v1.integer_data_summary_v1(
                builtin.un_i_data(data),
                bounded_blob_root_v1(item_cbor),
              ),
            ),
            {
              let bytes = builtin.un_b_data(data)
              Some(
                cek_data_v1.bytes_data_summary_v1(
                  bytearray.length(bytes),
                  bounded_blob_root_v1(bytes),
                ),
              )
            },
          )
          when summary is {
            None -> None
            Some(exact) -> Some(RawDataLeafV1 { next_offset, summary: exact })
          }
        }
    }
  } else {
    None
  }
}

fn canonical_small_constructor_prefix(constructor: Int) -> ByteArray {
  expect constructor >= 0
  expect constructor <= 127
  if constructor <= 6 {
    #"d8"
      |> bytearray.concat(bytearray.from_int_big_endian(121 + constructor, 1))
  } else {
    #"d9"
      |> bytearray.concat(
          bytearray.from_int_big_endian(1280 + constructor - 7, 2),
        )
  }
}

fn sequence_header_at(
  raw_cbor: ByteArray,
  offset: Int,
  expected_children: Int,
) -> Option<Int> {
  if expected_children == 0 {
    if slice_len(raw_cbor, offset, 1) == #"80" {
      Some(offset + 1)
    } else {
      None
    }
  } else if slice_len(raw_cbor, offset, 1) == #"9f" {
    Some(offset + 1)
  } else {
    None
  }
}

pub fn open_small_constructor_at_v1(
  raw_cbor: ByteArray,
  expected_hash: ByteArray,
  offset: Int,
  constructor: Int,
  expected_children: Int,
  tail: ByteArray,
) -> Option<OpenedDataFrameV1> {
  let prefix = canonical_small_constructor_prefix(constructor)
  let prefix_length = bytearray.length(prefix)
  if and {
    raw_preimage_matches(raw_cbor, expected_hash),
    offset >= 0,
    expected_children >= 0,
    offset + prefix_length < bytearray.length(raw_cbor),
    slice_len(raw_cbor, offset, prefix_length) == prefix,
  } {
    when
      sequence_header_at(raw_cbor, offset + prefix_length, expected_children)
    is {
      None -> None
      Some(next_offset) ->
        Some(
          OpenedDataFrameV1 {
            next_offset,
            frame: DataScanFrameV1 {
              kind: constructor_frame,
              constructor,
              tail,
              expected_children,
              child_count: 0,
              child_peaks: validation_merkle_v1.empty_frontier(),
              fold_cursor: 0,
              sequence: cek_data_v1.empty_data_list_summary_v1(),
            },
          },
        )
    }
  } else {
    None
  }
}

pub fn open_constructor_at_v1(
  raw_cbor: ByteArray,
  expected_hash: ByteArray,
  offset: Int,
  constructor: Int,
  expected_children: Int,
  tail: ByteArray,
) -> Option<OpenedDataFrameV1> {
  if constructor <= 127 {
    open_small_constructor_at_v1(
      raw_cbor,
      expected_hash,
      offset,
      constructor,
      expected_children,
      tail,
    )
  } else {
    let constructor_cbor = cbor.serialise(builtin.i_data(constructor))
    let constructor_cbor_length = bytearray.length(constructor_cbor)
    let fields_offset = offset + 3 + constructor_cbor_length
    if and {
      constructor > 127,
      raw_preimage_matches(raw_cbor, expected_hash),
      offset >= 0,
      expected_children >= 0,
      constructor_cbor_length <= max_revealed_data_leaf_bytes,
      fields_offset < bytearray.length(raw_cbor),
      slice_len(raw_cbor, offset, 3) == #"d86682",
      slice_len(raw_cbor, offset + 3, constructor_cbor_length) == constructor_cbor,
    } {
      when sequence_header_at(raw_cbor, fields_offset, expected_children) is {
        None -> None
        Some(next_offset) ->
          Some(
            OpenedDataFrameV1 {
              next_offset,
              frame: DataScanFrameV1 {
                kind: constructor_frame,
                constructor,
                tail,
                expected_children,
                child_count: 0,
                child_peaks: validation_merkle_v1.empty_frontier(),
                fold_cursor: 0,
                sequence: cek_data_v1.empty_data_list_summary_v1(),
              },
            },
          )
      }
    } else {
      None
    }
  }
}

pub fn open_list_at_v1(
  raw_cbor: ByteArray,
  expected_hash: ByteArray,
  offset: Int,
  expected_children: Int,
  tail: ByteArray,
) -> Option<OpenedDataFrameV1> {
  if and {
    raw_preimage_matches(raw_cbor, expected_hash),
    offset >= 0,
    expected_children >= 0,
    offset < bytearray.length(raw_cbor),
  } {
    when sequence_header_at(raw_cbor, offset, expected_children) is {
      None -> None
      Some(next_offset) ->
        Some(
          OpenedDataFrameV1 {
            next_offset,
            frame: DataScanFrameV1 {
              kind: list_frame,
              constructor: 0,
              tail,
              expected_children,
              child_count: 0,
              child_peaks: validation_merkle_v1.empty_frontier(),
              fold_cursor: 0,
              sequence: cek_data_v1.empty_data_list_summary_v1(),
            },
          },
        )
    }
  } else {
    None
  }
}

pub fn open_map_at_v1(
  raw_cbor: ByteArray,
  expected_hash: ByteArray,
  offset: Int,
  tail: ByteArray,
) -> Option<OpenedDataFrameV1> {
  if and {
    raw_preimage_matches(raw_cbor, expected_hash),
    offset >= 0,
    offset < bytearray.length(raw_cbor),
  } {
    let Pair(next_offset, pair_count) = decode_canonical_map_header_at(
      raw_cbor,
      offset,
    )
    Some(
      OpenedDataFrameV1 {
        next_offset,
        frame: DataScanFrameV1 {
          kind: map_frame,
          constructor: 0,
          tail,
          expected_children: pair_count * 2,
          child_count: 0,
          child_peaks: validation_merkle_v1.empty_frontier(),
          fold_cursor: 0,
          sequence: cek_data_v1.empty_data_pair_summary_v1(),
        },
      },
    )
  } else {
    None
  }
}

pub fn close_sequence_at_v1(
  raw_cbor: ByteArray,
  expected_hash: ByteArray,
  offset: Int,
  frame: DataScanFrameV1,
) -> Option<Int> {
  if and {
    raw_preimage_matches(raw_cbor, expected_hash),
    frame_is_well_formed(frame),
    frame.child_count == frame.expected_children,
    or {
      frame.kind == constructor_frame,
      frame.kind == list_frame,
    },
  } {
    if frame.expected_children == 0 {
      Some(offset)
    } else if and {
      offset < bytearray.length(raw_cbor),
      slice_len(raw_cbor, offset, 1) == #"ff",
    } {
      Some(offset + 1)
    } else {
      None
    }
  } else {
    None
  }
}

/// Pops a completed content-addressed child frame into its authenticated
/// parent. Only the two bounded frame preimages are revealed in this step.
pub fn append_completed_child_v1(
  child: DataScanFrameV1,
  child_summary: cek_data_v1.DataSummaryV1,
  parent: DataScanFrameV1,
) -> Option<DataScanFrameV1> {
  if and {
    child.tail == hash_frame_v1(parent),
    finalized_summary_v1(child, None) == Some(child_summary),
  } {
    append_child_v1(parent, child_summary)
  } else {
    None
  }
}

fn open_parent_tail_v1(
  control: DataScanControlV1,
  parent: Option<DataScanFrameV1>,
) -> Option<ByteArray> {
  if and {
    control_is_well_formed(control),
    control.result.root == #"",
    !control.frame_closed,
  } {
    when parent is {
      None ->
        if control.frame_root == #"" {
          Some(#"")
        } else {
          None
        }
      Some(frame) ->
        if and {
          frame_is_well_formed(frame),
          control.frame_root == hash_frame_v1(frame),
          frame.fold_cursor == 0,
          frame.child_count < frame.expected_children,
        } {
          Some(control.frame_root)
        } else {
          None
        }
    }
  } else {
    None
  }
}

fn control_with_opened_frame_v1(
  control: DataScanControlV1,
  opened: OpenedDataFrameV1,
) -> Option<DataScanControlV1> {
  let next = DataScanControlV1 {
    ..control,
    offset: opened.next_offset,
    frame_root: hash_frame_v1(opened.frame),
    frame_closed: opened.frame.expected_children == 0,
  }
  if control_is_well_formed(next) {
    Some(next)
  } else {
    None
  }
}

pub fn open_constructor_control_step_v1(
  control: DataScanControlV1,
  raw_cbor: ByteArray,
  parent: Option<DataScanFrameV1>,
  constructor: Int,
  expected_children: Int,
) -> Option<DataScanControlV1> {
  if raw_matches_control(control, raw_cbor) {
    when open_parent_tail_v1(control, parent) is {
      None -> None
      Some(tail) ->
        when
          open_constructor_at_v1(
            raw_cbor,
            control.raw_hash,
            control.offset,
            constructor,
            expected_children,
            tail,
          )
        is {
          None -> None
          Some(opened) -> control_with_opened_frame_v1(control, opened)
        }
    }
  } else {
    None
  }
}

pub fn open_list_control_step_v1(
  control: DataScanControlV1,
  raw_cbor: ByteArray,
  parent: Option<DataScanFrameV1>,
  expected_children: Int,
) -> Option<DataScanControlV1> {
  if raw_matches_control(control, raw_cbor) {
    when open_parent_tail_v1(control, parent) is {
      None -> None
      Some(tail) ->
        when
          open_list_at_v1(
            raw_cbor,
            control.raw_hash,
            control.offset,
            expected_children,
            tail,
          )
        is {
          None -> None
          Some(opened) -> control_with_opened_frame_v1(control, opened)
        }
    }
  } else {
    None
  }
}

pub fn open_map_control_step_v1(
  control: DataScanControlV1,
  raw_cbor: ByteArray,
  parent: Option<DataScanFrameV1>,
) -> Option<DataScanControlV1> {
  if raw_matches_control(control, raw_cbor) {
    when open_parent_tail_v1(control, parent) is {
      None -> None
      Some(tail) ->
        when
          open_map_at_v1(raw_cbor, control.raw_hash, control.offset, tail)
        is {
          None -> None
          Some(opened) -> control_with_opened_frame_v1(control, opened)
        }
    }
  } else {
    None
  }
}

pub fn reveal_leaf_control_step_v1(
  control: DataScanControlV1,
  raw_cbor: ByteArray,
  parent: Option<DataScanFrameV1>,
  item_length: Int,
) -> Option<DataScanControlV1> {
  if and {
    raw_matches_control(control, raw_cbor),
    control.result.root == #"",
    !control.frame_closed,
  } {
    when
      revealed_leaf_at_v1(
        raw_cbor,
        control.raw_hash,
        control.offset,
        item_length,
      )
    is {
      None -> None
      Some(leaf) ->
        when parent is {
          None ->
            if and {
              control.frame_root == #"",
              leaf.next_offset == control.raw_length,
            } {
              let next = DataScanControlV1 {
                ..control,
                offset: leaf.next_offset,
                result: leaf.summary,
              }
              if control_is_well_formed(next) {
                Some(next)
              } else {
                None
              }
            } else {
              None
            }
          Some(frame) ->
            if and {
              frame_is_well_formed(frame),
              control.frame_root == hash_frame_v1(frame),
            } {
              when append_child_v1(frame, leaf.summary) is {
                None -> None
                Some(next_frame) -> {
                  let next = DataScanControlV1 {
                    ..control,
                    offset: leaf.next_offset,
                    frame_root: hash_frame_v1(next_frame),
                    frame_closed: and {
                      next_frame.kind == map_frame,
                      next_frame.child_count == next_frame.expected_children,
                    },
                  }
                  if control_is_well_formed(next) {
                    Some(next)
                  } else {
                    None
                  }
                }
              }
            } else {
              None
            }
        }
    }
  } else {
    None
  }
}

pub fn close_sequence_control_step_v1(
  control: DataScanControlV1,
  raw_cbor: ByteArray,
  frame: DataScanFrameV1,
) -> Option<DataScanControlV1> {
  if and {
    raw_matches_control(control, raw_cbor),
    control.result.root == #"",
    !control.frame_closed,
    frame_is_well_formed(frame),
    control.frame_root == hash_frame_v1(frame),
    frame.fold_cursor == 0,
  } {
    when
      close_sequence_at_v1(raw_cbor, control.raw_hash, control.offset, frame)
    is {
      None -> None
      Some(next_offset) -> {
        let next = DataScanControlV1 {
          ..control,
          offset: next_offset,
          frame_closed: True,
        }
        if control_is_well_formed(next) {
          Some(next)
        } else {
          None
        }
      }
    }
  } else {
    None
  }
}

pub fn fold_list_control_step_v1(
  control: DataScanControlV1,
  frame: DataScanFrameV1,
  child_index: Int,
  child: cek_data_v1.DataSummaryV1,
  siblings: List<ByteArray>,
) -> Option<DataScanControlV1> {
  if and {
    control_is_well_formed(control),
    control.result.root == #"",
    control.frame_closed,
    frame_is_well_formed(frame),
    control.frame_root == hash_frame_v1(frame),
  } {
    when fold_list_child_v1(frame, child_index, child, siblings) is {
      None -> None
      Some(next_frame) ->
        Some(
          DataScanControlV1 { ..control, frame_root: hash_frame_v1(next_frame) },
        )
    }
  } else {
    None
  }
}

pub fn fold_map_control_step_v1(
  control: DataScanControlV1,
  frame: DataScanFrameV1,
  pair_index: Int,
  key: cek_data_v1.DataSummaryV1,
  value: cek_data_v1.DataSummaryV1,
  key_siblings: List<ByteArray>,
  value_siblings: List<ByteArray>,
) -> Option<DataScanControlV1> {
  if and {
    control_is_well_formed(control),
    control.result.root == #"",
    control.frame_closed,
    frame_is_well_formed(frame),
    control.frame_root == hash_frame_v1(frame),
  } {
    when
      fold_map_pair_v1(
        frame,
        pair_index,
        key,
        value,
        key_siblings,
        value_siblings,
      )
    is {
      None -> None
      Some(next_frame) ->
        Some(
          DataScanControlV1 { ..control, frame_root: hash_frame_v1(next_frame) },
        )
    }
  } else {
    None
  }
}

pub fn finalize_frame_control_step_v1(
  control: DataScanControlV1,
  frame: DataScanFrameV1,
  parent: Option<DataScanFrameV1>,
) -> Option<DataScanControlV1> {
  if and {
    control_is_well_formed(control),
    control.result.root == #"",
    control.frame_closed,
    frame_is_well_formed(frame),
    control.frame_root == hash_frame_v1(frame),
  } {
    when finalized_summary_v1(frame, None) is {
      None -> None
      Some(summary) ->
        when parent is {
          None ->
            if and {
              frame.tail == #"",
              control.offset == control.raw_length,
            } {
              let next = DataScanControlV1 {
                ..control,
                frame_root: #"",
                frame_closed: False,
                result: summary,
              }
              if control_is_well_formed(next) {
                Some(next)
              } else {
                None
              }
            } else {
              None
            }
          Some(parent_frame) ->
            when append_completed_child_v1(frame, summary, parent_frame) is {
              None -> None
              Some(next_parent) -> {
                let next = DataScanControlV1 {
                  ..control,
                  frame_root: hash_frame_v1(next_parent),
                  frame_closed: and {
                    next_parent.kind == map_frame,
                    next_parent.child_count == next_parent.expected_children,
                  },
                }
                if control_is_well_formed(next) {
                  Some(next)
                } else {
                  None
                }
              }
            }
        }
    }
  } else {
    None
  }
}
