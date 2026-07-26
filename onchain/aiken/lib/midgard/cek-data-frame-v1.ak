use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/validation_merkle_v1

const frame_domain =
  #"4d69646761726443656b446174614672616d655631"

const child_domain =
  #"4d69646761726443656b446174614672616d654368696c645631"

const uint32_max = 4_294_967_295

const uint64_max = 18_446_744_073_709_551_615

pub const constr_small_frame: Int = 0

pub const constr_large_frame: Int = 1

pub const list_frame: Int = 2

pub const map_frame: Int = 3

/// Content-addressed traversal frame. Large constructor alternatives are
/// represented only by the authenticated scalar submachine summary, so this
/// preimage remains bounded independently of the constructor integer size.
pub type DataFrameV1 {
  kind: Int,
  constructor: Int,
  constructor_cbor_root: ByteArray,
  constructor_cbor_length: Int,
  constructor_memory: Int,
  tail: ByteArray,
  expected_children: Int,
  child_count: Int,
  child_peaks: List<validation_merkle_v1.FrontierPeak>,
  fold_cursor: Int,
  sequence: cek_data_v1.DataSequenceSummaryV1,
}

fn uint32_is_well_formed(value: Int) -> Bool {
  value >= 0 && value <= uint32_max
}

fn uint64_is_well_formed(value: Int) -> Bool {
  value >= 0 && value <= uint64_max
}

fn optional_hash_is_well_formed(value: ByteArray) -> Bool {
  or {
    value == #"",
    bytearray.length(value) == 32,
  }
}

fn constructor_is_well_formed(frame: DataFrameV1) -> Bool {
  if frame.kind == constr_small_frame {
    and {
      frame.constructor >= 0,
      frame.constructor <= 127,
      frame.constructor_cbor_root == #"",
      frame.constructor_cbor_length == 0,
      frame.constructor_memory == 0,
    }
  } else if frame.kind == constr_large_frame {
    and {
      frame.constructor == 0,
      bytearray.length(frame.constructor_cbor_root) == 32,
      frame.constructor_cbor_length > 0,
      uint32_is_well_formed(frame.constructor_cbor_length),
      frame.constructor_memory >= 5,
      uint64_is_well_formed(frame.constructor_memory),
    }
  } else {
    and {
      frame.constructor == 0,
      frame.constructor_cbor_root == #"",
      frame.constructor_cbor_length == 0,
      frame.constructor_memory == 0,
    }
  }
}

fn empty_sequence_for(
  frame: DataFrameV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  if frame.kind == map_frame {
    cek_data_v1.empty_data_pair_summary_v1()
  } else {
    cek_data_v1.empty_data_list_summary_v1()
  }
}

pub fn frame_is_well_formed(frame: DataFrameV1) -> Bool {
  let maximum_fold_cursor = if frame.kind == map_frame {
    frame.expected_children / 2
  } else {
    frame.expected_children
  }
  let empty_sequence = empty_sequence_for(frame)
  and {
    frame.kind >= constr_small_frame,
    frame.kind <= map_frame,
    constructor_is_well_formed(frame),
    optional_hash_is_well_formed(frame.tail),
    uint32_is_well_formed(frame.expected_children),
    if frame.kind == map_frame {
      frame.expected_children % 2 == 0
    } else {
      True
    },
    uint32_is_well_formed(frame.child_count),
    frame.child_count <= frame.expected_children,
    validation_merkle_v1.frontier_is_well_formed(
      frame.child_count,
      frame.child_peaks,
    ),
    uint32_is_well_formed(frame.fold_cursor),
    frame.fold_cursor <= maximum_fold_cursor,
    bytearray.length(frame.sequence.root) == 32,
    uint32_is_well_formed(frame.sequence.length),
    frame.sequence.length == frame.fold_cursor,
    uint64_is_well_formed(frame.sequence.payload_cbor_length),
    uint64_is_well_formed(frame.sequence.memory),
    if frame.fold_cursor == 0 {
      frame.sequence == empty_sequence
    } else {
      frame.child_count == frame.expected_children
    },
  }
}

fn encode_sequence(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> ByteArray {
  encode_definite_array_header(4)
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.length))
    |> bytearray.concat(cbor.serialise(summary.payload_cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

pub fn encode_frame_v1(frame: DataFrameV1) -> ByteArray {
  expect frame_is_well_formed(frame)
  encode_definite_array_header(11)
    |> bytearray.concat(cbor.serialise(frame.kind))
    |> bytearray.concat(cbor.serialise(frame.constructor))
    |> bytearray.concat(encode_definite_bytes(frame.constructor_cbor_root))
    |> bytearray.concat(cbor.serialise(frame.constructor_cbor_length))
    |> bytearray.concat(cbor.serialise(frame.constructor_memory))
    |> bytearray.concat(encode_definite_bytes(frame.tail))
    |> bytearray.concat(cbor.serialise(frame.expected_children))
    |> bytearray.concat(cbor.serialise(frame.child_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(frame.child_peaks),
      )
    |> bytearray.concat(cbor.serialise(frame.fold_cursor))
    |> bytearray.concat(encode_sequence(frame.sequence))
}

pub fn hash_frame_v1(frame: DataFrameV1) -> ByteArray {
  frame_domain
    |> bytearray.concat(encode_frame_v1(frame))
    |> blake2b_256
}

fn summary_is_well_formed(
  summary: cek_data_v1.DataSummaryV1,
) -> Bool {
  and {
    bytearray.length(summary.root) == 32,
    summary.cbor_length > 0,
    uint64_is_well_formed(summary.cbor_length),
    summary.memory >= 4,
    uint64_is_well_formed(summary.memory),
  }
}

pub fn child_leaf_hash_v1(
  child_index: Int,
  child: cek_data_v1.DataSummaryV1,
) -> ByteArray {
  expect uint32_is_well_formed(child_index)
  expect summary_is_well_formed(child)
  child_domain
    |> bytearray.concat(encode_definite_array_header(4))
    |> bytearray.concat(cbor.serialise(child_index))
    |> bytearray.concat(encode_definite_bytes(child.root))
    |> bytearray.concat(cbor.serialise(child.cbor_length))
    |> bytearray.concat(cbor.serialise(child.memory))
    |> blake2b_256
}

fn initial_frame(
  kind: Int,
  constructor: Int,
  constructor_cbor_root: ByteArray,
  constructor_cbor_length: Int,
  constructor_memory: Int,
  tail: ByteArray,
  expected_children: Int,
  sequence: cek_data_v1.DataSequenceSummaryV1,
) -> DataFrameV1 {
  let frame = DataFrameV1 {
    kind,
    constructor,
    constructor_cbor_root,
    constructor_cbor_length,
    constructor_memory,
    tail,
    expected_children,
    child_count: 0,
    child_peaks: validation_merkle_v1.empty_frontier(),
    fold_cursor: 0,
    sequence,
  }
  expect frame_is_well_formed(frame)
  frame
}

pub fn initial_small_constr_frame_v1(
  constructor: Int,
  tail: ByteArray,
  expected_children: Int,
) -> DataFrameV1 {
  initial_frame(
    constr_small_frame,
    constructor,
    #"",
    0,
    0,
    tail,
    expected_children,
    cek_data_v1.empty_data_list_summary_v1(),
  )
}

pub fn initial_large_constr_frame_v1(
  constructor_cbor_root: ByteArray,
  constructor_cbor_length: Int,
  constructor_memory: Int,
  tail: ByteArray,
  expected_children: Int,
) -> DataFrameV1 {
  initial_frame(
    constr_large_frame,
    0,
    constructor_cbor_root,
    constructor_cbor_length,
    constructor_memory,
    tail,
    expected_children,
    cek_data_v1.empty_data_list_summary_v1(),
  )
}

pub fn initial_list_frame_v1(
  tail: ByteArray,
  expected_children: Int,
) -> DataFrameV1 {
  initial_frame(
    list_frame,
    0,
    #"",
    0,
    0,
    tail,
    expected_children,
    cek_data_v1.empty_data_list_summary_v1(),
  )
}

pub fn initial_map_frame_v1(
  tail: ByteArray,
  expected_children: Int,
) -> DataFrameV1 {
  initial_frame(
    map_frame,
    0,
    #"",
    0,
    0,
    tail,
    expected_children,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
}

pub fn append_child_v1(
  frame: DataFrameV1,
  child: cek_data_v1.DataSummaryV1,
) -> Option<DataFrameV1> {
  if and {
    frame_is_well_formed(frame),
    summary_is_well_formed(child),
    frame.fold_cursor == 0,
    frame.child_count < frame.expected_children,
  } {
    Some(
      DataFrameV1 {
        ..frame,
        child_count: frame.child_count + 1,
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
  frame: DataFrameV1,
  child_index: Int,
  child: cek_data_v1.DataSummaryV1,
  siblings: List<ByteArray>,
) -> Option<DataFrameV1> {
  let expected_index = frame.expected_children - frame.fold_cursor - 1
  if and {
    frame_is_well_formed(frame),
    frame.kind != map_frame,
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
      DataFrameV1 {
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
  frame: DataFrameV1,
  pair_index: Int,
  key: cek_data_v1.DataSummaryV1,
  value: cek_data_v1.DataSummaryV1,
  key_siblings: List<ByteArray>,
  value_siblings: List<ByteArray>,
) -> Option<DataFrameV1> {
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
      DataFrameV1 {
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
  frame: DataFrameV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  let expected_fold_cursor = if frame.kind == map_frame {
    frame.expected_children / 2
  } else {
    frame.expected_children
  }
  if !and {
    frame_is_well_formed(frame),
    frame.child_count == frame.expected_children,
    frame.fold_cursor == expected_fold_cursor,
  } {
    None
  } else if frame.kind == constr_small_frame {
    Some(
      cek_data_v1.small_constr_data_summary_v1(
        frame.constructor,
        frame.sequence,
      ),
    )
  } else if frame.kind == constr_large_frame {
    Some(
      cek_data_v1.large_constr_data_summary_from_cbor_v1(
        frame.constructor_cbor_root,
        frame.constructor_cbor_length,
        frame.constructor_memory,
        frame.sequence,
      ),
    )
  } else if frame.kind == list_frame {
    Some(cek_data_v1.list_data_summary_v1(frame.sequence))
  } else {
    expect frame.kind == map_frame
    Some(cek_data_v1.map_data_summary_v1(frame.sequence))
  }
}
