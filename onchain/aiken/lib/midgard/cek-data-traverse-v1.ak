use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/cek_data_bytes_v1
use midgard/cek_data_frame_v1
use midgard/cek_data_integer_v1
use midgard/cek_data_v1
use midgard/cek_source_blob_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes, slice_len,
}

pub const version: Int = 1

pub const head_bytes: Int = 14

pub const max_source_span: Int = 132

pub const stage_head: Int = 0

pub const stage_integer: Int = 1

pub const stage_bytes: Int = 2

pub const stage_large_constructor: Int = 3

pub const stage_large_fields: Int = 4

pub const stage_close: Int = 5

pub const stage_fold: Int = 6

pub const stage_terminal: Int = 7

const control_domain =
  #"4d69646761726443656b446174615472617665727365436f6e74726f6c5631"

const uint32_max = 4_294_967_295

const uint64_max = 18_446_744_073_709_551_615

pub type DataTraverseControlV1 {
  version: Int,
  stage: Int,
  source_start: Int,
  source_length: Int,
  offset: Int,
  frame_root: ByteArray,
  pending_large_expected_children: Option<Int>,
  integer: Option<cek_data_integer_v1.CekDataIntegerControlV1>,
  bytes: Option<cek_data_bytes_v1.CekDataBytesControlV1>,
  result: Option<cek_data_v1.DataSummaryV1>,
}

pub type DataTraverseActionV1 {
  NoAction
  HeadScalar { item_length: Int }
  HeadSequence { expected_children: Int }
  HeadMap
  HeadLargeConstructor {
    constructor_cbor_length: Int,
    expected_children: Int,
  }
  AttachScalar { parent: Option<cek_data_frame_v1.DataFrameV1> }
  FoldList {
    frame: cek_data_frame_v1.DataFrameV1,
    child_index: Int,
    child: cek_data_v1.DataSummaryV1,
    siblings: List<ByteArray>,
  }
  FoldMap {
    frame: cek_data_frame_v1.DataFrameV1,
    pair_index: Int,
    key: cek_data_v1.DataSummaryV1,
    value: cek_data_v1.DataSummaryV1,
    key_siblings: List<ByteArray>,
    value_siblings: List<ByteArray>,
  }
  FinalizeFrame {
    frame: cek_data_frame_v1.DataFrameV1,
    parent: Option<cek_data_frame_v1.DataFrameV1>,
  }
}

pub type FinalizeFrameTransitionV1 {
  next_stage: Int,
  next_frame_root: ByteArray,
  next_result: Option<cek_data_v1.DataSummaryV1>,
}

pub type CborArgumentV1 {
  major: Int,
  value: Int,
  next_offset: Int,
}

pub type SmallConstructorHeadV1 {
  constructor: Int,
  prefix_length: Int,
}

fn minimum(first: Int, second: Int) -> Int {
  if first < second {
    first
  } else {
    second
  }
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

fn optional_summary_is_well_formed(
  summary: Option<cek_data_v1.DataSummaryV1>,
) -> Bool {
  when summary is {
    None -> True
    Some(exact) -> summary_is_well_formed(exact)
  }
}

fn optional_uint32_is_well_formed(value: Option<Int>) -> Bool {
  when value is {
    None -> True
    Some(exact) -> uint32_is_well_formed(exact)
  }
}

fn nested_integer_fits(
  control: DataTraverseControlV1,
  integer: cek_data_integer_v1.CekDataIntegerControlV1,
  starts_at_cursor: Bool,
) -> Bool {
  let absolute_cursor = control.source_start + control.offset
  and {
    cek_data_integer_v1.control_is_well_formed(integer),
    if starts_at_cursor {
      integer.source_start == absolute_cursor
    } else {
      integer.source_start + integer.source_length == absolute_cursor
    },
    integer.source_start >= control.source_start,
    integer.source_start + integer.source_length
      <= control.source_start + control.source_length,
  }
}

fn head_stage_is_well_formed(
  control: DataTraverseControlV1,
) -> Bool {
  and {
    control.offset < control.source_length,
    or {
      bytearray.length(control.frame_root) == 32,
      control.offset == 0,
    },
    control.pending_large_expected_children == None,
    control.integer == None,
    control.bytes == None,
    control.result == None,
  }
}

fn integer_stage_is_well_formed(
  control: DataTraverseControlV1,
) -> Bool {
  when control.integer is {
    None -> False
    Some(integer) ->
      and {
        control.pending_large_expected_children == None,
        control.bytes == None,
        control.result == None,
        nested_integer_fits(control, integer, True),
      }
  }
}

fn bytes_stage_is_well_formed(
  control: DataTraverseControlV1,
) -> Bool {
  when control.bytes is {
    None -> False
    Some(bytes_control) ->
      and {
        control.pending_large_expected_children == None,
        control.integer == None,
        control.result == None,
        cek_data_bytes_v1.control_is_well_formed(bytes_control),
        bytes_control.source_start == control.source_start + control.offset,
        control.offset + bytes_control.source_length <= control.source_length,
      }
  }
}

fn large_constructor_stage_is_well_formed(
  control: DataTraverseControlV1,
) -> Bool {
  when control.integer is {
    None -> False
    Some(integer) ->
      and {
        control.pending_large_expected_children != None,
        control.bytes == None,
        control.result == None,
        nested_integer_fits(control, integer, True),
        control.offset + integer.source_length < control.source_length,
      }
  }
}

fn large_fields_stage_is_well_formed(
  control: DataTraverseControlV1,
) -> Bool {
  when control.integer is {
    None -> False
    Some(integer) ->
      and {
        control.pending_large_expected_children != None,
        integer.stage == cek_data_integer_v1.stage_terminal,
        control.bytes == None,
        control.result == None,
        nested_integer_fits(control, integer, False),
        control.offset < control.source_length,
      }
  }
}

fn closed_stage_is_well_formed(
  control: DataTraverseControlV1,
) -> Bool {
  and {
    bytearray.length(control.frame_root) == 32,
    control.pending_large_expected_children == None,
    control.integer == None,
    control.bytes == None,
    control.result == None,
    if control.stage == stage_close {
      control.offset < control.source_length
    } else {
      True
    },
  }
}

fn terminal_stage_is_well_formed(
  control: DataTraverseControlV1,
) -> Bool {
  and {
    control.offset == control.source_length,
    control.frame_root == #"",
    control.pending_large_expected_children == None,
    control.integer == None,
    control.bytes == None,
    control.result != None,
  }
}

pub fn control_is_well_formed(control: DataTraverseControlV1) -> Bool {
  and {
    control.version == version,
    control.stage >= stage_head,
    control.stage <= stage_terminal,
    uint32_is_well_formed(control.source_start),
    control.source_length > 0,
    uint32_is_well_formed(control.source_length),
    uint32_is_well_formed(control.offset),
    control.offset <= control.source_length,
    optional_hash_is_well_formed(control.frame_root),
    optional_uint32_is_well_formed(
      control.pending_large_expected_children,
    ),
    optional_summary_is_well_formed(control.result),
    if control.stage == stage_head {
      head_stage_is_well_formed(control)
    } else if control.stage == stage_integer {
      integer_stage_is_well_formed(control)
    } else if control.stage == stage_bytes {
      bytes_stage_is_well_formed(control)
    } else if control.stage == stage_large_constructor {
      large_constructor_stage_is_well_formed(control)
    } else if control.stage == stage_large_fields {
      large_fields_stage_is_well_formed(control)
    } else if or {
      control.stage == stage_close,
      control.stage == stage_fold,
    } {
      closed_stage_is_well_formed(control)
    } else {
      terminal_stage_is_well_formed(control)
    },
  }
}

pub fn initial_control_v1(
  source_start: Int,
  source_length: Int,
) -> DataTraverseControlV1 {
  let control = DataTraverseControlV1 {
    version,
    stage: stage_head,
    source_start,
    source_length,
    offset: 0,
    frame_root: #"",
    pending_large_expected_children: None,
    integer: None,
    bytes: None,
    result: None,
  }
  expect control_is_well_formed(control)
  control
}

fn encode_optional_int(value: Option<Int>) -> ByteArray {
  when value is {
    None -> #"d87a80"
    Some(exact) ->
      cbor.serialise(exact)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn encode_optional_integer(
  value: Option<cek_data_integer_v1.CekDataIntegerControlV1>,
) -> ByteArray {
  when value is {
    None -> #"d87a80"
    Some(exact) ->
      cek_data_integer_v1.encode_control_v1(exact)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn encode_optional_bytes(
  value: Option<cek_data_bytes_v1.CekDataBytesControlV1>,
) -> ByteArray {
  when value is {
    None -> #"d87a80"
    Some(exact) ->
      cek_data_bytes_v1.encode_control_v1(exact)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn encode_summary(summary: cek_data_v1.DataSummaryV1) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

pub fn encode_optional_summary_v1(
  value: Option<cek_data_v1.DataSummaryV1>,
) -> ByteArray {
  expect optional_summary_is_well_formed(value)
  when value is {
    None -> #"d87a80"
    Some(exact) ->
      encode_summary(exact)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

pub fn encode_control_v1(control: DataTraverseControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(10)
    |> bytearray.concat(cbor.serialise(control.version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.source_start))
    |> bytearray.concat(cbor.serialise(control.source_length))
    |> bytearray.concat(cbor.serialise(control.offset))
    |> bytearray.concat(encode_definite_bytes(control.frame_root))
    |> bytearray.concat(
        encode_optional_int(control.pending_large_expected_children),
      )
    |> bytearray.concat(encode_optional_integer(control.integer))
    |> bytearray.concat(encode_optional_bytes(control.bytes))
    |> bytearray.concat(encode_optional_summary_v1(control.result))
}

fn optional_int_from_data(data: Data) -> Option<Int> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(builtin.un_i_data(value))
  }
}

fn optional_integer_from_data(
  data: Data,
) -> Option<cek_data_integer_v1.CekDataIntegerControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(cek_data_integer_v1.control_from_data_v1(value))
  }
}

fn optional_bytes_from_data(
  data: Data,
) -> Option<cek_data_bytes_v1.CekDataBytesControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(cek_data_bytes_v1.control_from_data_v1(value))
  }
}

fn optional_summary_from_data(
  data: Data,
) -> Option<cek_data_v1.DataSummaryV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    expect [root_data, cbor_length_data, memory_data] =
      builtin.un_list_data(value)
    Some(
      cek_data_v1.DataSummaryV1 {
        root: builtin.un_b_data(root_data),
        cbor_length: builtin.un_i_data(cbor_length_data),
        memory: builtin.un_i_data(memory_data),
      },
    )
  }
}

pub fn control_from_data_v1(data: Data) -> DataTraverseControlV1 {
  expect
    [
      version_data, stage_data, source_start_data, source_length_data,
      offset_data, frame_root_data, pending_large_data, integer_data,
      bytes_data, result_data,
    ] = builtin.un_list_data(data)
  let control = DataTraverseControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    source_start: builtin.un_i_data(source_start_data),
    source_length: builtin.un_i_data(source_length_data),
    offset: builtin.un_i_data(offset_data),
    frame_root: builtin.un_b_data(frame_root_data),
    pending_large_expected_children: optional_int_from_data(
      pending_large_data,
    ),
    integer: optional_integer_from_data(integer_data),
    bytes: optional_bytes_from_data(bytes_data),
    result: optional_summary_from_data(result_data),
  }
  expect control_is_well_formed(control)
  control
}

pub fn decode_control_v1(control_cbor: ByteArray) -> DataTraverseControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

pub fn hash_control_v1(control: DataTraverseControlV1) -> ByteArray {
  control_domain
    |> bytearray.concat(encode_control_v1(control))
    |> blake2b_256
}

pub fn next_source_span_v1(
  control: DataTraverseControlV1,
) -> Option<cek_source_blob_v1.CekSourceBlobSpanV1> {
  if !control_is_well_formed(control) {
    None
  } else if control.stage == stage_head {
    Some(
      cek_source_blob_v1.CekSourceBlobSpanV1 {
        absolute_start: control.source_start + control.offset,
        length: minimum(
          head_bytes,
          control.source_length - control.offset,
        ),
      },
    )
  } else if or {
    control.stage == stage_integer,
    control.stage == stage_large_constructor,
  } {
    expect Some(integer) = control.integer
    cek_data_integer_v1.next_source_span_v1(integer)
  } else if control.stage == stage_bytes {
    expect Some(bytes_control) = control.bytes
    cek_data_bytes_v1.next_source_span_v1(bytes_control)
  } else if or {
    control.stage == stage_large_fields,
    control.stage == stage_close,
  } {
    Some(
      cek_source_blob_v1.CekSourceBlobSpanV1 {
        absolute_start: control.source_start + control.offset,
        length: 1,
      },
    )
  } else {
    None
  }
}

fn argument_byte_count(additional: Int) -> Int {
  if additional == 24 {
    1
  } else if additional == 25 {
    2
  } else if additional == 26 {
    4
  } else {
    -1
  }
}

fn argument_is_minimal(additional: Int, value: Int) -> Bool {
  if additional == 24 {
    value >= 24
  } else if additional == 25 {
    value > 255
  } else if additional == 26 {
    value > 65535
  } else {
    False
  }
}

fn read_canonical_argument(
  bytes: ByteArray,
  offset: Int,
) -> Option<CborArgumentV1> {
  let length = bytearray.length(bytes)
  if or {
    offset < 0,
    offset >= length,
  } {
    None
  } else {
    let initial = bytearray.at(bytes, offset)
    let major = initial / 32
    let additional = initial % 32
    if additional < 24 {
      Some(
        CborArgumentV1 {
          major,
          value: additional,
          next_offset: offset + 1,
        },
      )
    } else {
      let byte_count = argument_byte_count(additional)
      if or {
        byte_count < 0,
        offset + 1 + byte_count > length,
      } {
        None
      } else {
        let value =
          slice_len(bytes, offset + 1, byte_count)
            |> builtin.bytearray_to_integer(True, _)
        if argument_is_minimal(additional, value) {
          Some(
            CborArgumentV1 {
              major,
              value,
              next_offset: offset + 1 + byte_count,
            },
          )
        } else {
          None
        }
      }
    }
  }
}

fn parse_small_constructor_head(
  bytes: ByteArray,
) -> Option<SmallConstructorHeadV1> {
  let length = bytearray.length(bytes)
  if and {
    length >= 2,
    bytearray.at(bytes, 0) == 216,
    bytearray.at(bytes, 1) >= 121,
    bytearray.at(bytes, 1) <= 127,
  } {
    Some(
      SmallConstructorHeadV1 {
        constructor: bytearray.at(bytes, 1) - 121,
        prefix_length: 2,
      },
    )
  } else if and {
    length >= 3,
    bytearray.at(bytes, 0) == 217,
  } {
    let tag = bytearray.at(bytes, 1) * 256 + bytearray.at(bytes, 2)
    let constructor = tag - 1280 + 7
    if and {
      tag >= 1280,
      tag <= 1400,
      constructor <= 127,
    } {
      Some(SmallConstructorHeadV1 { constructor, prefix_length: 3 })
    } else {
      None
    }
  } else {
    None
  }
}

fn exact_source_bytes(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<ByteArray> {
  when source_bytes is {
    None -> None
    Some(bytes) ->
      when next_source_span_v1(control) is {
        None -> None
        Some(span) ->
          if bytearray.length(bytes) == span.length {
            Some(bytes)
          } else {
            None
          }
      }
  }
}

fn checked_control(
  control: DataTraverseControlV1,
) -> Option<DataTraverseControlV1> {
  if control_is_well_formed(control) {
    Some(control)
  } else {
    None
  }
}

fn next_parent_stage(frame: cek_data_frame_v1.DataFrameV1) -> Int {
  if frame.child_count < frame.expected_children {
    stage_head
  } else if frame.kind == cek_data_frame_v1.map_frame {
    stage_fold
  } else {
    stage_close
  }
}

fn prevalidated_attach_summary_transition_v1(
  current_frame_root: ByteArray,
  summary: cek_data_v1.DataSummaryV1,
  parent: Option<cek_data_frame_v1.DataFrameV1>,
  offset: Int,
  source_length: Int,
) -> Option<FinalizeFrameTransitionV1> {
  if !summary_is_well_formed(summary) {
    None
  } else if current_frame_root == #"" {
    if and {
      parent == None,
      offset == source_length,
    } {
      Some(
        FinalizeFrameTransitionV1 {
          next_stage: stage_terminal,
          next_frame_root: #"",
          next_result: Some(summary),
        },
      )
    } else {
      None
    }
  } else {
    when parent is {
      None -> None
      Some(parent_frame) ->
        if and {
          cek_data_frame_v1.frame_is_well_formed(parent_frame),
          cek_data_frame_v1.hash_frame_v1(parent_frame) == current_frame_root,
        } {
          when cek_data_frame_v1.append_child_v1(parent_frame, summary) is {
            None -> None
            Some(next_parent) -> {
              let next_stage = next_parent_stage(next_parent)
              if and {
                if or {
                  next_stage == stage_head,
                  next_stage == stage_close,
                } {
                  offset < source_length
                } else {
                  True
                },
                or {
                  next_stage == stage_head,
                  next_stage == stage_close,
                  next_stage == stage_fold,
                },
              } {
                Some(
                  FinalizeFrameTransitionV1 {
                    next_stage,
                    next_frame_root: cek_data_frame_v1.hash_frame_v1(
                      next_parent,
                    ),
                    next_result: None,
                  },
                )
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
}

fn attach_summary(
  control: DataTraverseControlV1,
  summary: cek_data_v1.DataSummaryV1,
  parent: Option<cek_data_frame_v1.DataFrameV1>,
  offset: Int,
) -> Option<DataTraverseControlV1> {
  when
    prevalidated_attach_summary_transition_v1(
      control.frame_root,
      summary,
      parent,
      offset,
      control.source_length,
    )
  is {
    None -> None
    Some(transition) ->
      checked_control(
        DataTraverseControlV1 {
          ..control,
          stage: transition.next_stage,
          offset,
          frame_root: transition.next_frame_root,
          pending_large_expected_children: None,
          integer: None,
          bytes: None,
          result: transition.next_result,
        },
      )
  }
}

fn step_head_scalar(
  control: DataTraverseControlV1,
  bytes: ByteArray,
  item_length: Int,
) -> Option<DataTraverseControlV1> {
  if or {
    item_length <= 0,
    !uint32_is_well_formed(item_length),
    control.offset + item_length > control.source_length,
  } {
    None
  } else {
    let first = bytearray.at(bytes, 0)
    if or {
      first / 32 <= 1,
      first == 194,
      first == 195,
    } {
      checked_control(
        DataTraverseControlV1 {
          ..control,
          stage: stage_integer,
          integer: Some(
            cek_data_integer_v1.initial_control_v1(
              control.source_start + control.offset,
              item_length,
            ),
          ),
        },
      )
    } else if first / 32 == 2 {
      checked_control(
        DataTraverseControlV1 {
          ..control,
          stage: stage_bytes,
          bytes: Some(
            cek_data_bytes_v1.initial_control_v1(
              control.source_start + control.offset,
              item_length,
            ),
          ),
        },
      )
    } else {
      None
    }
  }
}

fn opened_frame_control(
  control: DataTraverseControlV1,
  frame: cek_data_frame_v1.DataFrameV1,
  head_length: Int,
) -> Option<DataTraverseControlV1> {
  checked_control(
    DataTraverseControlV1 {
      ..control,
      stage: if frame.expected_children == 0 {
        stage_fold
      } else {
        stage_head
      },
      offset: control.offset + head_length,
      frame_root: cek_data_frame_v1.hash_frame_v1(frame),
    },
  )
}

fn step_head_sequence(
  control: DataTraverseControlV1,
  bytes: ByteArray,
  expected_children: Int,
) -> Option<DataTraverseControlV1> {
  if !uint32_is_well_formed(expected_children) {
    None
  } else {
    let sequence_header = if expected_children == 0 {
      128
    } else {
      159
    }
    when parse_small_constructor_head(bytes) is {
      Some(head) ->
        if and {
          head.prefix_length < bytearray.length(bytes),
          bytearray.at(bytes, head.prefix_length) == sequence_header,
        } {
          let frame = cek_data_frame_v1.initial_small_constr_frame_v1(
            head.constructor,
            control.frame_root,
            expected_children,
          )
          opened_frame_control(control, frame, head.prefix_length + 1)
        } else {
          None
        }
      None ->
        if bytearray.at(bytes, 0) == sequence_header {
          let frame = cek_data_frame_v1.initial_list_frame_v1(
            control.frame_root,
            expected_children,
          )
          opened_frame_control(control, frame, 1)
        } else {
          None
        }
    }
  }
}

fn step_head_map(
  control: DataTraverseControlV1,
  bytes: ByteArray,
) -> Option<DataTraverseControlV1> {
  when read_canonical_argument(bytes, 0) is {
    None -> None
    Some(argument) ->
      if and {
        argument.major == 5,
        argument.value <= uint32_max / 2,
      } {
        let frame = cek_data_frame_v1.initial_map_frame_v1(
          control.frame_root,
          argument.value * 2,
        )
        opened_frame_control(control, frame, argument.next_offset)
      } else {
        None
      }
  }
}

fn step_head_large_constructor(
  control: DataTraverseControlV1,
  bytes: ByteArray,
  constructor_cbor_length: Int,
  expected_children: Int,
) -> Option<DataTraverseControlV1> {
  if and {
    constructor_cbor_length > 0,
    uint32_is_well_formed(constructor_cbor_length),
    uint32_is_well_formed(expected_children),
    bytearray.length(bytes) >= 3,
    slice_len(bytes, 0, 3) == #"d86682",
    control.offset + 3 + constructor_cbor_length < control.source_length,
  } {
    let offset = control.offset + 3
    checked_control(
      DataTraverseControlV1 {
        ..control,
        stage: stage_large_constructor,
        offset,
        pending_large_expected_children: Some(expected_children),
        integer: Some(
          cek_data_integer_v1.initial_control_v1(
            control.source_start + offset,
            constructor_cbor_length,
          ),
        ),
      },
    )
  } else {
    None
  }
}

fn step_head(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  when exact_source_bytes(control, source_bytes) is {
    None -> None
    Some(bytes) ->
      when action is {
        HeadScalar { item_length } ->
          step_head_scalar(control, bytes, item_length)
        HeadSequence { expected_children } ->
          step_head_sequence(control, bytes, expected_children)
        HeadMap -> step_head_map(control, bytes)
        HeadLargeConstructor {
          constructor_cbor_length,
          expected_children,
        } ->
          step_head_large_constructor(
            control,
            bytes,
            constructor_cbor_length,
            expected_children,
          )
        _ -> None
      }
  }
}

fn step_integer(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  expect Some(integer) = control.integer
  if integer.stage == cek_data_integer_v1.stage_terminal {
    if source_bytes != None {
      None
    } else {
      when action is {
        AttachScalar { parent } ->
          when cek_data_integer_v1.finalize_v1(integer) is {
            None -> None
            Some(summary) ->
              attach_summary(
                control,
                summary,
                parent,
                control.offset + integer.source_length,
              )
          }
        _ -> None
      }
    }
  } else if action != NoAction {
    None
  } else {
    when cek_data_integer_v1.step_v1(integer, source_bytes) is {
      None -> None
      Some(next_integer) ->
        checked_control(
          DataTraverseControlV1 {
            ..control,
            integer: Some(next_integer),
          },
        )
    }
  }
}

fn step_bytes(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  expect Some(bytes_control) = control.bytes
  if bytes_control.stage == cek_data_bytes_v1.stage_terminal {
    if source_bytes != None {
      None
    } else {
      when action is {
        AttachScalar { parent } ->
          when cek_data_bytes_v1.finalize_v1(bytes_control) is {
            None -> None
            Some(summary) ->
              attach_summary(
                control,
                summary,
                parent,
                control.offset + bytes_control.source_length,
              )
          }
        _ -> None
      }
    }
  } else if action != NoAction {
    None
  } else {
    when cek_data_bytes_v1.step_v1(bytes_control, source_bytes) is {
      None -> None
      Some(next_bytes) ->
        checked_control(
          DataTraverseControlV1 {
            ..control,
            bytes: Some(next_bytes),
          },
        )
    }
  }
}

fn step_large_constructor(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  expect Some(integer) = control.integer
  if integer.stage == cek_data_integer_v1.stage_terminal {
    if and {
      source_bytes == None,
      action == NoAction,
    } {
      checked_control(
        DataTraverseControlV1 {
          ..control,
          stage: stage_large_fields,
          offset: control.offset + integer.source_length,
        },
      )
    } else {
      None
    }
  } else if action != NoAction {
    None
  } else {
    let syntax_is_valid =
      if integer.stage == cek_data_integer_v1.stage_syntax {
        when source_bytes is {
          None -> False
          Some(bytes) ->
            cek_data_integer_v1.parse_large_constructor_syntax_v1(
              bytes,
              integer.source_length,
            )
              != None
        }
      } else {
        True
      }
    if !syntax_is_valid {
      None
    } else {
      when cek_data_integer_v1.step_v1(integer, source_bytes) is {
        None -> None
        Some(next_integer) ->
          checked_control(
            DataTraverseControlV1 {
              ..control,
              integer: Some(next_integer),
            },
          )
      }
    }
  }
}

fn step_large_fields(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  if action != NoAction {
    None
  } else {
    expect Some(expected_children) =
      control.pending_large_expected_children
    expect Some(integer) = control.integer
    expect Some(blob) = integer.blob
    let sequence_header = if expected_children == 0 {
      128
    } else {
      159
    }
    when Pair(
      exact_source_bytes(control, source_bytes),
      cek_source_blob_v1.finalize_v1(blob),
    ) is {
      Pair(Some(bytes), Some(constructor_cbor_root)) ->
        if bytearray.at(bytes, 0) == sequence_header {
          let frame = cek_data_frame_v1.initial_large_constr_frame_v1(
            constructor_cbor_root,
            integer.source_length,
            integer.memory,
            control.frame_root,
            expected_children,
          )
          checked_control(
            DataTraverseControlV1 {
              ..control,
              stage: if expected_children == 0 {
                stage_fold
              } else {
                stage_head
              },
              offset: control.offset + 1,
              frame_root: cek_data_frame_v1.hash_frame_v1(frame),
              pending_large_expected_children: None,
              integer: None,
            },
          )
        } else {
          None
        }
      _ -> None
    }
  }
}

fn step_close(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  if action != NoAction {
    None
  } else {
    when exact_source_bytes(control, source_bytes) is {
      Some(bytes) ->
        if bytearray.at(bytes, 0) == 255 {
          checked_control(
            DataTraverseControlV1 {
              ..control,
              stage: stage_fold,
              offset: control.offset + 1,
            },
          )
        } else {
          None
        }
      None -> None
    }
  }
}

pub fn prevalidated_finalize_frame_transition_v1(
  current_frame_root: ByteArray,
  current_offset: Int,
  source_length: Int,
  frame: cek_data_frame_v1.DataFrameV1,
  parent: Option<cek_data_frame_v1.DataFrameV1>,
) -> Option<FinalizeFrameTransitionV1> {
  if and {
    cek_data_frame_v1.frame_is_well_formed(frame),
    cek_data_frame_v1.hash_frame_v1(frame) == current_frame_root,
  } {
    when cek_data_frame_v1.finalized_summary_v1(frame) is {
      None -> None
      Some(summary) ->
        if frame.tail == #"" {
          prevalidated_attach_summary_transition_v1(
            #"",
            summary,
            parent,
            current_offset,
            source_length,
          )
        } else {
          prevalidated_attach_summary_transition_v1(
            frame.tail,
            summary,
            parent,
            current_offset,
            source_length,
          )
        }
    }
  } else {
    None
  }
}

fn step_finalize_frame(
  control: DataTraverseControlV1,
  frame: cek_data_frame_v1.DataFrameV1,
  parent: Option<cek_data_frame_v1.DataFrameV1>,
) -> Option<DataTraverseControlV1> {
  when
    prevalidated_finalize_frame_transition_v1(
      control.frame_root,
      control.offset,
      control.source_length,
      frame,
      parent,
    )
  is {
    None -> None
    Some(transition) ->
      checked_control(
        DataTraverseControlV1 {
          ..control,
          stage: transition.next_stage,
          frame_root: transition.next_frame_root,
          pending_large_expected_children: None,
          integer: None,
          bytes: None,
          result: transition.next_result,
        },
      )
  }
}

pub fn prevalidated_fold_map_next_frame_root_v1(
  current_frame_root: ByteArray,
  frame: cek_data_frame_v1.DataFrameV1,
  pair_index: Int,
  key: cek_data_v1.DataSummaryV1,
  value: cek_data_v1.DataSummaryV1,
  key_siblings: List<ByteArray>,
  value_siblings: List<ByteArray>,
) -> Option<ByteArray> {
  if and {
    cek_data_frame_v1.frame_is_well_formed(frame),
    cek_data_frame_v1.hash_frame_v1(frame) == current_frame_root,
  } {
    when
      cek_data_frame_v1.fold_map_pair_v1(
        frame,
        pair_index,
        key,
        value,
        key_siblings,
        value_siblings,
      )
    is {
      None -> None
      Some(next_frame) -> Some(cek_data_frame_v1.hash_frame_v1(next_frame))
    }
  } else {
    None
  }
}

fn step_fold(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  if source_bytes != None {
    None
  } else {
    when action is {
      FoldList { frame, child_index, child, siblings } ->
        if and {
          cek_data_frame_v1.frame_is_well_formed(frame),
          cek_data_frame_v1.hash_frame_v1(frame) == control.frame_root,
        } {
          when
            cek_data_frame_v1.fold_list_child_v1(
              frame,
              child_index,
              child,
              siblings,
            )
          is {
            None -> None
            Some(next_frame) ->
              checked_control(
                DataTraverseControlV1 {
                  ..control,
                  frame_root: cek_data_frame_v1.hash_frame_v1(next_frame),
                },
              )
          }
        } else {
          None
        }
      FoldMap {
        frame,
        pair_index,
        key,
        value,
        key_siblings,
        value_siblings,
      } ->
        when
          prevalidated_fold_map_next_frame_root_v1(
            control.frame_root,
            frame,
            pair_index,
            key,
            value,
            key_siblings,
            value_siblings,
          )
        is {
          None -> None
          Some(next_frame_root) ->
            checked_control(
              DataTraverseControlV1 {
                ..control,
                frame_root: next_frame_root,
              },
            )
        }
      FinalizeFrame { frame, parent } ->
        step_finalize_frame(control, frame, parent)
      _ -> None
    }
  }
}

pub fn step_v1(
  control: DataTraverseControlV1,
  source_bytes: Option<ByteArray>,
  action: DataTraverseActionV1,
) -> Option<DataTraverseControlV1> {
  if !control_is_well_formed(control) {
    None
  } else if control.stage == stage_head {
    step_head(control, source_bytes, action)
  } else if control.stage == stage_integer {
    step_integer(control, source_bytes, action)
  } else if control.stage == stage_bytes {
    step_bytes(control, source_bytes, action)
  } else if control.stage == stage_large_constructor {
    step_large_constructor(control, source_bytes, action)
  } else if control.stage == stage_large_fields {
    step_large_fields(control, source_bytes, action)
  } else if control.stage == stage_close {
    step_close(control, source_bytes, action)
  } else if control.stage == stage_fold {
    step_fold(control, source_bytes, action)
  } else {
    None
  }
}

pub fn finalize_v1(
  control: DataTraverseControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
  } {
    control.result
  } else {
    None
  }
}
