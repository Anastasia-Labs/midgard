use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1 as bounded
use midgard/cek_data_bytes_v1 as bytes_control
use midgard/cek_data_frame_v1 as frame
use midgard/cek_data_integer_v1 as integer_control
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_data_v1 as summary
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_v1 as proof

fn field(items: List<Data>, index: Int) -> Data {
  expect Some(value) = list.at(items, index)
  value
}

fn integer(items: List<Data>, index: Int) -> Int {
  builtin.un_i_data(field(items, index))
}

fn fold_control(control: raw.Frame) -> List<Data> {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  let items = builtin.un_list_data(data)
  expect list.length(items) == 10
  expect integer(items, 0) == 1
  expect integer(items, 1) == traverse.stage_fold
  expect integer(items, 2) >= 0 && integer(items, 2) <= 4294967295
  expect integer(items, 3) > 0 && integer(items, 3) <= 4294967295
  expect integer(items, 4) >= 0 && integer(items, 4) <= integer(items, 3)
  expect bytearray.length(builtin.un_b_data(field(items, 5))) == 32
  let none = builtin.constr_data(1, [])
  expect
    field(items, 6) == none && field(items, 7) == none && field(items, 8) == none && field(
      items,
      9,
    ) == none
  items
}

fn action_fields(witness: Data, expected: Int) -> List<Data> {
  expect Pair(3, [action, first, second]) = builtin.un_constr_data(witness)
  expect
    first == builtin.constr_data(1, []) && second == builtin.constr_data(1, [])
  let Pair(tag, fields) = builtin.un_constr_data(action)
  expect tag == expected
  fields
}

fn changed_root(
  control: raw.Frame,
  items: List<Data>,
  root: ByteArray,
) -> Option<stages.StepResult> {
  let next = list.indexed_map(
    items,
    fn(index, old) {
      if index == 5 {
        builtin.b_data(root)
      } else {
        old
      }
    },
  )
  Some(
    stages.Advanced {
      control: raw.replace(
        control,
        7,
        builtin.constr_data(0, [builtin.list_data(next)]),
      ),
    },
  )
}

pub fn fold_map(control: raw.Frame, witness: Data) -> Option<stages.StepResult> {
  let items = fold_control(control)
  expect [frame_data,
        index_data,
        key_data, value_data, key_path, value_path] = action_fields(witness, 7)
  expect current_frame: frame.DataFrameV1 = frame_data
  expect key: summary.DataSummaryV1 = key_data
  expect value: summary.DataSummaryV1 = value_data
  expect key_siblings: List<ByteArray> = key_path
  expect value_siblings: List<ByteArray> = value_path
  when
    traverse.prevalidated_fold_map_next_frame_root_v1(
      builtin.un_b_data(field(items, 5)),
      current_frame,
      builtin.un_i_data(index_data),
      key,
      value,
      key_siblings,
      value_siblings,
    )
  is {
    None -> None
    Some(root) -> changed_root(control, items, root)
  }
}

pub fn fold_list(control: raw.Frame, witness: Data) -> Option<stages.StepResult> {
  let items = fold_control(control)
  expect [frame_data, index_data, child_data, path] = action_fields(witness, 6)
  expect current_frame: frame.DataFrameV1 = frame_data
  expect child: summary.DataSummaryV1 = child_data
  expect siblings: List<ByteArray> = path
  when
    traverse.prevalidated_fold_list_next_frame_root(
      builtin.un_b_data(field(items, 5)),
      current_frame,
      builtin.un_i_data(index_data),
      child,
      siblings,
    )
  is {
    None -> None
    Some(root) -> changed_root(control, items, root)
  }
}

pub fn finalize_frame(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let items = fold_control(control)
  expect [frame_data, parent_data] = action_fields(witness, 8)
  expect current_frame: frame.DataFrameV1 = frame_data
  expect parent: Option<frame.DataFrameV1> = parent_data
  when
    traverse.prevalidated_finalize_frame_transition_v1(
      builtin.un_b_data(field(items, 5)),
      integer(items, 4),
      integer(items, 3),
      current_frame,
      parent,
    )
  is {
    None -> None
    Some(transition) -> {
      expect Some(result_data) = cbor.deserialise(
        traverse.encode_optional_summary_v1(transition.next_result),
      )
      let next = list.indexed_map(
        items,
        fn(index, old) {
          if index == 1 {
            builtin.i_data(transition.next_stage)
          } else if index == 5 {
            builtin.b_data(transition.next_frame_root)
          } else if index == 9 {
            result_data
          } else {
            old
          }
        },
      )
      Some(
        stages.Advanced {
          control: raw.replace(
            control,
            7,
            builtin.constr_data(0, [builtin.list_data(next)]),
          ),
        },
      )
    }
  }
}

fn typed_control(control: raw.Frame) -> traverse.DataTraverseControlV1 {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  traverse.control_from_data_v1(data)
}

fn next_control(
  control: raw.Frame,
  result: Option<traverse.DataTraverseControlV1>,
) -> Option<stages.StepResult> {
  when result is {
    None -> None
    Some(next) -> {
      expect Some(data) = cbor.deserialise(traverse.encode_control_v1(next))
      Some(
        stages.Advanced {
          control: raw.replace(control, 7, builtin.constr_data(0, [data])),
        },
      )
    }
  }
}

fn source(
  control: raw.Frame,
  current: traverse.DataTraverseControlV1,
  witness: Data,
) -> (Data, Option<ByteArray>) {
  expect Pair(3, [action,
        first_data, second_data]) = builtin.un_constr_data(witness)
  expect first: Option<bounded.ChunkProofV1> = first_data
  expect second: Option<bounded.ChunkProofV1> = second_data
  let bytes = when traverse.prevalidated_next_source_span(current) is {
    None -> {
      expect first == None && second == None
      None
    }
    Some(span) -> {
      expect Some(chunk_proof) = first
      expect Some(bytes) = proof.authenticated_output_span_for_output(
        raw.integer(control, 2),
        raw.integer(control, 3),
        raw.bytes(control, 4),
        span.absolute_start,
        span.length,
        proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof: second },
      )
      Some(bytes)
    }
  }
  (action, bytes)
}

pub fn head_scalar(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(control, current, witness)
  expect Pair(1, [item_length]) = builtin.un_constr_data(action)
  expect Some(bytes) = source_bytes
  next_control(
    control,
    traverse.prevalidated_head_scalar(
      current,
      bytes,
      builtin.un_i_data(item_length),
    ),
  )
}

pub fn head_sequence(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(control, current, witness)
  expect Pair(2, [expected_children]) = builtin.un_constr_data(action)
  expect Some(bytes) = source_bytes
  next_simple_control(
    control,
    traverse.prevalidated_head_sequence(
      current,
      bytes,
      builtin.un_i_data(expected_children),
    ),
  )
}

pub fn head_map(control: raw.Frame, witness: Data) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(control, current, witness)
  expect Pair(3, []) = builtin.un_constr_data(action)
  expect Some(bytes) = source_bytes
  next_simple_control(control, traverse.prevalidated_head_map(current, bytes))
}

pub fn head_large_constructor(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(control, current, witness)
  expect Pair(4, [constructor_length,
        expected_children]) = builtin.un_constr_data(action)
  expect Some(bytes) = source_bytes
  next_control(
    control,
    traverse.prevalidated_head_large_constructor(
      current,
      bytes,
      builtin.un_i_data(constructor_length),
      builtin.un_i_data(expected_children),
    ),
  )
}

fn head_control(control: raw.Frame) -> traverse.DataTraverseControlV1 {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  expect
      [
        version, stage, start, length, offset, root, pending, int_control,
        byte_control, result,
      ]
    = builtin.un_list_data(data)
  expect
    builtin.un_i_data(version) == 1 && builtin.un_i_data(stage) == traverse.stage_head
  let source_start = builtin.un_i_data(start)
  let source_length = builtin.un_i_data(length)
  let cursor = builtin.un_i_data(offset)
  let frame_root = builtin.un_b_data(root)
  expect source_start >= 0 && source_start <= 4294967295
  expect source_length > 0 && source_length <= 4294967295
  expect cursor >= 0 && cursor < source_length
  expect bytearray.length(frame_root) == 32 || frame_root == #"" && cursor == 0
  let none = builtin.constr_data(1, [])
  expect
    pending == none && int_control == none && byte_control == none && result == none
  traverse.DataTraverseControlV1 {
    version: 1,
    stage: traverse.stage_head,
    source_start,
    source_length,
    offset: cursor,
    frame_root,
    pending_large_expected_children: None,
    integer: None,
    bytes: None,
    result: None,
  }
}

fn head_source(
  control: raw.Frame,
  current: traverse.DataTraverseControlV1,
  witness: Data,
) -> (Data, Option<ByteArray>) {
  expect Pair(3, [action,
        first_data, second_data]) = builtin.un_constr_data(witness)
  expect Some(first): Option<bounded.ChunkProofV1> = first_data
  expect second: Option<bounded.ChunkProofV1> = second_data
  let remaining = current.source_length - current.offset
  let length = if remaining < traverse.head_bytes {
    remaining
  } else {
    traverse.head_bytes
  }
  expect Some(bytes) = proof.authenticated_output_span_for_output(
    raw.integer(control, 2),
    raw.integer(control, 3),
    raw.bytes(control, 4),
    current.source_start + current.offset,
    length,
    proof.LedgerOutputProofChunks {
      chunk_proof: first,
      next_chunk_proof: second,
    },
  )
  (action, Some(bytes))
}

fn next_simple_control(
  control: raw.Frame,
  result: Option<traverse.DataTraverseControlV1>,
) -> Option<stages.StepResult> {
  when result is {
    None -> None
    Some(next) -> {
      expect
        next.pending_large_expected_children == None && next.integer == None && next.bytes == None && next.result == None
      let none = builtin.constr_data(1, [])
      let data = builtin.list_data(
        [
          builtin.i_data(next.version),
          builtin.i_data(next.stage),
          builtin.i_data(next.source_start),
          builtin.i_data(next.source_length),
          builtin.i_data(next.offset),
          builtin.b_data(next.frame_root),
          none,
          none,
          none,
          none,
        ],
      )
      Some(
        stages.Advanced {
          control: raw.replace(control, 7, builtin.constr_data(0, [data])),
        },
      )
    }
  }
}

fn scalar_control(
  control: raw.Frame,
  kind: Int,
) -> traverse.DataTraverseControlV1 {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  expect
      [
        version, stage, start, length, offset, root, pending, int_data,
        byte_data, result,
      ]
    = builtin.un_list_data(data)
  expect builtin.un_i_data(version) == 1 && builtin.un_i_data(stage) == kind
  let source_start = builtin.un_i_data(start)
  let source_length = builtin.un_i_data(length)
  let cursor = builtin.un_i_data(offset)
  let frame_root = builtin.un_b_data(root)
  expect source_start >= 0 && source_start <= 4294967295
  expect source_length > 0 && source_length <= 4294967295
  expect cursor >= 0 && cursor <= source_length
  expect frame_root == #"" || bytearray.length(frame_root) == 32
  let none = builtin.constr_data(1, [])
  expect pending == none && result == none
  let (integer, bytes) = if kind == traverse.stage_integer {
    expect byte_data == none
    expect Pair(0, [inner]) = builtin.un_constr_data(int_data)
    let integer = integer_control.control_from_data_v1(inner)
    expect integer.source_start == source_start + cursor
    expect
      integer.source_start + integer.source_length <= source_start + source_length
    (Some(integer), None)
  } else {
    expect kind == traverse.stage_bytes
    expect int_data == none
    expect Pair(0, [inner]) = builtin.un_constr_data(byte_data)
    let bytes = bytes_control.control_from_data_v1(inner)
    expect bytes.source_start == source_start + cursor
    expect
      bytes.source_start + bytes.source_length <= source_start + source_length
    (None, Some(bytes))
  }
  traverse.DataTraverseControlV1 {
    version: 1,
    stage: kind,
    source_start,
    source_length,
    offset: cursor,
    frame_root,
    pending_large_expected_children: None,
    integer,
    bytes,
    result: None,
  }
}

fn completed_scalar(
  control: raw.Frame,
  result: Option<traverse.DataTraverseControlV1>,
) -> Option<stages.StepResult> {
  when result is {
    None -> None
    Some(next) -> {
      expect
        next.pending_large_expected_children == None && next.integer == None && next.bytes == None
      expect Some(result_data) = cbor.deserialise(
        traverse.encode_optional_summary_v1(next.result),
      )
      let none = builtin.constr_data(1, [])
      let data = builtin.list_data(
        [
          builtin.i_data(next.version),
          builtin.i_data(next.stage),
          builtin.i_data(next.source_start),
          builtin.i_data(next.source_length),
          builtin.i_data(next.offset),
          builtin.b_data(next.frame_root),
          none,
          none,
          none,
          result_data,
        ],
      )
      Some(
        stages.Advanced {
          control: raw.replace(control, 7, builtin.constr_data(0, [data])),
        },
      )
    }
  }
}

pub fn attach_integer(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let current = scalar_control(control, traverse.stage_integer)
  expect [parent_data] = action_fields(witness, 5)
  expect parent: Option<frame.DataFrameV1> = parent_data
  completed_scalar(
    control,
    traverse.prevalidated_attach_integer(current, parent),
  )
}

pub fn attach_bytes(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let current = scalar_control(control, traverse.stage_bytes)
  expect [parent_data] = action_fields(witness, 5)
  expect parent: Option<frame.DataFrameV1> = parent_data
  completed_scalar(control, traverse.prevalidated_attach_bytes(current, parent))
}

fn replace_scalar(
  control: raw.Frame,
  index: Int,
  next_cbor: ByteArray,
) -> Option<stages.StepResult> {
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  expect Some(next) = cbor.deserialise(next_cbor)
  let items = list.indexed_map(
    builtin.un_list_data(data),
    fn(i, old) {
      if i == index {
        builtin.constr_data(0, [next])
      } else {
        old
      }
    },
  )
  Some(
    stages.Advanced {
      control: raw.replace(
        control,
        7,
        builtin.constr_data(0, [builtin.list_data(items)]),
      ),
    },
  )
}

pub fn advance_integer(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let current = scalar_control(control, traverse.stage_integer)
  expect Some(integer) = current.integer
  expect integer.stage != integer_control.stage_terminal
  let (action, bytes) = source(control, current, witness)
  expect Pair(0, []) = builtin.un_constr_data(action)
  when integer_control.step_v1(integer, bytes) is {
    None -> None
    Some(next) -> {
      expect
        next.source_start == integer.source_start && next.source_length == integer.source_length
      replace_scalar(control, 7, integer_control.encode_control_v1(next))
    }
  }
}

pub fn advance_bytes(
  control: raw.Frame,
  witness: Data,
) -> Option<stages.StepResult> {
  let current = scalar_control(control, traverse.stage_bytes)
  expect Some(scalar) = current.bytes
  expect scalar.stage != bytes_control.stage_terminal
  let (action, bytes) = source(control, current, witness)
  expect Pair(0, []) = builtin.un_constr_data(action)
  when bytes_control.step_v1(scalar, bytes) is {
    None -> None
    Some(next) -> {
      expect
        next.source_start == scalar.source_start && next.source_length == scalar.source_length
      replace_scalar(control, 8, bytes_control.encode_control_v1(next))
    }
  }
}
