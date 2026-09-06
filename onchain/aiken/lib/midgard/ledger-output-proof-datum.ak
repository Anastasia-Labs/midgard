use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/blake2b_256_trace_v1 as blake256
use midgard/bounded_item_v1 as bounded
use midgard/cek_blob_frontier_v1 as blob_frontier
use midgard/cek_data_bytes_v1 as bytes_control
use midgard/cek_data_frame_v1 as frame
use midgard/cek_data_integer_v1 as integer_control
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_data_v1 as summary
use midgard/cek_source_blob_v1 as blob
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

/// Take the span a stage's control demands from the witness chunks, requiring
/// the dispatcher action to claim exactly this span (or its absence) so the
/// span attestation yield of the same transaction verifies the chunk merkle
/// membership the slicing here no longer re-checks.
fn source(
  control: raw.Frame,
  required: Option<blob.CekSourceBlobSpanV1>,
  witness: Data,
  claimed_span: Data,
) -> (Data, Option<ByteArray>) {
  expect Pair(3, [action,
        first_data, second_data]) = builtin.un_constr_data(witness)
  let none = builtin.constr_data(1, [])
  let bytes = when required is {
    None -> {
      expect stages.no_span_claim(claimed_span)
      expect first_data == none && second_data == none
      None
    }
    Some(span) -> {
      // A datum-traversal source span stays small (at most
      // `traverse.head_bytes` / `cek_data_bytes_v1.maximum_source_span`
      // bytes), so the claim carries the span content itself; the span
      // attestation yield of the same transaction verifies these exact bytes
      // are the authenticated output span at exactly this (start, length).
      expect Pair(2, [start_data,
            length_data, bytes_data]) = builtin.un_constr_data(claimed_span)
      expect
        builtin.un_i_data(start_data) == span.absolute_start && builtin.un_i_data(
          length_data,
        ) == span.length
      Some(builtin.un_b_data(bytes_data))
    }
  }
  (action, bytes)
}

pub fn head_scalar(
  control: raw.Frame,
  witness: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(
    control,
    current,
    witness,
    claimed_span,
  )
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
  claimed_span: Data,
) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(
    control,
    current,
    witness,
    claimed_span,
  )
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

pub fn head_map(
  control: raw.Frame,
  witness: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(
    control,
    current,
    witness,
    claimed_span,
  )
  expect Pair(3, []) = builtin.un_constr_data(action)
  expect Some(bytes) = source_bytes
  next_simple_control(control, traverse.prevalidated_head_map(current, bytes))
}

pub fn head_large_constructor(
  control: raw.Frame,
  witness: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  let current = head_control(control)
  let (action, source_bytes) = head_source(
    control,
    current,
    witness,
    claimed_span,
  )
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
  claimed_span: Data,
) -> (Data, Option<ByteArray>) {
  let remaining = current.source_length - current.offset
  let length = if remaining < traverse.head_bytes {
    remaining
  } else {
    traverse.head_bytes
  }
  source(
    control,
    Some(
      blob.CekSourceBlobSpanV1 {
        absolute_start: current.source_start + current.offset,
        length,
      },
    ),
    witness,
    claimed_span,
  )
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

/// The scalar sub-control frame, without either scalar codec. Splitting the
/// integer and bytes decoders apart keeps a yield from linking the codec of
/// the family it does not serve: linking both put the advance yields over the
/// script size limit (measured 19,786 / 20,152 raw before this split).
fn scalar_prefix(
  control: raw.Frame,
  kind: Int,
) -> (Int, Int, Int, ByteArray, Data) {
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
  let active = if kind == traverse.stage_integer {
    expect byte_data == none
    int_data
  } else {
    expect kind == traverse.stage_bytes
    expect int_data == none
    byte_data
  }
  expect Pair(0, [inner]) = builtin.un_constr_data(active)
  (source_start, source_length, cursor, frame_root, inner)
}

fn integer_scalar_control(control: raw.Frame) -> traverse.DataTraverseControlV1 {
  let (source_start, source_length, cursor, frame_root, inner) = scalar_prefix(
    control,
    traverse.stage_integer,
  )
  let integer = integer_control.control_from_data_v1(inner)
  expect integer.source_start == source_start + cursor
  expect
    integer.source_start + integer.source_length <= source_start + source_length
  traverse.DataTraverseControlV1 {
    version: 1,
    stage: traverse.stage_integer,
    source_start,
    source_length,
    offset: cursor,
    frame_root,
    pending_large_expected_children: None,
    integer: Some(integer),
    bytes: None,
    result: None,
  }
}

fn bytes_scalar_control(control: raw.Frame) -> traverse.DataTraverseControlV1 {
  let (source_start, source_length, cursor, frame_root, inner) = scalar_prefix(
    control,
    traverse.stage_bytes,
  )
  let bytes = bytes_control.control_from_data_v1(inner)
  expect bytes.source_start == source_start + cursor
  expect
    bytes.source_start + bytes.source_length <= source_start + source_length
  traverse.DataTraverseControlV1 {
    version: 1,
    stage: traverse.stage_bytes,
    source_start,
    source_length,
    offset: cursor,
    frame_root,
    pending_large_expected_children: None,
    integer: None,
    bytes: Some(bytes),
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

/// The scalar claim of the shared step-dispatcher action: `constr 0
/// [source_start, source_length, offset, frame_root, scalar]` where `scalar`
/// is the active family sub-control as Plutus data, or `constr 1 []` for the
/// stage roles that read no scalar sub-control. The matching scalar
/// attestation yield verifies the claim against the pinned control bytes
/// through `integer_scalar_claim_is_exact` / `bytes_scalar_claim_is_exact`;
/// the stage yield consumes the claimed values without re-decoding the
/// control's scalar sub-control.
fn claimed_scalar_fields(claimed: Data) -> (Int, Int, Int, ByteArray, Data) {
  expect Pair(0, [start_data, length_data, offset_data, root_data, scalar_data])
    = builtin.un_constr_data(claimed)
  (
    builtin.un_i_data(start_data),
    builtin.un_i_data(length_data),
    builtin.un_i_data(offset_data),
    builtin.un_b_data(root_data),
    scalar_data,
  )
}

/// The scalar payload of the claim alone, for the advance steps that leave
/// the traverse prefix untouched; the scalar attestation yield verifies the
/// whole claim, prefix included.
fn claimed_scalar_payload(claimed: Data) -> Data {
  expect Pair(0, [_start,
        _length, _offset, _root, scalar_data]) = builtin.un_constr_data(claimed)
  scalar_data
}

fn scalar_claim_data(
  current: traverse.DataTraverseControlV1,
  scalar_data: Data,
) -> Data {
  builtin.constr_data(
    0,
    [
      builtin.i_data(current.source_start),
      builtin.i_data(current.source_length),
      builtin.i_data(current.offset),
      builtin.b_data(current.frame_root),
      scalar_data,
    ],
  )
}

/// The integer-family scalar attestation: the pinned control's active scalar
/// sub-control (with every well-formedness and range clause of the monolithic
/// decode path) equals the claim the stage yield consumes.
pub fn integer_scalar_claim_is_exact(control: raw.Frame, claimed: Data) -> Bool {
  let current = integer_scalar_control(control)
  expect Some(scalar) = current.integer
  let scalar_data: Data = scalar
  claimed == scalar_claim_data(current, scalar_data)
}

/// The bytes-family scalar attestation; see `integer_scalar_claim_is_exact`.
pub fn bytes_scalar_claim_is_exact(control: raw.Frame, claimed: Data) -> Bool {
  let current = bytes_scalar_control(control)
  expect Some(scalar) = current.bytes
  let scalar_data: Data = scalar
  claimed == scalar_claim_data(current, scalar_data)
}

/// Unchecked from-data readers for the attested scalar claim. The scalar
/// attestation yield of the same transaction proves the claim equals the
/// exact, fully validated decode of the pinned control's active scalar
/// sub-control (`integer_scalar_claim_is_exact` / `bytes_scalar_claim_is_exact`
/// upcast the checked record verbatim), so the consuming stage yield rebuilds
/// the typed record with direct builtins instead of linking the generated
/// checked decoder.
fn unchecked_blake256(data: Data) -> blake256.Blake2b256TraceControlV1 {
  let Pair(_, fields) = builtin.un_constr_data(data)
  expect
      [
        version, stage, cursor, total_length, chaining_value, active_block,
        active_block_length, working_value, round,
      ]
    = fields
  blake256.Blake2b256TraceControlV1 {
    version: builtin.un_i_data(version),
    stage: builtin.un_i_data(stage),
    cursor: builtin.un_i_data(cursor),
    total_length: builtin.un_i_data(total_length),
    chaining_value: builtin.un_b_data(chaining_value),
    active_block: builtin.un_b_data(active_block),
    active_block_length: builtin.un_i_data(active_block_length),
    working_value: builtin.un_b_data(working_value),
    round: builtin.un_i_data(round),
  }
}

fn unchecked_peaks(
  items: List<Data>,
) -> List<blob_frontier.CekBlobFrontierPeakV1> {
  when items is {
    [] -> []
    [head, ..rest] -> {
      let Pair(_, fields) = builtin.un_constr_data(head)
      expect [height, root, byte_length] = fields
      [
        blob_frontier.CekBlobFrontierPeakV1 {
          height: builtin.un_i_data(height),
          root: builtin.un_b_data(root),
          byte_length: builtin.un_i_data(byte_length),
        },
        ..unchecked_peaks(rest)
      ]
    }
  }
}

fn unchecked_blob(data: Data) -> Option<blob.CekSourceBlobControlV1> {
  let Pair(tag, fields) = builtin.un_constr_data(data)
  if tag == 1 {
    None
  } else {
    expect [inner] = fields
    let Pair(_, blob_fields) = builtin.un_constr_data(inner)
    expect
        [
          version, stage, source_start, source_length, frontier_data,
          active_hash,
        ]
      = blob_fields
    let Pair(frontier_tag, frontier_fields) = builtin.un_constr_data(
      frontier_data,
    )
    expect frontier_tag == 0
    expect [count, byte_length, peaks] = frontier_fields
    let Pair(hash_tag, hash_fields) = builtin.un_constr_data(active_hash)
    Some(
      blob.CekSourceBlobControlV1 {
        version: builtin.un_i_data(version),
        stage: builtin.un_i_data(stage),
        source_start: builtin.un_i_data(source_start),
        source_length: builtin.un_i_data(source_length),
        frontier: blob_frontier.CekBlobFrontierV1 {
          count: builtin.un_i_data(count),
          byte_length: builtin.un_i_data(byte_length),
          peaks: unchecked_peaks(builtin.un_list_data(peaks)),
        },
        active_hash: if hash_tag == 1 {
          None
        } else {
          expect [hash_inner] = hash_fields
          Some(unchecked_blake256(hash_inner))
        },
      },
    )
  }
}

fn unchecked_integer_scalar(
  data: Data,
) -> integer_control.CekDataIntegerControlV1 {
  let Pair(_, fields) = builtin.un_constr_data(data)
  expect [version,
        stage, source_start, source_length, memory, blob_data] = fields
  integer_control.CekDataIntegerControlV1 {
    version: builtin.un_i_data(version),
    stage: builtin.un_i_data(stage),
    source_start: builtin.un_i_data(source_start),
    source_length: builtin.un_i_data(source_length),
    memory: builtin.un_i_data(memory),
    blob: unchecked_blob(blob_data),
  }
}

fn unchecked_bytes_scalar(data: Data) -> bytes_control.CekDataBytesControlV1 {
  let Pair(_, fields) = builtin.un_constr_data(data)
  expect [version, stage, source_start, source_length, bytes_length, blob_data]
    = fields
  bytes_control.CekDataBytesControlV1 {
    version: builtin.un_i_data(version),
    stage: builtin.un_i_data(stage),
    source_start: builtin.un_i_data(source_start),
    source_length: builtin.un_i_data(source_length),
    bytes_length: builtin.un_i_data(bytes_length),
    blob: unchecked_blob(blob_data),
  }
}

fn claimed_integer_traverse(
  claimed: Data,
) -> (traverse.DataTraverseControlV1, integer_control.CekDataIntegerControlV1) {
  let (source_start, source_length, offset, frame_root, scalar_data) = claimed_scalar_fields(
    claimed,
  )
  let scalar = unchecked_integer_scalar(scalar_data)
  (
    traverse.DataTraverseControlV1 {
      version: 1,
      stage: traverse.stage_integer,
      source_start,
      source_length,
      offset,
      frame_root,
      pending_large_expected_children: None,
      integer: Some(scalar),
      bytes: None,
      result: None,
    },
    scalar,
  )
}

fn claimed_bytes_traverse(
  claimed: Data,
) -> (traverse.DataTraverseControlV1, bytes_control.CekDataBytesControlV1) {
  let (source_start, source_length, offset, frame_root, scalar_data) = claimed_scalar_fields(
    claimed,
  )
  let scalar = unchecked_bytes_scalar(scalar_data)
  (
    traverse.DataTraverseControlV1 {
      version: 1,
      stage: traverse.stage_bytes,
      source_start,
      source_length,
      offset,
      frame_root,
      pending_large_expected_children: None,
      integer: None,
      bytes: Some(scalar),
      result: None,
    },
    scalar,
  )
}

pub fn attach_integer(
  control: raw.Frame,
  witness: Data,
  claimed_scalar: Data,
) -> Option<stages.StepResult> {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  let (current, _scalar) = claimed_integer_traverse(claimed_scalar)
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
  claimed_scalar: Data,
) -> Option<stages.StepResult> {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  let (current, _scalar) = claimed_bytes_traverse(claimed_scalar)
  expect [parent_data] = action_fields(witness, 5)
  expect parent: Option<frame.DataFrameV1> = parent_data
  completed_scalar(control, traverse.prevalidated_attach_bytes(current, parent))
}

fn replace_scalar(
  control: raw.Frame,
  index: Int,
  next: Data,
) -> Option<stages.StepResult> {
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
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
  claimed_scalar: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  let integer = unchecked_integer_scalar(claimed_scalar_payload(claimed_scalar))
  expect integer.stage != integer_control.stage_terminal
  let (action, bytes) = source(
    control,
    integer_control.prevalidated_next_source_span(integer),
    witness,
    claimed_span,
  )
  expect Pair(0, []) = builtin.un_constr_data(action)
  when integer_control.prevalidated_step_v1(integer, bytes) is {
    None -> None
    Some(next) -> {
      expect
        next.source_start == integer.source_start && next.source_length == integer.source_length
      replace_scalar(control, 7, integer_control.control_data_v1(next))
    }
  }
}

pub fn advance_bytes(
  control: raw.Frame,
  witness: Data,
  claimed_scalar: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  let scalar = unchecked_bytes_scalar(claimed_scalar_payload(claimed_scalar))
  expect scalar.stage != bytes_control.stage_terminal
  let (action, bytes) = source(
    control,
    bytes_control.prevalidated_next_source_span(scalar),
    witness,
    claimed_span,
  )
  expect Pair(0, []) = builtin.un_constr_data(action)
  when bytes_control.prevalidated_step_v1(scalar, bytes) is {
    None -> None
    Some(next) -> {
      expect
        next.source_start == scalar.source_start && next.source_length == scalar.source_length
      replace_scalar(control, 8, bytes_control.control_data_v1(next))
    }
  }
}

/// Validating decode of a large-stage traverse sub-control (stages 3 and 4).
/// The large transitions run the traverse machine's own control-shape checks
/// rather than a scalar claim: their controls carry states (pending children,
/// the streaming constructor blob) the scalar attestation yields deliberately
/// do not serve, so these yields re-state every clause of
/// `control_is_well_formed` for their stage locally — the shared prefix
/// ranges, the pending-children window, the absent bytes/result slots and
/// `nested_integer_fits` over the fully validated integer sub-control —
/// without linking the bytes codec the stage can never hold.
fn large_stage_control(
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
  expect Pair(0, [children_data]) = builtin.un_constr_data(pending)
  let children = builtin.un_i_data(children_data)
  expect children >= 0 && children <= 4294967295
  let none = builtin.constr_data(1, [])
  expect byte_data == none && result == none
  expect Pair(0, [inner]) = builtin.un_constr_data(int_data)
  let integer = integer_control.control_from_data_v1(inner)
  expect
    if kind == traverse.stage_large_constructor {
      and {
        integer.source_start == source_start + cursor,
        cursor + integer.source_length < source_length,
      }
    } else {
      and {
        integer.stage == integer_control.stage_terminal,
        integer.source_start + integer.source_length == source_start + cursor,
        cursor < source_length,
      }
    }
  expect integer.source_start >= source_start
  expect
    integer.source_start + integer.source_length <= source_start + source_length
  traverse.DataTraverseControlV1 {
    version: 1,
    stage: kind,
    source_start,
    source_length,
    offset: cursor,
    frame_root,
    pending_large_expected_children: Some(children),
    integer: Some(integer),
    bytes: None,
    result: None,
  }
}

/// A traverse transition that changes only the stage and offset items of the
/// sub-control, copying every other item byte-for-byte.
fn large_transition(
  control: raw.Frame,
  next_stage: Int,
  next_offset: Int,
) -> Option<stages.StepResult> {
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  let items = list.indexed_map(
    builtin.un_list_data(data),
    fn(index, old) {
      if index == 1 {
        builtin.i_data(next_stage)
      } else if index == 4 {
        builtin.i_data(next_offset)
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

/// One `NoAction` streaming step of a large constructor's tag-and-length
/// prefix (traverse stage 3). This restates
/// `cek_data_traverse_v1.prevalidated_advance_large_constructor` with the
/// direct splice successors: the streaming branch touches only the integer
/// slot (its `source_start`/`source_length` preservation is checked, so every
/// `nested_integer_fits` clause carries over from the validated pre control),
/// and the completion branch touches only the stage and offset (its
/// large-fields clauses — terminal integer, `offset < source_length` — follow
/// from the pre control's stage-3 clauses).
pub fn advance_large_constructor(
  control: raw.Frame,
  witness: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  let current = large_stage_control(control, traverse.stage_large_constructor)
  expect Some(integer) = current.integer
  if integer.stage == integer_control.stage_terminal {
    let (action, bytes) = source(control, None, witness, claimed_span)
    expect Pair(0, []) = builtin.un_constr_data(action)
    expect bytes == None
    large_transition(
      control,
      traverse.stage_large_fields,
      current.offset + integer.source_length,
    )
  } else {
    let (action, bytes) = source(
      control,
      integer_control.prevalidated_next_source_span(integer),
      witness,
      claimed_span,
    )
    expect Pair(0, []) = builtin.un_constr_data(action)
    let syntax_is_valid = if integer.stage == integer_control.stage_syntax {
      when bytes is {
        None -> False
        Some(window) ->
          integer_control.parse_large_constructor_syntax_v1(
            window,
            integer.source_length,
          ) != None
      }
    } else {
      True
    }
    if !syntax_is_valid {
      None
    } else {
      when integer_control.prevalidated_step_v1(integer, bytes) is {
        None -> None
        Some(next) -> {
          expect
            next.source_start == integer.source_start && next.source_length == integer.source_length
          replace_scalar(control, 7, integer_control.control_data_v1(next))
        }
      }
    }
  }
}

/// The `NoAction` completion of a streamed large constructor: its sequence
/// header byte opens the field frame (traverse stage 4). This restates
/// `cek_data_traverse_v1.prevalidated_advance_large_fields` with the direct
/// simple-control successor; the one successor clause that does not follow
/// from the pre control's stage-4 clauses — a head successor must still have
/// a byte to read — is restated explicitly.
pub fn advance_large_fields(
  control: raw.Frame,
  witness: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  let current = large_stage_control(control, traverse.stage_large_fields)
  expect Some(integer) = current.integer
  expect Some(children) = current.pending_large_expected_children
  expect Some(active_blob) = integer.blob
  let (action, bytes) = source(
    control,
    Some(
      blob.CekSourceBlobSpanV1 {
        absolute_start: current.source_start + current.offset,
        length: 1,
      },
    ),
    witness,
    claimed_span,
  )
  expect Pair(0, []) = builtin.un_constr_data(action)
  let sequence_header = if children == 0 {
    128
  } else {
    159
  }
  when Pair(bytes, blob.finalize_v1(active_blob)) is {
    Pair(Some(window), Some(constructor_cbor_root)) ->
      if bytearray.at(window, 0) == sequence_header {
        let opened = frame.initial_large_constr_frame_v1(
          constructor_cbor_root,
          integer.source_length,
          integer.memory,
          current.frame_root,
          children,
        )
        let next_stage = if children == 0 {
          traverse.stage_fold
        } else {
          traverse.stage_head
        }
        if next_stage == traverse.stage_head && current.offset + 1 >= current.source_length {
          None
        } else {
          next_simple_control(
            control,
            Some(
              traverse.DataTraverseControlV1 {
                ..current,
                stage: next_stage,
                offset: current.offset + 1,
                frame_root: frame.hash_frame_v1(opened),
                pending_large_expected_children: None,
                integer: None,
              },
            ),
          )
        }
      } else {
        None
      }
    _ -> None
  }
}

/// Validating decode of a close-stage traverse sub-control: every clause of
/// `closed_stage_is_well_formed` for `stage_close`, with all four optional
/// slots absent, so no scalar codec is linked.
fn close_control(control: raw.Frame) -> traverse.DataTraverseControlV1 {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  expect
      [
        version, stage, start, length, offset, root, pending, int_control,
        byte_control, result,
      ]
    = builtin.un_list_data(data)
  expect
    builtin.un_i_data(version) == 1 && builtin.un_i_data(stage) == traverse.stage_close
  let source_start = builtin.un_i_data(start)
  let source_length = builtin.un_i_data(length)
  let cursor = builtin.un_i_data(offset)
  let frame_root = builtin.un_b_data(root)
  expect source_start >= 0 && source_start <= 4294967295
  expect source_length > 0 && source_length <= 4294967295
  expect cursor >= 0 && cursor < source_length
  expect bytearray.length(frame_root) == 32
  let none = builtin.constr_data(1, [])
  expect
    pending == none && int_control == none && byte_control == none && result == none
  traverse.DataTraverseControlV1 {
    version: 1,
    stage: traverse.stage_close,
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

/// The `NoAction` close step: the container's indefinite-length break byte
/// moves the traversal to the fold stage (traverse stage 5).
pub fn close(
  control: raw.Frame,
  witness: Data,
  claimed_span: Data,
) -> Option<stages.StepResult> {
  let current = close_control(control)
  let (action, bytes) = source(
    control,
    traverse.prevalidated_next_source_span(current),
    witness,
    claimed_span,
  )
  expect Pair(0, []) = builtin.un_constr_data(action)
  next_simple_control(
    control,
    traverse.prevalidated_close(current, bytes, traverse.NoAction),
  )
}

/// Scan item index of `reference_script_language` in the 23-item output scan
/// sub-control at LOP item 5.
const scan_reference_script_language_index: Int = 19

/// The datum-terminal hand-off (`LedgerOutputProofNoWitness` at the datum
/// traversal stage): the traversal is complete, so the step leaves the datum
/// stage for the reference-script commitment — or the terminal when the
/// output carries no reference script — touching only the LOP stage item,
/// exactly as `datum_traversal_step`'s terminal branch does.
pub fn finish(control: raw.Frame, witness: Data) -> Option<stages.StepResult> {
  expect raw.integer(control, 1) == proof.stage_datum_traversal
  expect Pair(0, [data]) = builtin.un_constr_data(raw.item(control, 7))
  expect integer(builtin.un_list_data(data), 1) == traverse.stage_terminal
  expect witness == builtin.constr_data(0, [])
  let scan_items = builtin.un_list_data(raw.item(control, 5))
  let next_stage = if integer(scan_items, scan_reference_script_language_index) == -1 {
    proof.stage_terminal
  } else {
    proof.stage_reference_script_commitment
  }
  Some(
    stages.Advanced {
      control: raw.replace(control, 1, builtin.i_data(next_stage)),
    },
  )
}
