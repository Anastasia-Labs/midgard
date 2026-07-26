use aiken/builtin
use aiken/primitive/bytearray
use midgard/cek_data_frame_v1
use midgard/cek_data_integer_v1
use midgard/cek_data_traverse_v1
use midgard/cek_source_blob_v1
use midgard/fraud_proofs/native_tx/codec.{slice_len}

const typescript_terminal_control_cbor =
  #"8a010711111140d87a80d87a80d87a80d8799f835820844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c441109ff"

fn source_for_control(
  control: cek_data_traverse_v1.DataTraverseControlV1,
  source: ByteArray,
  source_start: Int,
) -> Option<ByteArray> {
  when cek_data_traverse_v1.next_source_span_v1(control) is {
    None -> None
    Some(span) ->
      Some(
        slice_len(
          source,
          span.absolute_start - source_start,
          span.length,
        ),
      )
  }
}

fn finish_large_constructor(
  control: cek_data_traverse_v1.DataTraverseControlV1,
  source: ByteArray,
  source_start: Int,
) -> cek_data_traverse_v1.DataTraverseControlV1 {
  if control.stage == cek_data_traverse_v1.stage_large_fields {
    control
  } else {
    expect control.stage == cek_data_traverse_v1.stage_large_constructor
    expect Some(next) = cek_data_traverse_v1.step_v1(
      control,
      source_for_control(control, source, source_start),
      cek_data_traverse_v1.NoAction,
    )
    finish_large_constructor(next, source, source_start)
  }
}

fn finish_scalar(
  control: cek_data_traverse_v1.DataTraverseControlV1,
  source: ByteArray,
  source_start: Int,
) -> cek_data_traverse_v1.DataTraverseControlV1 {
  let terminal = if control.stage == cek_data_traverse_v1.stage_integer {
    expect Some(integer) = control.integer
    integer.stage == cek_data_integer_v1.stage_terminal
  } else {
    expect control.stage == cek_data_traverse_v1.stage_bytes
    expect Some(bytes_control) = control.bytes
    bytes_control.stage == 3
  }
  if terminal {
    control
  } else {
    expect Some(next) = cek_data_traverse_v1.step_v1(
      control,
      source_for_control(control, source, source_start),
      cek_data_traverse_v1.NoAction,
    )
    finish_scalar(next, source, source_start)
  }
}

test streams_large_constructor_before_fields_and_matches_typescript() {
  let source = #"d86682c2490100000000000000009f01ff"
  let initial = cek_data_traverse_v1.initial_control_v1(17, 17)
  expect Some(opened) = cek_data_traverse_v1.step_v1(
    initial,
    source_for_control(initial, source, 17),
    cek_data_traverse_v1.HeadLargeConstructor {
      constructor_cbor_length: 11,
      expected_children: 1,
    },
  )
  let large_fields = finish_large_constructor(opened, source, 17)
  expect Some(integer) = large_fields.integer
  expect Some(blob) = integer.blob
  expect Some(constructor_cbor_root) = cek_source_blob_v1.finalize_v1(blob)
  let initial_frame = cek_data_frame_v1.initial_large_constr_frame_v1(
    constructor_cbor_root,
    integer.source_length,
    integer.memory,
    #"",
    1,
  )
  expect Some(fields_opened) = cek_data_traverse_v1.step_v1(
    large_fields,
    source_for_control(large_fields, source, 17),
    cek_data_traverse_v1.NoAction,
  )
  expect Some(field_opened) = cek_data_traverse_v1.step_v1(
    fields_opened,
    source_for_control(fields_opened, source, 17),
    cek_data_traverse_v1.HeadScalar { item_length: 1 },
  )
  let field_terminal = finish_scalar(field_opened, source, 17)
  expect Some(field_integer) = field_terminal.integer
  expect Some(field_summary) = cek_data_integer_v1.finalize_v1(field_integer)
  expect Some(field_attached) = cek_data_traverse_v1.step_v1(
    field_terminal,
    None,
    cek_data_traverse_v1.AttachScalar { parent: Some(initial_frame) },
  )
  expect Some(full_frame) = cek_data_frame_v1.append_child_v1(
    initial_frame,
    field_summary,
  )
  expect Some(closed) = cek_data_traverse_v1.step_v1(
    field_attached,
    source_for_control(field_attached, source, 17),
    cek_data_traverse_v1.NoAction,
  )
  expect Some(folded) = cek_data_traverse_v1.step_v1(
    closed,
    None,
    cek_data_traverse_v1.FoldList {
      frame: full_frame,
      child_index: 0,
      child: field_summary,
      siblings: [],
    },
  )
  expect Some(folded_frame) = cek_data_frame_v1.fold_list_child_v1(
    full_frame,
    0,
    field_summary,
    [],
  )
  expect Some(terminal) = cek_data_traverse_v1.step_v1(
    folded,
    None,
    cek_data_traverse_v1.FinalizeFrame {
      frame: folded_frame,
      parent: None,
    },
  )
  expect Some(summary) = cek_data_traverse_v1.finalize_v1(terminal)
  and {
    summary.root
      == #"844cdd8ac8dc97d87e4ed149da121054504365b523034a804a12c014d55c2c44",
    summary.cbor_length == 17,
    summary.memory == 9,
    cek_data_traverse_v1.encode_control_v1(terminal)
      == typescript_terminal_control_cbor,
    cek_data_traverse_v1.hash_control_v1(terminal)
      == #"173ab9eb57665546414d5c286c55bc1fcd939a1784a7b800863e9970b82f6c16",
  }
}

test pops_an_authenticated_empty_child_container_into_parent() {
  let source = #"d8799f80ff"
  let initial = cek_data_traverse_v1.initial_control_v1(17, 5)
  let parent = cek_data_frame_v1.initial_small_constr_frame_v1(0, #"", 1)
  expect Some(parent_opened) = cek_data_traverse_v1.step_v1(
    initial,
    source_for_control(initial, source, 17),
    cek_data_traverse_v1.HeadSequence { expected_children: 1 },
  )
  let child = cek_data_frame_v1.initial_list_frame_v1(
    cek_data_frame_v1.hash_frame_v1(parent),
    0,
  )
  expect Some(child_opened) = cek_data_traverse_v1.step_v1(
    parent_opened,
    source_for_control(parent_opened, source, 17),
    cek_data_traverse_v1.HeadSequence { expected_children: 0 },
  )
  expect Some(child_summary) = cek_data_frame_v1.finalized_summary_v1(child)
  expect Some(child_attached) = cek_data_traverse_v1.step_v1(
    child_opened,
    None,
    cek_data_traverse_v1.FinalizeFrame {
      frame: child,
      parent: Some(parent),
    },
  )
  expect Some(full_parent) = cek_data_frame_v1.append_child_v1(
    parent,
    child_summary,
  )
  expect Some(closed) = cek_data_traverse_v1.step_v1(
    child_attached,
    source_for_control(child_attached, source, 17),
    cek_data_traverse_v1.NoAction,
  )
  expect Some(folded) = cek_data_traverse_v1.step_v1(
    closed,
    None,
    cek_data_traverse_v1.FoldList {
      frame: full_parent,
      child_index: 0,
      child: child_summary,
      siblings: [],
    },
  )
  expect Some(folded_parent) = cek_data_frame_v1.fold_list_child_v1(
    full_parent,
    0,
    child_summary,
    [],
  )
  expect Some(terminal) = cek_data_traverse_v1.step_v1(
    folded,
    None,
    cek_data_traverse_v1.FinalizeFrame {
      frame: folded_parent,
      parent: None,
    },
  )
  terminal.stage == cek_data_traverse_v1.stage_terminal
}

test maximum_delegated_source_window_is_132_bytes() {
  let source =
    #"5f5840"
      |> bytearray.concat(builtin.replicate_byte(64, 106))
      |> bytearray.concat(#"5840")
      |> bytearray.concat(builtin.replicate_byte(64, 106))
      |> bytearray.concat(#"ff")
  let initial = cek_data_traverse_v1.initial_control_v1(17, 134)
  expect Some(opened) = cek_data_traverse_v1.step_v1(
    initial,
    source_for_control(initial, source, 17),
    cek_data_traverse_v1.HeadScalar { item_length: 134 },
  )
  expect Some(parsed) = cek_data_traverse_v1.step_v1(
    opened,
    source_for_control(opened, source, 17),
    cek_data_traverse_v1.NoAction,
  )
  expect Some(span) = cek_data_traverse_v1.next_source_span_v1(
    parsed,
  )
  and {
    span.length == 109,
    span.length <= cek_data_traverse_v1.max_source_span,
  }
}

test malformed_counts_trailing_bytes_and_small_large_constructor_fail_closed() {
  let wrong_source = #"9f01ff"
  let initial = cek_data_traverse_v1.initial_control_v1(17, 3)
  expect Some(opened) = cek_data_traverse_v1.step_v1(
    initial,
    source_for_control(initial, wrong_source, 17),
    cek_data_traverse_v1.HeadSequence { expected_children: 2 },
  )
  expect Some(child_opened) = cek_data_traverse_v1.step_v1(
    opened,
    source_for_control(opened, wrong_source, 17),
    cek_data_traverse_v1.HeadScalar { item_length: 1 },
  )
  let child_terminal = finish_scalar(child_opened, wrong_source, 17)
  let parent = cek_data_frame_v1.initial_list_frame_v1(#"", 2)
  expect Some(child_integer) = child_terminal.integer
  expect Some(child_summary) = cek_data_integer_v1.finalize_v1(child_integer)
  expect Some(one_attached) = cek_data_traverse_v1.step_v1(
    child_terminal,
    None,
    cek_data_traverse_v1.AttachScalar { parent: Some(parent) },
  )
  let small_large_source = #"d86682187f80"
  let small_large = cek_data_traverse_v1.initial_control_v1(17, 6)
  expect Some(small_opened) = cek_data_traverse_v1.step_v1(
    small_large,
    source_for_control(small_large, small_large_source, 17),
    cek_data_traverse_v1.HeadLargeConstructor {
      constructor_cbor_length: 2,
      expected_children: 0,
    },
  )
  and {
    cek_data_traverse_v1.step_v1(
      one_attached,
      Some(#"ff"),
      cek_data_traverse_v1.HeadScalar { item_length: 1 },
    )
      == None,
    cek_data_traverse_v1.step_v1(
      small_opened,
      Some(#"187f"),
      cek_data_traverse_v1.NoAction,
    )
      == None,
    child_summary.cbor_length == 1,
  }
}
