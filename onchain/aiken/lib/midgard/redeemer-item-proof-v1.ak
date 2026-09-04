use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/canonical_cbor_scan_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}

pub const version: Int = 1

pub const redeemer_field_index: Int = 8

pub const mode_descriptor: Int = 0

pub const mode_data: Int = 1

pub const stage_header: Int = 0

pub const stage_tail: Int = 1

pub const stage_data: Int = 2

pub const stage_terminal: Int = 3

pub const max_header_span: Int = 28

pub const max_tail_span: Int = 19

const control_domain =
  #"4d69646761726452656465656d65724974656d50726f6f66436f6e74726f6c5631"

pub type RedeemerItemDescriptorV1 {
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  purpose_tag: Int,
  pointer_index: Int,
  data_offset: Int,
  data_length: Int,
  execution_memory: Int,
  execution_steps: Int,
}

pub type RedeemerItemSourceSpanV1 {
  absolute_start: Int,
  length: Int,
}

pub type RedeemerItemProofControlV1 {
  version: Int,
  mode: Int,
  stage: Int,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  expected_purpose_tag: Int,
  expected_pointer_index: Int,
  purpose_tag: Int,
  pointer_index: Int,
  data_offset: Int,
  data_length: Int,
  execution_memory: Int,
  execution_steps: Int,
  traversal: Option<cek_data_traverse_v1.DataTraverseControlV1>,
}

pub type RedeemerItemProofActionV1 {
  RedeemerItemOpenHeader
  RedeemerItemOpenTail
  RedeemerItemTraverseData { action: cek_data_traverse_v1.DataTraverseActionV1 }
  RedeemerItemFinishData
}

pub type RedeemerItemProofWitnessV1 {
  action: RedeemerItemProofActionV1,
  chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
}

pub type RedeemerItemProofStepResultV1 {
  RedeemerItemProofAdvanced { control: RedeemerItemProofControlV1 }
  RedeemerItemProofInvalid
}

fn supported_purpose_tag(tag: Int) -> Bool {
  or {
    tag == 0,
    tag == 1,
    tag == 3,
    tag == 6,
  }
}

fn optional_traversal_is_well_formed(
  control: RedeemerItemProofControlV1,
) -> Bool {
  when control.traversal is {
    None -> False
    Some(traversal) -> and {
        cek_data_traverse_v1.control_is_well_formed(traversal),
        traversal.source_start == control.data_offset,
        traversal.source_length == control.data_length,
      }
  }
}

fn descriptor_fields_are_empty(control: RedeemerItemProofControlV1) -> Bool {
  and {
    control.purpose_tag == -1,
    control.pointer_index == -1,
    control.data_offset == 0,
    control.data_length == 0,
    control.execution_memory == -1,
    control.execution_steps == -1,
    control.traversal == None,
  }
}

fn opened_descriptor_is_well_formed(control: RedeemerItemProofControlV1) -> Bool {
  let tail_length = control.total_length - control.data_offset - control.data_length
  and {
    supported_purpose_tag(control.purpose_tag),
    control.pointer_index >= 0,
    control.data_offset > 0,
    control.data_length > 0,
    control.data_offset + control.data_length < control.total_length,
    tail_length > 0,
    tail_length <= max_tail_span,
    if control.expected_purpose_tag == -1 {
      True
    } else {
      and {
        control.purpose_tag == control.expected_purpose_tag,
        control.pointer_index == control.expected_pointer_index,
      }
    },
  }
}

pub fn stage_data_outer_fields_are_well_formed_v1(
  control: RedeemerItemProofControlV1,
) -> Bool {
  let expected_is_absent = and {
    control.expected_purpose_tag == -1,
    control.expected_pointer_index == -1,
  }
  let expected_is_present = and {
    supported_purpose_tag(control.expected_purpose_tag),
    control.expected_pointer_index >= 0,
  }
  let descriptor_is_open = opened_descriptor_is_well_formed(control)
  let ex_units_are_open = and {
    control.execution_memory >= 0,
    control.execution_steps >= 0,
  }
  and {
    control.version == version,
    control.mode == mode_data,
    control.stage == stage_data,
    control.item_index >= 0,
    control.item_count > control.item_index,
    control.total_length > 0,
    bytearray.length(control.item_commitment) == 32,
    expected_is_absent || expected_is_present,
    descriptor_is_open,
    ex_units_are_open,
    when control.traversal is {
      None -> False
      Some(traversal) -> and {
          traversal.source_start == control.data_offset,
          traversal.source_length == control.data_length,
        }
    },
  }
}

pub fn control_is_well_formed(control: RedeemerItemProofControlV1) -> Bool {
  let expected_is_absent = and {
    control.expected_purpose_tag == -1,
    control.expected_pointer_index == -1,
  }
  let expected_is_present = and {
    supported_purpose_tag(control.expected_purpose_tag),
    control.expected_pointer_index >= 0,
  }
  let descriptor_is_open = opened_descriptor_is_well_formed(control)
  let ex_units_are_open = and {
    control.execution_memory >= 0,
    control.execution_steps >= 0,
  }
  and {
    control.version == version,
    control.mode == mode_descriptor || control.mode == mode_data,
    control.stage >= stage_header,
    control.stage <= stage_terminal,
    control.item_index >= 0,
    control.item_count > control.item_index,
    control.total_length > 0,
    bytearray.length(control.item_commitment) == 32,
    expected_is_absent || expected_is_present,
    if control.stage == stage_header {
      descriptor_fields_are_empty(control)
    } else if control.stage == stage_tail {
      and {
        descriptor_is_open,
        control.execution_memory == -1,
        control.execution_steps == -1,
        control.traversal == None,
      }
    } else if control.stage == stage_data {
      and {
        stage_data_outer_fields_are_well_formed_v1(control),
        when control.traversal is {
          None -> False
          Some(traversal) ->
            cek_data_traverse_v1.control_is_well_formed(traversal)
        },
      }
    } else if control.mode == mode_descriptor {
      and {
        descriptor_is_open,
        ex_units_are_open,
        control.traversal == None,
      }
    } else {
      and {
        descriptor_is_open,
        ex_units_are_open,
        optional_traversal_is_well_formed(control),
        when control.traversal is {
          None -> False
          Some(traversal) -> and {
              traversal.stage == cek_data_traverse_v1.stage_terminal,
              cek_data_traverse_v1.finalize_v1(traversal) != None,
            }
        },
      }
    },
  }
}

pub fn initial_control_v1(
  mode: Int,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  expected_purpose_tag: Int,
  expected_pointer_index: Int,
) -> RedeemerItemProofControlV1 {
  let control = RedeemerItemProofControlV1 {
    version,
    mode,
    stage: stage_header,
    item_index,
    item_count,
    total_length,
    item_commitment,
    expected_purpose_tag,
    expected_pointer_index,
    purpose_tag: -1,
    pointer_index: -1,
    data_offset: 0,
    data_length: 0,
    execution_memory: -1,
    execution_steps: -1,
    traversal: None,
  }
  expect control_is_well_formed(control)
  control
}

fn encode_optional_traversal(
  traversal: Option<cek_data_traverse_v1.DataTraverseControlV1>,
) -> ByteArray {
  when traversal is {
    None -> #"d87a80"
    Some(control) ->
      cek_data_traverse_v1.encode_control_v1(control)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

pub fn encode_control_v1(control: RedeemerItemProofControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_control_prefix_v1(control)
    |> bytearray.concat(encode_optional_traversal(control.traversal))
}

fn encode_control_prefix_v1(control: RedeemerItemProofControlV1) -> ByteArray {
  encode_definite_array_header(16)
    |> bytearray.concat(cbor.serialise(control.version))
    |> bytearray.concat(cbor.serialise(control.mode))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.item_index))
    |> bytearray.concat(cbor.serialise(control.item_count))
    |> bytearray.concat(cbor.serialise(control.total_length))
    |> bytearray.concat(encode_definite_bytes(control.item_commitment))
    |> bytearray.concat(cbor.serialise(control.expected_purpose_tag))
    |> bytearray.concat(cbor.serialise(control.expected_pointer_index))
    |> bytearray.concat(cbor.serialise(control.purpose_tag))
    |> bytearray.concat(cbor.serialise(control.pointer_index))
    |> bytearray.concat(cbor.serialise(control.data_offset))
    |> bytearray.concat(cbor.serialise(control.data_length))
    |> bytearray.concat(cbor.serialise(control.execution_memory))
    |> bytearray.concat(cbor.serialise(control.execution_steps))
}

fn control_hash_prefix_v1(control: RedeemerItemProofControlV1) -> ByteArray {
  control_domain
    |> bytearray.concat(encode_control_prefix_v1(control))
}

pub fn stage_data_outer_control_prefix_v1(
  control: RedeemerItemProofControlV1,
) -> ByteArray {
  expect stage_data_outer_fields_are_well_formed_v1(control)
  encode_control_prefix_v1(control)
}

pub fn stage_data_some_traversal_hash_prefix_v1(
  control: RedeemerItemProofControlV1,
) -> ByteArray {
  expect stage_data_outer_fields_are_well_formed_v1(control)
  control_hash_prefix_v1(control)
    |> bytearray.concat(#"d8799f")
}

pub fn hash_stage_data_from_authenticated_prefix_v1(
  authenticated_hash_prefix_cbor: ByteArray,
  checked_next_traversal_control_cbor: ByteArray,
) -> ByteArray {
  authenticated_hash_prefix_cbor
    |> bytearray.concat(checked_next_traversal_control_cbor)
    |> bytearray.concat(#"ff")
    |> blake2b_256
}

pub fn hash_stage_data_outer_with_checked_traversal_v1(
  control: RedeemerItemProofControlV1,
  checked_traversal_control_cbor: ByteArray,
) -> ByteArray {
  hash_stage_data_from_authenticated_prefix_v1(
    stage_data_some_traversal_hash_prefix_v1(control),
    checked_traversal_control_cbor,
  )
}

fn optional_traversal_from_data(
  data: Data,
) -> Option<cek_data_traverse_v1.DataTraverseControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(cek_data_traverse_v1.control_from_data_v1(value))
  }
}

pub fn decode_control_v1(control_cbor: ByteArray) -> RedeemerItemProofControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        version_data, mode_data, stage_data, item_index_data, item_count_data,
        total_length_data, item_commitment_data, expected_purpose_tag_data,
        expected_pointer_index_data, purpose_tag_data, pointer_index_data,
        data_offset_data, data_length_data, execution_memory_data,
        execution_steps_data, traversal_data,
      ]
    = builtin.un_list_data(data)
  let control = RedeemerItemProofControlV1 {
    version: builtin.un_i_data(version_data),
    mode: builtin.un_i_data(mode_data),
    stage: builtin.un_i_data(stage_data),
    item_index: builtin.un_i_data(item_index_data),
    item_count: builtin.un_i_data(item_count_data),
    total_length: builtin.un_i_data(total_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    expected_purpose_tag: builtin.un_i_data(expected_purpose_tag_data),
    expected_pointer_index: builtin.un_i_data(expected_pointer_index_data),
    purpose_tag: builtin.un_i_data(purpose_tag_data),
    pointer_index: builtin.un_i_data(pointer_index_data),
    data_offset: builtin.un_i_data(data_offset_data),
    data_length: builtin.un_i_data(data_length_data),
    execution_memory: builtin.un_i_data(execution_memory_data),
    execution_steps: builtin.un_i_data(execution_steps_data),
    traversal: optional_traversal_from_data(traversal_data),
  }
  expect control_is_well_formed(control)
  expect encode_control_v1(control) == control_cbor
  control
}

pub fn hash_control_v1(control: RedeemerItemProofControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  control_hash_prefix_v1(control)
    |> bytearray.concat(encode_optional_traversal(control.traversal))
    |> blake2b_256
}

pub fn descriptor_v1(
  control: RedeemerItemProofControlV1,
) -> Option<RedeemerItemDescriptorV1> {
  let descriptor_is_available = or {
    and {
      control_is_well_formed(control),
      control.stage >= stage_data,
    },
    and {
      control_is_well_formed(control),
      control.stage == stage_terminal,
      control.mode == mode_descriptor,
    },
  }
  if descriptor_is_available {
    Some(
      RedeemerItemDescriptorV1 {
        item_index: control.item_index,
        item_count: control.item_count,
        total_length: control.total_length,
        item_commitment: control.item_commitment,
        purpose_tag: control.purpose_tag,
        pointer_index: control.pointer_index,
        data_offset: control.data_offset,
        data_length: control.data_length,
        execution_memory: control.execution_memory,
        execution_steps: control.execution_steps,
      },
    )
  } else {
    None
  }
}

pub fn finalize_v1(
  control: RedeemerItemProofControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if and {
    control_is_well_formed(control),
    control.mode == mode_data,
    control.stage == stage_terminal,
  } {
    when control.traversal is {
      None -> None
      Some(traversal) -> cek_data_traverse_v1.finalize_v1(traversal)
    }
  } else {
    None
  }
}

pub fn next_source_span_v1(
  control: RedeemerItemProofControlV1,
) -> Option<RedeemerItemSourceSpanV1> {
  let header_length = if control.total_length < max_header_span {
    control.total_length
  } else {
    max_header_span
  }
  if !control_is_well_formed(control) {
    None
  } else if control.stage == stage_header {
    Some(RedeemerItemSourceSpanV1 { absolute_start: 0, length: header_length })
  } else if control.stage == stage_tail {
    Some(
      RedeemerItemSourceSpanV1 {
        absolute_start: control.data_offset + control.data_length,
        length: control.total_length - control.data_offset - control.data_length,
      },
    )
  } else if control.stage == stage_data {
    when control.traversal is {
      None -> None
      Some(traversal) ->
        when cek_data_traverse_v1.next_source_span_v1(traversal) is {
          None -> None
          Some(span) ->
            Some(
              RedeemerItemSourceSpanV1 {
                absolute_start: span.absolute_start,
                length: span.length,
              },
            )
        }
    }
  } else {
    None
  }
}

fn chunk_proof_matches(
  control: RedeemerItemProofControlV1,
  proof: bounded_item_v1.ChunkProofV1,
  chunk_index: Int,
) -> Bool {
  and {
    proof.field_index == redeemer_field_index,
    proof.item_index == control.item_index,
    proof.total_length == control.total_length,
    proof.chunk_index == chunk_index,
    bounded_item_v1.verify_chunk(control.item_commitment, proof),
  }
}

fn authenticated_span(
  control: RedeemerItemProofControlV1,
  span: RedeemerItemSourceSpanV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<ByteArray> {
  if or {
    span.length <= 0,
    span.length > cek_data_traverse_v1.max_source_span,
    span.absolute_start < 0,
    span.absolute_start + span.length > control.total_length,
  } {
    None
  } else {
    let first_chunk_index = span.absolute_start / bounded_item_v1.chunk_bytes
    let last_chunk_index = ( span.absolute_start + span.length - 1 ) / bounded_item_v1.chunk_bytes
    let local_start = span.absolute_start - first_chunk_index * bounded_item_v1.chunk_bytes
    if or {
      last_chunk_index > first_chunk_index + 1,
      !chunk_proof_matches(control, chunk_proof, first_chunk_index),
    } {
      None
    } else if last_chunk_index == first_chunk_index {
      if next_chunk_proof == None {
        Some(
          builtin.slice_bytearray(local_start, span.length, chunk_proof.chunk),
        )
      } else {
        None
      }
    } else {
      when next_chunk_proof is {
        None -> None
        Some(next) ->
          if chunk_proof_matches(control, next, last_chunk_index) {
            Some(
              chunk_proof.chunk
                |> bytearray.concat(next.chunk)
                |> builtin.slice_bytearray(local_start, span.length, _),
            )
          } else {
            None
          }
      }
    }
  }
}

fn header_step(
  control: RedeemerItemProofControlV1,
  source_bytes: ByteArray,
) -> RedeemerItemProofStepResultV1 {
  when canonical_cbor_scan_v1.head_at_v1(source_bytes, 0, 4) is {
    None -> RedeemerItemProofInvalid
    Some(outer) ->
      if outer.value != 4 {
        RedeemerItemProofInvalid
      } else {
        when
          canonical_cbor_scan_v1.head_at_v1(source_bytes, outer.next_offset, 0)
        is {
          None -> RedeemerItemProofInvalid
          Some(purpose) ->
            when
              canonical_cbor_scan_v1.head_at_v1(
                source_bytes,
                purpose.next_offset,
                0,
              )
            is {
              None -> RedeemerItemProofInvalid
              Some(pointer) ->
                when
                  canonical_cbor_scan_v1.head_at_v1(
                    source_bytes,
                    pointer.next_offset,
                    2,
                  )
                is {
                  None -> RedeemerItemProofInvalid
                  Some(data) -> {
                    let next = RedeemerItemProofControlV1 {
                      ..control,
                      stage: stage_tail,
                      purpose_tag: purpose.value,
                      pointer_index: pointer.value,
                      data_offset: data.next_offset,
                      data_length: data.value,
                    }
                    if control_is_well_formed(next) {
                      RedeemerItemProofAdvanced { control: next }
                    } else {
                      RedeemerItemProofInvalid
                    }
                  }
                }
            }
        }
      }
  }
}

fn tail_step(
  control: RedeemerItemProofControlV1,
  source_bytes: ByteArray,
) -> RedeemerItemProofStepResultV1 {
  when canonical_cbor_scan_v1.head_at_v1(source_bytes, 0, 4) is {
    None -> RedeemerItemProofInvalid
    Some(outer) ->
      if outer.value != 2 {
        RedeemerItemProofInvalid
      } else {
        when
          canonical_cbor_scan_v1.head_at_v1(source_bytes, outer.next_offset, 0)
        is {
          None -> RedeemerItemProofInvalid
          Some(memory) ->
            when
              canonical_cbor_scan_v1.head_at_v1(
                source_bytes,
                memory.next_offset,
                0,
              )
            is {
              None -> RedeemerItemProofInvalid
              Some(steps) ->
                if steps.next_offset != bytearray.length(source_bytes) {
                  RedeemerItemProofInvalid
                } else {
                  let next = RedeemerItemProofControlV1 {
                    ..control,
                    stage: if control.mode == mode_data {
                      stage_data
                    } else {
                      stage_terminal
                    },
                    execution_memory: memory.value,
                    execution_steps: steps.value,
                    traversal: if control.mode == mode_data {
                      Some(
                        cek_data_traverse_v1.initial_control_v1(
                          control.data_offset,
                          control.data_length,
                        ),
                      )
                    } else {
                      None
                    },
                  }
                  if control_is_well_formed(next) {
                    RedeemerItemProofAdvanced { control: next }
                  } else {
                    RedeemerItemProofInvalid
                  }
                }
            }
        }
      }
  }
}

fn advanced(
  control: RedeemerItemProofControlV1,
) -> Option<RedeemerItemProofStepResultV1> {
  if control_is_well_formed(control) {
    Some(RedeemerItemProofAdvanced { control })
  } else {
    None
  }
}

fn apply_step(
  control: RedeemerItemProofControlV1,
  source_bytes: Option<ByteArray>,
  action: RedeemerItemProofActionV1,
) -> Option<RedeemerItemProofStepResultV1> {
  if control.stage == stage_header {
    when action is {
      RedeemerItemOpenHeader -> {
        expect Some(bytes) = source_bytes
        Some(header_step(control, bytes))
      }
      _ -> None
    }
  } else if control.stage == stage_tail {
    when action is {
      RedeemerItemOpenTail -> {
        expect Some(bytes) = source_bytes
        Some(tail_step(control, bytes))
      }
      _ -> None
    }
  } else if control.stage == stage_data {
    expect Some(traversal) = control.traversal
    if traversal.stage == cek_data_traverse_v1.stage_terminal {
      when action is {
        RedeemerItemFinishData ->
          if source_bytes == None {
            advanced(
              RedeemerItemProofControlV1 { ..control, stage: stage_terminal },
            )
          } else {
            None
          }
        _ -> None
      }
    } else {
      when action is {
        RedeemerItemTraverseData { action: traversal_action } ->
          when
            cek_data_traverse_v1.step_v1(
              traversal,
              source_bytes,
              traversal_action,
            )
          is {
            None -> None
            Some(next_traversal) ->
              advanced(
                RedeemerItemProofControlV1 {
                  ..control,
                  traversal: Some(next_traversal),
                },
              )
          }
        _ -> None
      }
    }
  } else {
    None
  }
}

pub fn step_v1(
  control: RedeemerItemProofControlV1,
  witness: RedeemerItemProofWitnessV1,
) -> Option<RedeemerItemProofStepResultV1> {
  if !control_is_well_formed(control) {
    None
  } else {
    let authenticated: Option<Option<ByteArray>> = when
      next_source_span_v1(control)
    is {
      None ->
        if and {
          witness.chunk_proof == None,
          witness.next_chunk_proof == None,
        } {
          Some(None)
        } else {
          None
        }
      Some(span) ->
        when witness.chunk_proof is {
          None -> None
          Some(first) ->
            when
              authenticated_span(control, span, first, witness.next_chunk_proof)
            is {
              None -> None
              Some(bytes) -> Some(Some(bytes))
            }
        }
    }
    when authenticated is {
      None -> None
      Some(source_bytes) -> apply_step(control, source_bytes, witness.action)
    }
  }
}
