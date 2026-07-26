use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/blake2b_256_trace_v1
use midgard/cek_blob_frontier_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, slice_len,
}
use midgard/validation_merkle_v1

pub const version: Int = 1

pub const stage_active: Int = 0

pub const stage_terminal: Int = 1

const blob_chunk_domain =
  #"4d69646761726443656b426c6f624368756e6b5631"

pub type CekSourceBlobControlV1 {
  version: Int,
  stage: Int,
  source_start: Int,
  source_length: Int,
  frontier: cek_blob_frontier_v1.CekBlobFrontierV1,
  active_hash: Option<blake2b_256_trace_v1.Blake2b256TraceControlV1>,
}

pub type CekSourceBlobSpanV1 {
  absolute_start: Int,
  length: Int,
}

fn minimum(first: Int, second: Int) -> Int {
  if first < second {
    first
  } else {
    second
  }
}

fn expected_chunk_count(source_length: Int) -> Int {
  if source_length == 0 {
    1
  } else {
    (
      source_length + cek_proof_v1.max_blob_chunk_bytes - 1
    ) / cek_proof_v1.max_blob_chunk_bytes
  }
}

fn expected_frontier_byte_length(
  source_length: Int,
  count: Int,
) -> Int {
  minimum(
    source_length,
    count * cek_proof_v1.max_blob_chunk_bytes,
  )
}

fn definite_bytes_header(length: Int) -> ByteArray {
  expect and {
    length >= 0,
    length <= cek_proof_v1.max_blob_chunk_bytes,
  }
  if length < 24 {
    bytearray.from_int_big_endian(64 + length, 1)
  } else if length <= 255 {
    #"58"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 1))
  } else {
    #"59"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 2))
  }
}

fn chunk_prefix(length: Int) -> ByteArray {
  blob_chunk_domain
    |> bytearray.concat(definite_bytes_header(length))
}

fn active_chunk_length(control: CekSourceBlobControlV1) -> Int {
  if control.source_length == 0 {
    0
  } else {
    minimum(
      cek_proof_v1.max_blob_chunk_bytes,
      control.source_length - control.frontier.byte_length,
    )
  }
}

fn initial_active_hash(
  chunk_length: Int,
) -> blake2b_256_trace_v1.Blake2b256TraceControlV1 {
  blake2b_256_trace_v1.initial_control_v1(
    bytearray.length(chunk_prefix(chunk_length)) + chunk_length,
  )
}

pub fn control_is_well_formed(
  control: CekSourceBlobControlV1,
) -> Bool {
  let chunk_count = expected_chunk_count(control.source_length)
  and {
    control.version == version,
    control.stage >= stage_active,
    control.stage <= stage_terminal,
    control.source_start >= 0,
    control.source_length >= 0,
    chunk_count <= validation_merkle_v1.maximum_leaf_count,
    cek_blob_frontier_v1.frontier_is_well_formed(control.frontier),
    control.frontier.count <= chunk_count,
    control.frontier.byte_length
      == expected_frontier_byte_length(
          control.source_length,
          control.frontier.count,
        ),
    if control.stage == stage_terminal {
      and {
        control.frontier.count == chunk_count,
        control.active_hash == None,
      }
    } else {
      when control.active_hash is {
        None -> False
        Some(hash_control) -> {
          let chunk_length = active_chunk_length(control)
          and {
            control.frontier.count < chunk_count,
            chunk_length >= 0,
            blake2b_256_trace_v1.control_is_well_formed(hash_control),
            hash_control.total_length
              == bytearray.length(chunk_prefix(chunk_length))
                + chunk_length,
          }
        }
      }
    },
  }
}

pub fn initial_control_v1(
  source_start: Int,
  source_length: Int,
) -> CekSourceBlobControlV1 {
  let control = CekSourceBlobControlV1 {
    version,
    stage: stage_active,
    source_start,
    source_length,
    frontier: cek_blob_frontier_v1.empty_frontier_v1(),
    active_hash: Some(
      initial_active_hash(
        minimum(source_length, cek_proof_v1.max_blob_chunk_bytes),
      ),
    ),
  }
  expect control_is_well_formed(control)
  control
}

fn encode_optional_active_hash(
  control: Option<blake2b_256_trace_v1.Blake2b256TraceControlV1>,
) -> ByteArray {
  when control is {
    None -> #"d87a80"
    Some(value) ->
      blake2b_256_trace_v1.encode_control_v1(value)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

pub fn encode_control_v1(
  control: CekSourceBlobControlV1,
) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(6)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.source_start))
    |> bytearray.concat(cbor.serialise(control.source_length))
    |> bytearray.concat(
        cek_blob_frontier_v1.encode_frontier_v1(control.frontier),
      )
    |> bytearray.concat(encode_optional_active_hash(control.active_hash))
}

fn peak_from_data(
  data: Data,
) -> cek_blob_frontier_v1.CekBlobFrontierPeakV1 {
  expect [height_data, root_data, byte_length_data] =
    builtin.un_list_data(data)
  cek_blob_frontier_v1.CekBlobFrontierPeakV1 {
    height: builtin.un_i_data(height_data),
    root: builtin.un_b_data(root_data),
    byte_length: builtin.un_i_data(byte_length_data),
  }
}

fn peaks_from_data(
  items: List<Data>,
) -> List<cek_blob_frontier_v1.CekBlobFrontierPeakV1> {
  when items is {
    [] -> []
    [item, ..rest] -> [peak_from_data(item), ..peaks_from_data(rest)]
  }
}

fn frontier_from_data(
  data: Data,
) -> cek_blob_frontier_v1.CekBlobFrontierV1 {
  expect
    [
      version_data, count_data, byte_length_data, peaks_data,
    ] = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == cek_blob_frontier_v1.version
  let frontier = cek_blob_frontier_v1.CekBlobFrontierV1 {
    count: builtin.un_i_data(count_data),
    byte_length: builtin.un_i_data(byte_length_data),
    peaks: builtin.un_list_data(peaks_data) |> peaks_from_data,
  }
  expect cek_blob_frontier_v1.frontier_is_well_formed(frontier)
  frontier
}

fn optional_active_hash_from_data(
  data: Data,
) -> Option<blake2b_256_trace_v1.Blake2b256TraceControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 0 {
    expect [value] = fields
    Some(blake2b_256_trace_v1.control_from_data_v1(value))
  } else {
    expect index == 1
    expect fields == []
    None
  }
}

pub fn control_from_data_v1(data: Data) -> CekSourceBlobControlV1 {
  expect
    [
      version_data, stage_data, source_start_data, source_length_data,
      frontier_data, active_hash_data,
    ] = builtin.un_list_data(data)
  let control = CekSourceBlobControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    source_start: builtin.un_i_data(source_start_data),
    source_length: builtin.un_i_data(source_length_data),
    frontier: frontier_from_data(frontier_data),
    active_hash: optional_active_hash_from_data(active_hash_data),
  }
  expect control_is_well_formed(control)
  control
}

pub fn decode_control_v1(
  control_cbor: ByteArray,
) -> CekSourceBlobControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

fn expected_block_length(
  hash_control: blake2b_256_trace_v1.Blake2b256TraceControlV1,
) -> Int {
  minimum(
    blake2b_256_trace_v1.block_bytes,
    hash_control.total_length - hash_control.cursor,
  )
}

pub fn next_source_span_v1(
  control: CekSourceBlobControlV1,
) -> Option<CekSourceBlobSpanV1> {
  if !control_is_well_formed(control) {
    None
  } else {
    when control.active_hash is {
      None -> None
      Some(hash_control) ->
        if and {
          control.stage == stage_active,
          hash_control.stage == blake2b_256_trace_v1.stage_ready,
        } {
          let prefix = chunk_prefix(active_chunk_length(control))
          let prefix_length = bytearray.length(prefix)
          let block_length = expected_block_length(hash_control)
          let prefix_start = minimum(hash_control.cursor, prefix_length)
          let prefix_end = minimum(
            hash_control.cursor + block_length,
            prefix_length,
          )
          Some(
            CekSourceBlobSpanV1 {
              absolute_start: control.source_start
                + control.frontier.byte_length
                + if hash_control.cursor > prefix_length {
                  hash_control.cursor - prefix_length
                } else {
                  0
                },
              length: block_length - ( prefix_end - prefix_start ),
            },
          )
        } else {
          None
        }
    }
  }
}

fn active_message_block(
  control: CekSourceBlobControlV1,
  source_bytes: ByteArray,
) -> Option<ByteArray> {
  expect Some(hash_control) = control.active_hash
  expect Some(span) = next_source_span_v1(control)
  if bytearray.length(source_bytes) != span.length {
    None
  } else {
    let prefix = chunk_prefix(active_chunk_length(control))
    let block_length = expected_block_length(hash_control)
    let prefix_length = bytearray.length(prefix)
    let prefix_start = minimum(hash_control.cursor, prefix_length)
    let prefix_end = minimum(
      hash_control.cursor + block_length,
      prefix_length,
    )
    Some(
      slice_len(prefix, prefix_start, prefix_end - prefix_start)
        |> bytearray.concat(source_bytes),
    )
  }
}

fn finish_active_chunk(
  control: CekSourceBlobControlV1,
) -> Option<CekSourceBlobControlV1> {
  expect Some(hash_control) = control.active_hash
  when blake2b_256_trace_v1.digest_v1(hash_control) is {
    None -> None
    Some(root) ->
      when
        cek_blob_frontier_v1.append_chunk_root_v1(
          control.frontier,
          root,
          active_chunk_length(control),
        )
      is {
        None -> None
        Some(frontier) -> {
          let terminal =
            frontier.count == expected_chunk_count(control.source_length)
          let next = CekSourceBlobControlV1 {
            ..control,
            stage: if terminal {
              stage_terminal
            } else {
              stage_active
            },
            frontier,
            active_hash: if terminal {
              None
            } else {
              Some(
                initial_active_hash(
                  minimum(
                    cek_proof_v1.max_blob_chunk_bytes,
                    control.source_length - frontier.byte_length,
                  ),
                ),
              )
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

fn advance_active_hash(
  control: CekSourceBlobControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<CekSourceBlobControlV1> {
  expect Some(hash_control) = control.active_hash
  let next_hash =
    if hash_control.stage == blake2b_256_trace_v1.stage_ready {
      when source_bytes is {
        None -> None
        Some(bytes) ->
          when active_message_block(control, bytes) is {
            None -> None
            Some(block) ->
              blake2b_256_trace_v1.step_v1(
                hash_control,
                Some(block),
              )
          }
      }
    } else if source_bytes == None {
      blake2b_256_trace_v1.step_v1(hash_control, None)
    } else {
      None
    }
  when next_hash is {
    None -> None
    Some(hash) -> {
      let next = CekSourceBlobControlV1 {
        ..control,
        active_hash: Some(hash),
      }
      if control_is_well_formed(next) {
        Some(next)
      } else {
        None
      }
    }
  }
}

pub fn step_v1(
  control: CekSourceBlobControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<CekSourceBlobControlV1> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_active,
  } {
    expect Some(hash_control) = control.active_hash
    if hash_control.stage == blake2b_256_trace_v1.stage_terminal {
      if source_bytes == None {
        finish_active_chunk(control)
      } else {
        None
      }
    } else {
      advance_active_hash(control, source_bytes)
    }
  } else {
    None
  }
}

pub fn finalize_v1(
  control: CekSourceBlobControlV1,
) -> Option<ByteArray> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
  } {
    cek_blob_frontier_v1.finalize_v1(control.frontier)
  } else {
    None
  }
}
