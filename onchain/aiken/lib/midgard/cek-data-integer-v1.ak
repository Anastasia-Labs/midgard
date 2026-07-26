use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/cek_source_blob_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, slice_len,
}

pub const version: Int = 1

pub const syntax_bytes: Int = 14

pub const stage_syntax: Int = 0

pub const stage_blob: Int = 1

pub const stage_terminal: Int = 2

const uint32_max = 4294967295

const uint64_max = 18446744073709551615

pub type CekDataIntegerControlV1 {
  version: Int,
  stage: Int,
  source_start: Int,
  source_length: Int,
  memory: Int,
  blob: Option<cek_source_blob_v1.CekSourceBlobControlV1>,
}

pub type CborArgumentV1 {
  major: Int,
  value: Int,
  next_offset: Int,
}

fn minimum(first: Int, second: Int) -> Int {
  if first < second {
    first
  } else {
    second
  }
}

fn argument_byte_count(additional: Int) -> Int {
  if additional == 24 {
    1
  } else if additional == 25 {
    2
  } else if additional == 26 {
    4
  } else if additional == 27 {
    8
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
  } else if additional == 27 {
    value > 4294967295
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

fn unsigned_byte_size(value: Int) -> Int {
  if value < 256 {
    1
  } else {
    1 + unsigned_byte_size(value / 256)
  }
}

pub fn parse_syntax_v1(
  bytes: ByteArray,
  source_length: Int,
) -> Option<Int> {
  if or {
    source_length < 1,
    source_length > uint32_max,
    bytearray.length(bytes) != minimum(source_length, syntax_bytes),
  } {
    None
  } else {
    let first = bytearray.at(bytes, 0)
    if first / 32 <= 1 {
      when read_canonical_argument(bytes, 0) is {
        None -> None
        Some(argument) ->
          if and {
            argument.major <= 1,
            argument.value <= uint64_max,
            argument.next_offset == source_length,
          } {
            Some(4 + unsigned_byte_size(argument.value * 2))
          } else {
            None
          }
      }
    } else if or {
      first == 194,
      first == 195,
    } {
      when read_canonical_argument(bytes, 1) is {
        None -> None
        Some(magnitude) ->
          if and {
            magnitude.major == 2,
            magnitude.value >= 9,
            magnitude.value <= uint32_max,
            magnitude.next_offset + magnitude.value == source_length,
            magnitude.next_offset < bytearray.length(bytes),
          } {
            let first_magnitude_byte =
              bytearray.at(bytes, magnitude.next_offset)
            if first_magnitude_byte == 0 {
              None
            } else {
              Some(
                4 + magnitude.value + if first_magnitude_byte >= 128 {
                  1
                } else {
                  0
                },
              )
            }
          } else {
            None
          }
      }
    } else {
      None
    }
  }
}

/// Restricts the streamed integer grammar to a canonical nonnegative
/// constructor alternative above 127. Positive bignums are accepted without
/// materializing their value.
pub fn parse_large_constructor_syntax_v1(
  bytes: ByteArray,
  source_length: Int,
) -> Option<Int> {
  when parse_syntax_v1(bytes, source_length) is {
    None -> None
    Some(memory) -> {
      let first = bytearray.at(bytes, 0)
      if first == 194 {
        Some(memory)
      } else {
        when read_canonical_argument(bytes, 0) is {
          None -> None
          Some(argument) ->
            if and {
              argument.major == 0,
              argument.value > 127,
              argument.next_offset == source_length,
            } {
              Some(memory)
            } else {
              None
            }
        }
      }
    }
  }
}

pub fn control_is_well_formed(
  control: CekDataIntegerControlV1,
) -> Bool {
  and {
    control.version == version,
    control.stage >= stage_syntax,
    control.stage <= stage_terminal,
    control.source_start >= 0,
    control.source_length >= 1,
    control.source_length <= uint32_max,
    control.memory >= 0,
    control.memory <= uint64_max,
    if control.stage == stage_syntax {
      and {
        control.memory == 0,
        control.blob == None,
      }
    } else {
      when control.blob is {
        None -> False
        Some(blob_control) ->
          and {
            control.memory >= 5,
            cek_source_blob_v1.control_is_well_formed(blob_control),
            blob_control.source_start == control.source_start,
            blob_control.source_length == control.source_length,
            if control.stage == stage_terminal {
              blob_control.stage == cek_source_blob_v1.stage_terminal
            } else {
              True
            },
          }
      }
    },
  }
}

pub fn initial_control_v1(
  source_start: Int,
  source_length: Int,
) -> CekDataIntegerControlV1 {
  let control = CekDataIntegerControlV1 {
    version,
    stage: stage_syntax,
    source_start,
    source_length,
    memory: 0,
    blob: None,
  }
  expect control_is_well_formed(control)
  control
}

fn encode_optional_blob(
  blob: Option<cek_source_blob_v1.CekSourceBlobControlV1>,
) -> ByteArray {
  when blob is {
    None -> #"d87a80"
    Some(value) ->
      cek_source_blob_v1.encode_control_v1(value)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

pub fn encode_control_v1(
  control: CekDataIntegerControlV1,
) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(6)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.source_start))
    |> bytearray.concat(cbor.serialise(control.source_length))
    |> bytearray.concat(cbor.serialise(control.memory))
    |> bytearray.concat(encode_optional_blob(control.blob))
}

fn optional_blob_from_data(
  data: Data,
) -> Option<cek_source_blob_v1.CekSourceBlobControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 0 {
    expect [value] = fields
    Some(cek_source_blob_v1.control_from_data_v1(value))
  } else {
    expect index == 1
    expect fields == []
    None
  }
}

pub fn control_from_data_v1(data: Data) -> CekDataIntegerControlV1 {
  expect
    [
      version_data, stage_data, source_start_data, source_length_data,
      memory_data, blob_data,
    ] = builtin.un_list_data(data)
  let control = CekDataIntegerControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    source_start: builtin.un_i_data(source_start_data),
    source_length: builtin.un_i_data(source_length_data),
    memory: builtin.un_i_data(memory_data),
    blob: optional_blob_from_data(blob_data),
  }
  expect control_is_well_formed(control)
  control
}

pub fn decode_control_v1(
  control_cbor: ByteArray,
) -> CekDataIntegerControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

pub fn next_source_span_v1(
  control: CekDataIntegerControlV1,
) -> Option<cek_source_blob_v1.CekSourceBlobSpanV1> {
  if !control_is_well_formed(control) {
    None
  } else if control.stage == stage_syntax {
    Some(
      cek_source_blob_v1.CekSourceBlobSpanV1 {
        absolute_start: control.source_start,
        length: minimum(control.source_length, syntax_bytes),
      },
    )
  } else if control.stage == stage_blob {
    expect Some(blob_control) = control.blob
    cek_source_blob_v1.next_source_span_v1(blob_control)
  } else {
    None
  }
}

fn step_syntax(
  control: CekDataIntegerControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataIntegerControlV1> {
  when source_bytes is {
    None -> None
    Some(bytes) -> {
      expect Some(span) = next_source_span_v1(control)
      if bytearray.length(bytes) != span.length {
        None
      } else {
        when parse_syntax_v1(bytes, control.source_length) is {
          None -> None
          Some(memory) -> {
            let next = CekDataIntegerControlV1 {
              ..control,
              stage: stage_blob,
              memory,
              blob: Some(
                cek_source_blob_v1.initial_control_v1(
                  control.source_start,
                  control.source_length,
                ),
              ),
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
  }
}

fn step_blob(
  control: CekDataIntegerControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataIntegerControlV1> {
  expect Some(blob_control) = control.blob
  if blob_control.stage == cek_source_blob_v1.stage_terminal {
    if source_bytes == None {
      let next = CekDataIntegerControlV1 {
        ..control,
        stage: stage_terminal,
      }
      if control_is_well_formed(next) {
        Some(next)
      } else {
        None
      }
    } else {
      None
    }
  } else {
    when cek_source_blob_v1.step_v1(blob_control, source_bytes) is {
      None -> None
      Some(blob) -> {
        let next = CekDataIntegerControlV1 { ..control, blob: Some(blob) }
        if control_is_well_formed(next) {
          Some(next)
        } else {
          None
        }
      }
    }
  }
}

pub fn step_v1(
  control: CekDataIntegerControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataIntegerControlV1> {
  if !control_is_well_formed(control) {
    None
  } else if control.stage == stage_syntax {
    step_syntax(control, source_bytes)
  } else if control.stage == stage_blob {
    step_blob(control, source_bytes)
  } else {
    None
  }
}

pub fn finalize_v1(
  control: CekDataIntegerControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
  } {
    expect Some(blob_control) = control.blob
    when cek_source_blob_v1.finalize_v1(blob_control) is {
      None -> None
      Some(cbor_root) -> {
        let node = cek_data_v1.IntegerData {
          cbor_root,
          cbor_length: control.source_length,
          memory: control.memory,
        }
        Some(
          cek_data_v1.DataSummaryV1 {
            root: cek_data_v1.hash_data_node_v1(node),
            cbor_length: control.source_length,
            memory: control.memory,
          },
        )
      }
    }
  } else {
    None
  }
}
