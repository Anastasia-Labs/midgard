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

/// Measures canonical 64-byte magnitude chunks, one authenticated header per step.
pub const stage_measure: Int = 3

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
      Some(CborArgumentV1 { major, value: additional, next_offset: offset + 1 })
    } else {
      let byte_count = argument_byte_count(additional)
      if or {
        byte_count < 0,
        offset + 1 + byte_count > length,
      } {
        None
      } else {
        let value = slice_len(bytes, offset + 1, byte_count)
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

pub fn parse_syntax_v1(bytes: ByteArray, source_length: Int) -> Option<Int> {
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
      if bytearray.length(bytes) >= 5 && bytearray.at(bytes, 1) == 95 {
        when measured_magnitude_length(source_length - 1) is {
          Some(length) ->
            if and {
              length > 64,
              bytearray.at(bytes, 2) == 88,
              bytearray.at(bytes, 3) == 64,
              bytearray.at(bytes, 4) != 0,
            } {
              Some(
                4 + length + if bytearray.at(bytes, 4) >= 128 {
                  1
                } else {
                  0
                },
              )
            } else {
              None
            }
          None -> None
        }
      } else {
        when read_canonical_argument(bytes, 1) is {
          None -> None
          Some(magnitude) ->
            if and {
              magnitude.major == 2,
              magnitude.value >= 9,
              magnitude.value <= 64,
              magnitude.next_offset + magnitude.value == source_length,
              magnitude.next_offset < bytearray.length(bytes),
            } {
              let first_magnitude_byte = bytearray.at(
                bytes,
                magnitude.next_offset,
              )
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
      // parse_syntax_v1 already proved the complete canonical integer grammar.
      // Canonical positive bignums and uint16/32/64 exceed 127; uint8 must
      // inspect its one argument byte. All negative encodings are excluded.
      if or {
        first == 194,
        first >= 25 && first <= 27,
        first == 24 && bytearray.at(bytes, 1) >= 128,
      } {
        Some(memory)
      } else {
        None
      }
    }
  }
}

pub fn control_is_well_formed(control: CekDataIntegerControlV1) -> Bool {
  and {
    control.version == version,
    control.stage >= stage_syntax,
    control.stage <= stage_measure,
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
    } else if control.stage == stage_measure {
      and {
        control.blob == None,
        measured_memory_is_well_formed(control.source_length, control.memory),
      }
    } else {
      when control.blob is {
        None -> False
        Some(blob_control) -> and {
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

/// `source_length` covers the tag, opener and chunks measured so far.
/// The cursor itself fixes the chunk count and final partial-chunk width.
fn measured_magnitude_length(source_length: Int) -> Option<Int> {
  let encoded = source_length - 2
  let full = encoded / 66
  let remainder = encoded % 66
  if source_length < 68 {
    None
  } else if remainder == 0 {
    Some(full * 64)
  } else if remainder >= 2 && remainder <= 24 {
    Some(full * 64 + remainder - 1)
  } else if remainder >= 26 && remainder <= 65 {
    Some(full * 64 + remainder - 2)
  } else {
    None
  }
}

fn measured_memory_is_well_formed(source_length: Int, memory: Int) -> Bool {
  if source_length == 2 {
    memory == 0
  } else {
    when measured_magnitude_length(source_length) is {
      None -> False
      Some(length) -> memory == 4 + length || memory == 5 + length
    }
  }
}

pub fn initial_measure_control_v1(source_start: Int) -> CekDataIntegerControlV1 {
  let control = CekDataIntegerControlV1 {
    version,
    stage: stage_measure,
    source_start,
    source_length: 2,
    memory: 0,
    blob: None,
  }
  expect control_is_well_formed(control)
  control
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

pub fn encode_control_v1(control: CekDataIntegerControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(6)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.source_start))
    |> bytearray.concat(cbor.serialise(control.source_length))
    |> bytearray.concat(cbor.serialise(control.memory))
    |> bytearray.concat(encode_optional_blob(control.blob))
}

/// The Plutus data form of the wire control — structurally
/// `cbor.deserialise(encode_control_v1(control))` — built directly; see
/// `blake2b_256_trace_v1.control_data_v1`. Pinned by
/// `integer_control_data_matches_wire_encoding`.
pub fn control_data_v1(control: CekDataIntegerControlV1) -> Data {
  builtin.list_data(
    [
      builtin.i_data(version),
      builtin.i_data(control.stage),
      builtin.i_data(control.source_start),
      builtin.i_data(control.source_length),
      builtin.i_data(control.memory),
      when control.blob is {
        None -> builtin.constr_data(1, [])
        Some(value) ->
          builtin.constr_data(0, [cek_source_blob_v1.control_data_v1(value)])
      },
    ],
  )
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
      ]
    = builtin.un_list_data(data)
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

pub fn decode_control_v1(control_cbor: ByteArray) -> CekDataIntegerControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

pub fn next_source_span_v1(
  control: CekDataIntegerControlV1,
  source_end: Int,
) -> Option<cek_source_blob_v1.CekSourceBlobSpanV1> {
  if control_is_well_formed(control) {
    prevalidated_next_source_span(control, source_end)
  } else {
    None
  }
}

/// Same span selection after an earlier gate authenticated the complete
/// canonical control, including every nested scalar/blob/hash state.
pub fn prevalidated_next_source_span(
  control: CekDataIntegerControlV1,
  source_end: Int,
) -> Option<cek_source_blob_v1.CekSourceBlobSpanV1> {
  if control.stage == stage_measure {
    let absolute_start = control.source_start + control.source_length
    if absolute_start < source_end {
      Some(
        cek_source_blob_v1.CekSourceBlobSpanV1 {
          absolute_start,
          length: minimum(3, source_end - absolute_start),
        },
      )
    } else {
      None
    }
  } else if control.stage == stage_syntax {
    Some(
      cek_source_blob_v1.CekSourceBlobSpanV1 {
        absolute_start: control.source_start,
        length: minimum(control.source_length, syntax_bytes),
      },
    )
  } else if control.stage == stage_blob {
    expect Some(blob_control) = control.blob
    cek_source_blob_v1.prevalidated_next_source_span(blob_control)
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
      expect Some(span) = prevalidated_next_source_span(
        control,
        control.source_start + control.source_length,
      )
      if bytearray.length(bytes) != span.length {
        None
      } else {
        if and {
          bytearray.length(bytes) >= 2,
          bytearray.at(bytes, 0) == 194 || bytearray.at(bytes, 0) == 195,
          bytearray.at(bytes, 1) == 95,
        } {
          Some(
            CekDataIntegerControlV1 {
              version,
              stage: stage_measure,
              source_start: control.source_start,
              source_length: 2,
              memory: 0,
              blob: None,
            },
          )
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
              // The authenticated prefix is preserved; the parser bounds
              // memory, and the checked initial blob pins its source extent.
              Some(next)
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
      let next = CekDataIntegerControlV1 { ..control, stage: stage_terminal }
      // The pre-control already pins this terminal blob and its extent.
      Some(next)
    } else {
      None
    }
  } else {
    when cek_source_blob_v1.step_v1(blob_control, source_bytes) is {
      None -> None
      Some(blob) -> {
        let next = CekDataIntegerControlV1 { ..control, blob: Some(blob) }
        // The checked blob step preserves its extent and validates its
        // successor; every remaining integer field is unchanged.
        Some(next)
      }
    }
  }
}

/// First chunk is exactly 64 bytes with a nonzero leading magnitude.
/// Further full chunks may precede one partial chunk; only a break may follow
/// a partial chunk. No prover-supplied integer or length enters this machine.
fn step_measure(
  control: CekDataIntegerControlV1,
  source_end: Int,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataIntegerControlV1> {
  when
    Pair(source_bytes, prevalidated_next_source_span(control, source_end))
  is {
    Pair(Some(bytes), Some(span)) ->
      if bytearray.length(bytes) != span.length {
        None
      } else {
        let first = bytearray.at(bytes, 0)
        if first == 255 {
          when measured_magnitude_length(control.source_length) is {
            Some(length) ->
              if length > 64 {
                let source_length = control.source_length + 1
                Some(
                  CekDataIntegerControlV1 {
                    ..control,
                    stage: stage_blob,
                    source_length,
                    blob: Some(
                      cek_source_blob_v1.initial_control_v1(
                        control.source_start,
                        source_length,
                      ),
                    ),
                  },
                )
              } else {
                None
              }
            None -> None
          }
        } else if ( control.source_length - 2 ) % 66 != 0 {
          None
        } else {
          let header_length = if first >= 65 && first <= 87 {
            1
          } else {
            2
          }
          let length = if header_length == 1 {
            first - 64
          } else if first == 88 && span.length >= 2 {
            let value = bytearray.at(bytes, 1)
            if value >= 24 && value <= 64 {
              value
            } else {
              -1
            }
          } else {
            -1
          }
          let is_first = control.source_length == 2
          if or {
            length < 1,
            span.absolute_start + header_length + length >= source_end,
            is_first && ( length != 64 || span.length < 3 ),
          } {
            None
          } else if is_first && bytearray.at(bytes, 2) == 0 {
            None
          } else {
            let memory = if is_first {
              4 + length + if bytearray.at(bytes, 2) >= 128 {
                1
              } else {
                0
              }
            } else {
              control.memory + length
            }
            Some(
              CekDataIntegerControlV1 {
                ..control,
                source_length: control.source_length + header_length + length,
                memory,
              },
            )
          }
        }
      }
    _ -> None
  }
}

pub fn step_v1(
  control: CekDataIntegerControlV1,
  source_end: Int,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataIntegerControlV1> {
  if control_is_well_formed(control) {
    prevalidated_step_v1(control, source_end, source_bytes)
  } else {
    None
  }
}

/// The caller authenticated the complete canonical control before this step.
pub fn prevalidated_step_v1(
  control: CekDataIntegerControlV1,
  source_end: Int,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataIntegerControlV1> {
  if control.stage == stage_syntax {
    step_syntax(control, source_bytes)
  } else if control.stage == stage_measure {
    step_measure(control, source_end, source_bytes)
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
