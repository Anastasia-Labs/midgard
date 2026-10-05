use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/cek_source_blob_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, slice_len,
}

pub const version: Int = 1

pub const syntax_bytes: Int = 2

pub const maximum_source_span: Int = 132

pub const stage_syntax: Int = 0

pub const stage_blob: Int = 1

/// An indefinite byte string is measured before its content is streamed:
/// starting at the authenticated 0x5f opener, each step reads the next chunk
/// header from the source and extends `source_length` by that chunk, until the
/// authenticated 0xff break fixes the encoding length. The length is never a
/// witness value.
pub const stage_measure: Int = 2

pub const stage_terminal: Int = 3

const uint32_max = 4294967295

const cardano_data_bytes_chunk = 64

pub type CekDataBytesControlV1 {
  version: Int,
  stage: Int,
  source_start: Int,
  source_length: Int,
  bytes_length: Int,
  blob: Option<cek_source_blob_v1.CekSourceBlobControlV1>,
}

pub type ContentPlanV1 {
  span: cek_source_blob_v1.CekSourceBlobSpanV1,
  content_start: Int,
  content_length: Int,
}

fn minimum(first: Int, second: Int) -> Int {
  if first < second {
    first
  } else {
    second
  }
}

fn definite_bytes_header(length: Int) -> ByteArray {
  expect and {
      length >= 0,
      length <= cardano_data_bytes_chunk,
    }
  if length < 24 {
    bytearray.from_int_big_endian(64 + length, 1)
  } else {
    #"58"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 1))
  }
}

fn definite_header_length(length: Int) -> Int {
  bytearray.length(definite_bytes_header(length))
}

/// The content length of a canonical indefinite byte string whose complete
/// encoding (0x5f opener through the 0xff break) is `source_length` bytes.
fn indefinite_bytes_length(source_length: Int) -> Option<Int> {
  let framed_payload_length = source_length - 2
  let full_chunks = framed_payload_length / ( cardano_data_bytes_chunk + 2 )
  let encoded_remainder = framed_payload_length % (
    cardano_data_bytes_chunk + 2
  )
  let remainder = if encoded_remainder == 0 {
    0
  } else if and {
    encoded_remainder >= 2,
    encoded_remainder <= 24,
  } {
    encoded_remainder - 1
  } else if and {
    encoded_remainder >= 26,
    encoded_remainder <= 65,
  } {
    encoded_remainder - 2
  } else {
    -1
  }
  let bytes_length = full_chunks * cardano_data_bytes_chunk + remainder
  if and {
    source_length >= 2,
    remainder >= 0,
    bytes_length > cardano_data_bytes_chunk,
    bytes_length <= uint32_max,
    cek_data_v1.bytes_data_cbor_length_v1(bytes_length) == source_length,
  } {
    Some(bytes_length)
  } else {
    None
  }
}

/// The definite byte-string syntax. An indefinite (0x5f) string never reaches
/// this stage: it is measured (`stage_measure`) instead.
pub fn parse_syntax_v1(bytes: ByteArray, source_length: Int) -> Option<Int> {
  if or {
    source_length < 1,
    source_length > uint32_max,
    bytearray.length(bytes) != minimum(source_length, syntax_bytes),
  } {
    None
  } else {
    let first = bytearray.at(bytes, 0)
    if and {
      first >= 64,
      first <= 87,
    } {
      let bytes_length = first - 64
      if source_length == 1 + bytes_length {
        Some(bytes_length)
      } else {
        None
      }
    } else if first == 88 {
      if bytearray.length(bytes) < 2 {
        None
      } else {
        let bytes_length = bytearray.at(bytes, 1)
        if and {
          bytes_length >= 24,
          bytes_length <= cardano_data_bytes_chunk,
          source_length == 2 + bytes_length,
        } {
          Some(bytes_length)
        } else {
          None
        }
      }
    } else {
      None
    }
  }
}

pub fn control_is_well_formed(control: CekDataBytesControlV1) -> Bool {
  and {
    control.version == version,
    control.stage >= stage_syntax,
    control.stage <= stage_terminal,
    control.source_start >= 0,
    control.source_length >= 1,
    control.source_length <= uint32_max,
    control.bytes_length >= 0,
    control.bytes_length <= uint32_max,
    if or {
      control.stage == stage_syntax,
      control.stage == stage_measure,
    } {
      and {
        control.bytes_length == 0,
        control.blob == None,
      }
    } else {
      when control.blob is {
        None -> False
        Some(blob_control) -> and {
            cek_data_v1.bytes_data_cbor_length_v1(control.bytes_length) == control.source_length,
            cek_source_blob_v1.control_is_well_formed(blob_control),
            blob_control.source_start == 0,
            blob_control.source_length == control.bytes_length,
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
) -> CekDataBytesControlV1 {
  let control = CekDataBytesControlV1 {
    version,
    stage: stage_syntax,
    source_start,
    source_length,
    bytes_length: 0,
    blob: None,
  }
  expect control_is_well_formed(control)
  control
}

/// The measuring control of an indefinite byte string whose 0x5f opener sits
/// at `source_start`.
pub fn initial_measure_control_v1(source_start: Int) -> CekDataBytesControlV1 {
  let control = CekDataBytesControlV1 {
    version,
    stage: stage_measure,
    source_start,
    source_length: 1,
    bytes_length: 0,
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

pub fn encode_control_v1(control: CekDataBytesControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(6)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.source_start))
    |> bytearray.concat(cbor.serialise(control.source_length))
    |> bytearray.concat(cbor.serialise(control.bytes_length))
    |> bytearray.concat(encode_optional_blob(control.blob))
}

/// The Plutus data form of the wire control — structurally
/// `cbor.deserialise(encode_control_v1(control))` — built directly; see
/// `blake2b_256_trace_v1.control_data_v1`. Pinned by
/// `bytes_control_data_matches_wire_encoding`.
pub fn control_data_v1(control: CekDataBytesControlV1) -> Data {
  builtin.list_data(
    [
      builtin.i_data(version),
      builtin.i_data(control.stage),
      builtin.i_data(control.source_start),
      builtin.i_data(control.source_length),
      builtin.i_data(control.bytes_length),
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

pub fn control_from_data_v1(data: Data) -> CekDataBytesControlV1 {
  expect
      [
        version_data, stage_data, source_start_data, source_length_data,
        bytes_length_data, blob_data,
      ]
    = builtin.un_list_data(data)
  let control = CekDataBytesControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    source_start: builtin.un_i_data(source_start_data),
    source_length: builtin.un_i_data(source_length_data),
    bytes_length: builtin.un_i_data(bytes_length_data),
    blob: optional_blob_from_data(blob_data),
  }
  expect control_is_well_formed(control)
  control
}

pub fn decode_control_v1(control_cbor: ByteArray) -> CekDataBytesControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

fn indefinite_raw_start(
  control: CekDataBytesControlV1,
  content_start: Int,
) -> Int {
  if content_start == control.bytes_length {
    control.source_length - 1
  } else {
    let chunk_index = content_start / cardano_data_bytes_chunk
    let chunk_start = chunk_index * cardano_data_bytes_chunk
    let within_chunk = content_start - chunk_start
    let chunk_length = minimum(
      cardano_data_bytes_chunk,
      control.bytes_length - chunk_start,
    )
    let header_start = 1 + chunk_index * ( cardano_data_bytes_chunk + 2 )
    if within_chunk == 0 {
      header_start
    } else {
      header_start + definite_header_length(chunk_length) + within_chunk
    }
  }
}

fn mapped_raw_length(
  bytes_length: Int,
  content_cursor: Int,
  remaining: Int,
) -> Int {
  if remaining == 0 {
    0
  } else {
    let chunk_start = content_cursor / cardano_data_bytes_chunk * cardano_data_bytes_chunk
    let within_chunk = content_cursor - chunk_start
    let chunk_length = minimum(
      cardano_data_bytes_chunk,
      bytes_length - chunk_start,
    )
    let take = minimum(remaining, chunk_length - within_chunk)
    let header_length = if within_chunk == 0 {
      definite_header_length(chunk_length)
    } else {
      0
    }
    header_length + take + mapped_raw_length(
      bytes_length,
      content_cursor + take,
      remaining - take,
    )
  }
}

fn prevalidated_content_plan(
  control: CekDataBytesControlV1,
) -> Option<ContentPlanV1> {
  if control.stage != stage_blob {
    None
  } else {
    expect Some(blob_control) = control.blob
    when cek_source_blob_v1.prevalidated_next_source_span(blob_control) is {
      None -> None
      Some(virtual_span) -> {
        let content_start = virtual_span.absolute_start
        let content_end = content_start + virtual_span.length
        if or {
          content_start < 0,
          content_end > control.bytes_length,
        } {
          None
        } else if control.bytes_length <= cardano_data_bytes_chunk {
          Some(
            ContentPlanV1 {
              span: cek_source_blob_v1.CekSourceBlobSpanV1 {
                absolute_start: control.source_start + definite_header_length(
                  control.bytes_length,
                ) + content_start,
                length: virtual_span.length,
              },
              content_start,
              content_length: virtual_span.length,
            },
          )
        } else {
          let raw_length = mapped_raw_length(
            control.bytes_length,
            content_start,
            virtual_span.length,
          )
          if raw_length > maximum_source_span {
            None
          } else {
            Some(
              ContentPlanV1 {
                span: cek_source_blob_v1.CekSourceBlobSpanV1 {
                  absolute_start: control.source_start + indefinite_raw_start(
                    control,
                    content_start,
                  ),
                  length: raw_length,
                },
                content_start,
                content_length: virtual_span.length,
              },
            )
          }
        }
      }
    }
  }
}

fn extract_indefinite_content(
  source_bytes: ByteArray,
  raw_cursor: Int,
  bytes_length: Int,
  content_cursor: Int,
  remaining: Int,
) -> Option<ByteArray> {
  if remaining == 0 {
    if raw_cursor == bytearray.length(source_bytes) {
      Some(#"")
    } else {
      None
    }
  } else {
    let chunk_start = content_cursor / cardano_data_bytes_chunk * cardano_data_bytes_chunk
    let within_chunk = content_cursor - chunk_start
    let chunk_length = minimum(
      cardano_data_bytes_chunk,
      bytes_length - chunk_start,
    )
    let take = minimum(remaining, chunk_length - within_chunk)
    let header = if within_chunk == 0 {
      definite_bytes_header(chunk_length)
    } else {
      #""
    }
    let header_length = bytearray.length(header)
    if or {
      take <= 0,
      raw_cursor + header_length + take > bytearray.length(source_bytes),
      slice_len(source_bytes, raw_cursor, header_length) != header,
    } {
      None
    } else {
      let content = slice_len(source_bytes, raw_cursor + header_length, take)
      when
        extract_indefinite_content(
          source_bytes,
          raw_cursor + header_length + take,
          bytes_length,
          content_cursor + take,
          remaining - take,
        )
      is {
        None -> None
        Some(rest) -> Some(bytearray.concat(content, rest))
      }
    }
  }
}

fn extract_content(
  control: CekDataBytesControlV1,
  plan: ContentPlanV1,
  source_bytes: ByteArray,
) -> Option<ByteArray> {
  if bytearray.length(source_bytes) != plan.span.length {
    None
  } else if control.bytes_length <= cardano_data_bytes_chunk {
    Some(source_bytes)
  } else {
    extract_indefinite_content(
      source_bytes,
      0,
      control.bytes_length,
      plan.content_start,
      plan.content_length,
    )
  }
}

/// `source_end` is the absolute end of the enclosing authenticated source;
/// only the measuring stage reads it, to bound its chunk-header window.
pub fn next_source_span_v1(
  control: CekDataBytesControlV1,
  source_end: Int,
) -> Option<cek_source_blob_v1.CekSourceBlobSpanV1> {
  if control_is_well_formed(control) {
    prevalidated_next_source_span(control, source_end)
  } else {
    None
  }
}

/// The next chunk-header window of a measuring control: two bytes (a chunk
/// header, or the 0xff break and whatever follows it), or the single last
/// byte of the enclosing source.
fn measure_span(
  control: CekDataBytesControlV1,
  source_end: Int,
) -> Option<cek_source_blob_v1.CekSourceBlobSpanV1> {
  let absolute_start = control.source_start + control.source_length
  if absolute_start < source_end {
    Some(
      cek_source_blob_v1.CekSourceBlobSpanV1 {
        absolute_start,
        length: minimum(syntax_bytes, source_end - absolute_start),
      },
    )
  } else {
    None
  }
}

/// Same span selection after an earlier gate authenticated the complete
/// canonical control, including every nested scalar/blob/hash state.
pub fn prevalidated_next_source_span(
  control: CekDataBytesControlV1,
  source_end: Int,
) -> Option<cek_source_blob_v1.CekSourceBlobSpanV1> {
  if control.stage == stage_syntax {
    Some(
      cek_source_blob_v1.CekSourceBlobSpanV1 {
        absolute_start: control.source_start,
        length: minimum(control.source_length, syntax_bytes),
      },
    )
  } else if control.stage == stage_measure {
    measure_span(control, source_end)
  } else {
    when prevalidated_content_plan(control) is {
      None -> None
      Some(plan) -> Some(plan.span)
    }
  }
}

fn step_syntax(
  control: CekDataBytesControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataBytesControlV1> {
  when source_bytes is {
    None -> None
    Some(bytes) -> {
      let span_length = minimum(control.source_length, syntax_bytes)
      if bytearray.length(bytes) != span_length {
        None
      } else {
        when parse_syntax_v1(bytes, control.source_length) is {
          None -> None
          Some(bytes_length) -> {
            let next = CekDataBytesControlV1 {
              ..control,
              stage: stage_blob,
              bytes_length,
              blob: Some(cek_source_blob_v1.initial_control_v1(0, bytes_length)),
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

/// One measuring step: the authenticated window at the measured end is either
/// the 0xff break, which fixes the encoding length and starts the content
/// pass, or one definite chunk header, which extends the measured length by
/// that whole chunk. The chunk layout itself is checked by the content pass.
fn step_measure(
  control: CekDataBytesControlV1,
  source_end: Int,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataBytesControlV1> {
  when Pair(source_bytes, measure_span(control, source_end)) is {
    Pair(Some(bytes), Some(span)) ->
      if bytearray.length(bytes) != span.length {
        None
      } else {
        let first = bytearray.at(bytes, 0)
        if first == 255 {
          let source_length = control.source_length + 1
          when indefinite_bytes_length(source_length) is {
            None -> None
            Some(bytes_length) ->
              Some(
                CekDataBytesControlV1 {
                  ..control,
                  stage: stage_blob,
                  source_length,
                  bytes_length,
                  blob: Some(
                    cek_source_blob_v1.initial_control_v1(0, bytes_length),
                  ),
                },
              )
          }
        } else {
          let chunk_length = if and {
            first >= 65,
            first <= 87,
          } {
            first - 63
          } else if and {
            first == 88,
            span.length == 2,
          } {
            let content_length = bytearray.at(bytes, 1)
            if and {
              content_length >= 24,
              content_length <= cardano_data_bytes_chunk,
            } {
              content_length + 2
            } else {
              -1
            }
          } else {
            -1
          }
          if and {
            chunk_length > 0,
            span.absolute_start + chunk_length < source_end,
          } {
            Some(
              CekDataBytesControlV1 {
                ..control,
                source_length: control.source_length + chunk_length,
              },
            )
          } else {
            None
          }
        }
      }
    _ -> None
  }
}

fn step_blob(
  control: CekDataBytesControlV1,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataBytesControlV1> {
  expect Some(blob_control) = control.blob
  if blob_control.stage == cek_source_blob_v1.stage_terminal {
    if source_bytes == None {
      let next = CekDataBytesControlV1 { ..control, stage: stage_terminal }
      if control_is_well_formed(next) {
        Some(next)
      } else {
        None
      }
    } else {
      None
    }
  } else {
    when prevalidated_content_plan(control) is {
      None ->
        if source_bytes == None {
          when cek_source_blob_v1.step_v1(blob_control, None) is {
            None -> None
            Some(blob) -> {
              let next = CekDataBytesControlV1 { ..control, blob: Some(blob) }
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
      Some(plan) ->
        when source_bytes is {
          None -> None
          Some(bytes) ->
            when extract_content(control, plan, bytes) is {
              None -> None
              Some(content) ->
                when
                  cek_source_blob_v1.step_v1(blob_control, Some(content))
                is {
                  None -> None
                  Some(blob) -> {
                    let next = CekDataBytesControlV1 {
                      ..control,
                      blob: Some(blob),
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
}

pub fn step_v1(
  control: CekDataBytesControlV1,
  source_end: Int,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataBytesControlV1> {
  if !control_is_well_formed(control) {
    None
  } else {
    prevalidated_step_v1(control, source_end, source_bytes)
  }
}

/// `step_v1` for a control already proven well-formed by the caller's
/// authentication (the shared-LOP scalar attestation yield attests the claim
/// equals the exact `control_from_data_v1` decode, which enforces
/// `control_is_well_formed`), so the consuming stage yield skips the check.
pub fn prevalidated_step_v1(
  control: CekDataBytesControlV1,
  source_end: Int,
  source_bytes: Option<ByteArray>,
) -> Option<CekDataBytesControlV1> {
  if control.stage == stage_syntax {
    step_syntax(control, source_bytes)
  } else if control.stage == stage_blob {
    step_blob(control, source_bytes)
  } else if control.stage == stage_measure {
    step_measure(control, source_end, source_bytes)
  } else {
    None
  }
}

pub fn finalize_v1(
  control: CekDataBytesControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
  } {
    expect Some(blob_control) = control.blob
    when cek_source_blob_v1.finalize_v1(blob_control) is {
      None -> None
      Some(bytes_root) -> {
        let node = cek_data_v1.BytesData {
          bytes_root,
          bytes_length: control.bytes_length,
          cbor_length: control.source_length,
          memory: 4 + if control.bytes_length == 0 {
            1
          } else {
            control.bytes_length
          },
        }
        Some(
          cek_data_v1.DataSummaryV1 {
            root: cek_data_v1.hash_data_node_v1(node),
            cbor_length: control.source_length,
            memory: cek_data_v1.data_node_memory_v1(node),
          },
        )
      }
    }
  } else {
    None
  }
}
