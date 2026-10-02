use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_bytes_v1
use midgard/fraud_proofs/native_tx/codec.{slice_len}

const typescript_terminal_control_cbor =
  #"8601031118461841d8799f86010100184184010118418183005820ebc3448dad1500c73547d17bd6e9e93387c20bc9422b3313e45b434bf26a967c1841d87a80ff"

fn source_65() -> ByteArray {
  #"5f5840"
    |> bytearray.concat(builtin.replicate_byte(64, 106))
    |> bytearray.concat(#"416aff")
}

fn finish_trace(
  control: cek_data_bytes_v1.CekDataBytesControlV1,
  source: ByteArray,
  source_start: Int,
  source_end: Int,
) -> cek_data_bytes_v1.CekDataBytesControlV1 {
  if control.stage == cek_data_bytes_v1.stage_terminal {
    control
  } else {
    expect
      cek_data_bytes_v1.prevalidated_next_source_span(control, source_end) == cek_data_bytes_v1.next_source_span_v1(
        control,
        source_end,
      )
    expect Some(next) = cek_data_bytes_v1.step_v1(
      control,
      source_end,
      window(control, source, source_start, source_end),
    )
    finish_trace(next, source, source_start, source_end)
  }
}

/// The authenticated window the next step of `control` reads, sliced out of
/// `source` (which starts at absolute `source_start`).
fn window(
  control: cek_data_bytes_v1.CekDataBytesControlV1,
  source: ByteArray,
  source_start: Int,
  source_end: Int,
) -> Option<ByteArray> {
  when cek_data_bytes_v1.next_source_span_v1(control, source_end) is {
    None -> None
    Some(span) ->
      Some(slice_len(source, span.absolute_start - source_start, span.length))
  }
}

/// Measures the indefinite byte string `source` (0x5f opener at absolute 0)
/// and returns the control that leaves the measuring stage, or `None` when a
/// measuring step refuses the authenticated window.
fn measured(
  source: ByteArray,
) -> Option<cek_data_bytes_v1.CekDataBytesControlV1> {
  measure_from(
    cek_data_bytes_v1.initial_measure_control_v1(0),
    source,
    bytearray.length(source),
  )
}

fn measure_from(
  control: cek_data_bytes_v1.CekDataBytesControlV1,
  source: ByteArray,
  source_end: Int,
) -> Option<cek_data_bytes_v1.CekDataBytesControlV1> {
  if control.stage != cek_data_bytes_v1.stage_measure {
    Some(control)
  } else {
    when
      cek_data_bytes_v1.step_v1(
        control,
        source_end,
        window(control, source, 0, source_end),
      )
    is {
      None -> None
      Some(next) -> measure_from(next, source, source_end)
    }
  }
}

/// True when some step of the honest-window trace from `control` refuses.
fn trace_refused(
  control: cek_data_bytes_v1.CekDataBytesControlV1,
  source: ByteArray,
  source_end: Int,
) -> Bool {
  if control.stage == cek_data_bytes_v1.stage_terminal {
    False
  } else {
    when
      cek_data_bytes_v1.step_v1(
        control,
        source_end,
        window(control, source, 0, source_end),
      )
    is {
      None -> True
      Some(next) -> trace_refused(next, source, source_end)
    }
  }
}

fn measured_length(source: ByteArray) -> Option<Int> {
  when measured(source) is {
    None -> None
    Some(control) -> Some(control.bytes_length)
  }
}

fn indefinite(chunks: List<ByteArray>) -> ByteArray {
  chunks
    |> list.foldr(#"ff", fn(chunk, tail) { bytearray.concat(chunk, tail) })
    |> bytearray.concat(#"5f", _)
}

fn chunk(length: Int) -> ByteArray {
  let header = if length < 24 {
    bytearray.from_int_big_endian(64 + length, 1)
  } else {
    #"58" |> bytearray.concat(bytearray.from_int_big_endian(length, 1))
  }
  bytearray.concat(header, builtin.replicate_byte(length, 106))
}

test proves_canonical_chunked_bytes_in_raw_and_virtual_coordinates() {
  let source = source_65()
  let terminal = finish_trace(
    cek_data_bytes_v1.initial_measure_control_v1(17),
    source,
    17,
    87,
  )
  expect Some(summary) = cek_data_bytes_v1.finalize_v1(terminal)
  and {
    summary.root == #"e36bf656c2ebd1bb060fa2a5b3c8d515c595d7ddac821eab2ab29d5e97b29836",
    summary.cbor_length == 70,
    summary.memory == 69,
    cek_data_bytes_v1.encode_control_v1(terminal) == typescript_terminal_control_cbor,
  }
}

test decodes_the_typescript_terminal_control() {
  let control = cek_data_bytes_v1.decode_control_v1(
    typescript_terminal_control_cbor,
  )
  expect Some(summary) = cek_data_bytes_v1.finalize_v1(control)
  and {
    control.stage == cek_data_bytes_v1.stage_terminal,
    control.source_start == 17,
    control.source_length == 70,
    control.bytes_length == 65,
    summary.root == #"e36bf656c2ebd1bb060fa2a5b3c8d515c595d7ddac821eab2ab29d5e97b29836",
    cek_data_bytes_v1.encode_control_v1(control) == typescript_terminal_control_cbor,
  }
}

test canonical_cardano_framing_boundaries_derive_exact_content_lengths() {
  and {
    [
      cek_data_bytes_v1.parse_syntax_v1(#"40", 1),
      cek_data_bytes_v1.parse_syntax_v1(#"416a", 2),
      cek_data_bytes_v1.parse_syntax_v1(#"576a", 24),
      cek_data_bytes_v1.parse_syntax_v1(#"5818", 26),
      cek_data_bytes_v1.parse_syntax_v1(#"583f", 65),
      cek_data_bytes_v1.parse_syntax_v1(#"5840", 66),
    ] == [Some(0), Some(1), Some(23), Some(24), Some(63), Some(64)],
    [
      measured_length(indefinite([chunk(64), chunk(1)])),
      measured_length(indefinite([chunk(64), chunk(64)])),
      measured_length(indefinite([chunk(64), chunk(64), chunk(1)])),
    ] == [Some(65), Some(128), Some(129)],
  }
}

/// An indefinite opener never reaches the definite syntax stage: it is
/// measured from the authenticated chunk headers instead.
test the_definite_syntax_stage_refuses_an_indefinite_opener() {
  and {
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 70) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 134) == None,
  }
}

test rejects_noncanonical_and_impossible_framing_lengths() {
  and {
    cek_data_bytes_v1.parse_syntax_v1(#"5817", 25) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"5841", 67) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"596a", 66) == None,
    // An indefinite string of at most one chunk is not canonical.
    measured_length(indefinite([chunk(1)])) == None,
    measured_length(indefinite([chunk(64)])) == None,
    // An empty chunk, an oversized chunk and a short leading chunk are not
    // canonical chunkings.
    measured_length(indefinite([chunk(64), #"40", chunk(1)])) == None,
    measured_length(
      #"5f5841"
        |> bytearray.concat(builtin.replicate_byte(65, 106))
        |> bytearray.concat(#"416aff"),
    ) == None,
    // A short leading chunk measures to a canonical total length; its
    // layout is refused by the content pass.
    measured_length(indefinite([chunk(1), chunk(64)])) == Some(65),
    trace_refused(
      cek_data_bytes_v1.initial_measure_control_v1(0),
      indefinite([chunk(1), chunk(64)]),
      70,
    ),
    // A non-bytes chunk header.
    measured_length(indefinite([chunk(64), #"01"])) == None,
  }
}

test rejects_a_malformed_authenticated_chunk_header() {
  let source = source_65()
  expect Some(blob_control) = measure_from(
    cek_data_bytes_v1.initial_measure_control_v1(0),
    source,
    70,
  )
  expect blob_control.stage == cek_data_bytes_v1.stage_blob
  expect Some(span) = cek_data_bytes_v1.next_source_span_v1(blob_control, 70)
  let source_bytes = slice_len(source, span.absolute_start, span.length)
  let malformed = #"00"
    |> bytearray.concat(
        slice_len(source_bytes, 1, bytearray.length(source_bytes) - 1),
      )
  cek_data_bytes_v1.step_v1(blob_control, 70, Some(malformed)) == None
}

test rejects_a_missing_or_wrong_indefinite_break() {
  let source = source_65()
  let initial = cek_data_bytes_v1.initial_measure_control_v1(0)
  expect Some(after_first) = cek_data_bytes_v1.step_v1(
    initial,
    70,
    window(initial, source, 0, 70),
  )
  expect Some(at_break) = cek_data_bytes_v1.step_v1(
    after_first,
    70,
    window(after_first, source, 0, 70),
  )
  expect at_break.stage == cek_data_bytes_v1.stage_measure
  expect Some(break_window) = window(at_break, source, 0, 70)
  and {
    break_window == #"ff",
    cek_data_bytes_v1.step_v1(at_break, 70, None) == None,
    cek_data_bytes_v1.step_v1(at_break, 70, Some(#"00")) == None,
    // The enclosing source ends before the break: the last chunk cannot be
    // measured, since a break must still follow it.
    cek_data_bytes_v1.step_v1(
      after_first,
      69,
      window(after_first, source, 0, 69),
    ) == None,
    // The enclosing source ends at the break: there is nothing to measure.
    cek_data_bytes_v1.next_source_span_v1(after_first, 67) == None,
  }
}

/// `control_data_v1` is pinned to the wire encoding at every state of an
/// honest trace: the directly built Plutus data equals the deserialised
/// `encode_control_v1` bytes (the promise its doc comment makes for the
/// shared-LOP scalar attestation path).
fn trace_pins_wire_data(
  control: cek_data_bytes_v1.CekDataBytesControlV1,
  source: ByteArray,
  source_start: Int,
  source_end: Int,
) -> Bool {
  expect Some(wire_data) = cbor.deserialise(
    cek_data_bytes_v1.encode_control_v1(control),
  )
  expect cek_data_bytes_v1.control_data_v1(control) == wire_data
  if control.stage == cek_data_bytes_v1.stage_terminal {
    True
  } else {
    expect Some(next) = cek_data_bytes_v1.step_v1(
      control,
      source_end,
      window(control, source, source_start, source_end),
    )
    trace_pins_wire_data(next, source, source_start, source_end)
  }
}

test bytes_control_data_matches_wire_encoding() {
  and {
    trace_pins_wire_data(
      cek_data_bytes_v1.initial_measure_control_v1(17),
      source_65(),
      17,
      87,
    ),
    trace_pins_wire_data(
      cek_data_bytes_v1.initial_control_v1(0, 4),
      #"436b6b6b",
      0,
      4,
    ),
  }
}
