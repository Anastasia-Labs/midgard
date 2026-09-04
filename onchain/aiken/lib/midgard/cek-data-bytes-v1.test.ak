use aiken/builtin
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
) -> cek_data_bytes_v1.CekDataBytesControlV1 {
  if control.stage == cek_data_bytes_v1.stage_terminal {
    control
  } else {
    when cek_data_bytes_v1.next_source_span_v1(control) is {
      None -> {
        expect Some(next) = cek_data_bytes_v1.step_v1(control, None)
        finish_trace(next, source, source_start)
      }
      Some(span) -> {
        let source_bytes = slice_len(
          source,
          span.absolute_start - source_start,
          span.length,
        )
        expect Some(next) = cek_data_bytes_v1.step_v1(
          control,
          Some(source_bytes),
        )
        finish_trace(next, source, source_start)
      }
    }
  }
}

fn finish_to_break(
  control: cek_data_bytes_v1.CekDataBytesControlV1,
  source: ByteArray,
  source_start: Int,
) -> cek_data_bytes_v1.CekDataBytesControlV1 {
  if control.stage == cek_data_bytes_v1.stage_break {
    control
  } else {
    when cek_data_bytes_v1.next_source_span_v1(control) is {
      None -> {
        expect Some(next) = cek_data_bytes_v1.step_v1(control, None)
        finish_to_break(next, source, source_start)
      }
      Some(span) -> {
        let source_bytes = slice_len(
          source,
          span.absolute_start - source_start,
          span.length,
        )
        expect Some(next) = cek_data_bytes_v1.step_v1(
          control,
          Some(source_bytes),
        )
        finish_to_break(next, source, source_start)
      }
    }
  }
}

test proves_canonical_chunked_bytes_in_raw_and_virtual_coordinates() {
  let source = source_65()
  let terminal = finish_trace(
    cek_data_bytes_v1.initial_control_v1(17, 70),
    source,
    17,
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
  [
    cek_data_bytes_v1.parse_syntax_v1(#"40", 1),
    cek_data_bytes_v1.parse_syntax_v1(#"416a", 2),
    cek_data_bytes_v1.parse_syntax_v1(#"576a", 24),
    cek_data_bytes_v1.parse_syntax_v1(#"5818", 26),
    cek_data_bytes_v1.parse_syntax_v1(#"583f", 65),
    cek_data_bytes_v1.parse_syntax_v1(#"5840", 66),
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 70),
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 134),
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 136),
  ] == [
    Some(0),
    Some(1),
    Some(23),
    Some(24),
    Some(63),
    Some(64),
    Some(65),
    Some(128),
    Some(129),
  ]
}

test rejects_noncanonical_and_impossible_framing_lengths() {
  and {
    cek_data_bytes_v1.parse_syntax_v1(#"5817", 25) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"5841", 67) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 68) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 69) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"5f58", 93) == None,
    cek_data_bytes_v1.parse_syntax_v1(#"596a", 66) == None,
  }
}

test rejects_a_malformed_authenticated_chunk_header() {
  let source = source_65()
  let initial = cek_data_bytes_v1.initial_control_v1(17, 70)
  expect Some(blob_control) = cek_data_bytes_v1.step_v1(
    initial,
    Some(slice_len(source, 0, 2)),
  )
  expect Some(span) = cek_data_bytes_v1.next_source_span_v1(blob_control)
  let source_bytes = slice_len(source, span.absolute_start - 17, span.length)
  let malformed = #"00"
    |> bytearray.concat(
        slice_len(source_bytes, 1, bytearray.length(source_bytes) - 1),
      )
  cek_data_bytes_v1.step_v1(blob_control, Some(malformed)) == None
}

test rejects_a_missing_or_wrong_indefinite_break() {
  let break_control = finish_to_break(
    cek_data_bytes_v1.initial_control_v1(17, 70),
    source_65(),
    17,
  )
  and {
    cek_data_bytes_v1.step_v1(break_control, None) == None,
    cek_data_bytes_v1.step_v1(break_control, Some(#"00")) == None,
  }
}
