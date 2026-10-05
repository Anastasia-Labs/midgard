use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_data_integer_v1
use midgard/fraud_proofs/native_tx/codec.{slice_len}

const typescript_terminal_control_cbor =
  #"860102110b0dd8799f860101110b8401010b8183005820529618b73f1e990ed364ce58c08a76518a3f4ddaf2397ea92207a760422764840bd87a80ff"

fn finish_trace(
  control: cek_data_integer_v1.CekDataIntegerControlV1,
  source: ByteArray,
  source_start: Int,
) -> cek_data_integer_v1.CekDataIntegerControlV1 {
  if control.stage == cek_data_integer_v1.stage_terminal {
    control
  } else {
    expect
      cek_data_integer_v1.prevalidated_next_source_span(
        control,
        source_start + bytearray.length(source),
      ) == cek_data_integer_v1.next_source_span_v1(
        control,
        source_start + bytearray.length(source),
      )
    when
      cek_data_integer_v1.next_source_span_v1(
        control,
        source_start + bytearray.length(source),
      )
    is {
      None -> {
        expect Some(next) = cek_data_integer_v1.step_v1(
          control,
          source_start + bytearray.length(source),
          None,
        )
        finish_trace(next, source, source_start)
      }
      Some(span) -> {
        let source_bytes = slice_len(
          source,
          span.absolute_start - source_start,
          span.length,
        )
        expect Some(next) = cek_data_integer_v1.step_v1(
          control,
          source_start + bytearray.length(source),
          Some(source_bytes),
        )
        finish_trace(next, source, source_start)
      }
    }
  }
}

test proves_a_canonical_negative_bignum_without_materializing_it() {
  let source = #"c349010000000000000000"
  let terminal = finish_trace(
    cek_data_integer_v1.initial_control_v1(17, 11),
    source,
    17,
  )
  expect Some(summary) = cek_data_integer_v1.finalize_v1(terminal)
  and {
    summary.root == #"720c28eb8291c0e25d860108458a13027f509d93b9c61296532fdb230063c691",
    summary.cbor_length == 11,
    summary.memory == 13,
    cek_data_integer_v1.encode_control_v1(terminal) == typescript_terminal_control_cbor,
  }
}

test decodes_the_typescript_terminal_control() {
  let control = cek_data_integer_v1.decode_control_v1(
    typescript_terminal_control_cbor,
  )
  expect Some(summary) = cek_data_integer_v1.finalize_v1(control)
  and {
    control.stage == cek_data_integer_v1.stage_terminal,
    control.source_start == 17,
    control.source_length == 11,
    control.memory == 13,
    summary.root == #"720c28eb8291c0e25d860108458a13027f509d93b9c61296532fdb230063c691",
    cek_data_integer_v1.encode_control_v1(control) == typescript_terminal_control_cbor,
  }
}

test canonical_signed_boundaries_have_exact_cek_memory() {
  [
    cek_data_integer_v1.parse_syntax_v1(#"00", 1),
    cek_data_integer_v1.parse_syntax_v1(#"20", 1),
    cek_data_integer_v1.parse_syntax_v1(#"1bffffffffffffffff", 9),
    cek_data_integer_v1.parse_syntax_v1(#"3bffffffffffffffff", 9),
    cek_data_integer_v1.parse_syntax_v1(#"c249010000000000000000", 11),
    cek_data_integer_v1.parse_syntax_v1(#"c349010000000000000000", 11),
  ] == [Some(5), Some(5), Some(13), Some(13), Some(13), Some(13)]
}

test accepts_only_canonical_constructor_alternatives_above_127() {
  and {
    cek_data_integer_v1.parse_large_constructor_syntax_v1(#"1880", 2) != None,
    cek_data_integer_v1.parse_large_constructor_syntax_v1(
      #"1bffffffffffffffff",
      9,
    ) != None,
    cek_data_integer_v1.parse_large_constructor_syntax_v1(
      #"c249010000000000000000",
      11,
    ) != None,
    cek_data_integer_v1.parse_large_constructor_syntax_v1(#"1817", 2) == None,
    cek_data_integer_v1.parse_large_constructor_syntax_v1(#"187f", 2) == None,
    cek_data_integer_v1.parse_large_constructor_syntax_v1(#"3880", 2) == None,
    cek_data_integer_v1.parse_large_constructor_syntax_v1(
      #"c349010000000000000000",
      11,
    ) == None,
  }
}

test rejects_malformed_and_noncanonical_integer_syntax() {
  and {
    cek_data_integer_v1.parse_syntax_v1(#"1817", 2) == None,
    cek_data_integer_v1.parse_syntax_v1(#"c248ffffffffffffffff", 10) == None,
    cek_data_integer_v1.parse_syntax_v1(#"c249000100000000000000", 11) == None,
    cek_data_integer_v1.parse_syntax_v1(#"c25809010000000000000000", 12) == None,
    cek_data_integer_v1.parse_syntax_v1(#"c25f490100000000000000ff", 12) == None,
    cek_data_integer_v1.parse_syntax_v1(#"40", 1) == None,
  }
}

test rejects_missing_short_and_surplus_authenticated_syntax_windows() {
  let initial = cek_data_integer_v1.initial_control_v1(9, 11)
  and {
    cek_data_integer_v1.step_v1(initial, 20, None) == None,
    cek_data_integer_v1.step_v1(initial, 20, Some(#"0000000000")) == None,
    cek_data_integer_v1.step_v1(initial, 20, Some(#"000000000000")) == None,
  }
}

/// `control_data_v1` is pinned to the wire encoding at every state of an
/// honest trace: the directly built Plutus data equals the deserialised
/// `encode_control_v1` bytes (the promise its doc comment makes for the
/// shared-LOP scalar attestation path).
fn trace_pins_wire_data(
  control: cek_data_integer_v1.CekDataIntegerControlV1,
  source: ByteArray,
  source_start: Int,
) -> Bool {
  expect Some(wire_data) = cbor.deserialise(
    cek_data_integer_v1.encode_control_v1(control),
  )
  expect cek_data_integer_v1.control_data_v1(control) == wire_data
  if control.stage == cek_data_integer_v1.stage_terminal {
    True
  } else {
    let source_bytes = when
      cek_data_integer_v1.next_source_span_v1(
        control,
        source_start + bytearray.length(source),
      )
    is {
      None -> None
      Some(span) ->
        Some(slice_len(source, span.absolute_start - source_start, span.length))
    }
    expect Some(next) = cek_data_integer_v1.step_v1(
      control,
      source_start + bytearray.length(source),
      source_bytes,
    )
    trace_pins_wire_data(next, source, source_start)
  }
}

test integer_control_data_matches_wire_encoding() {
  and {
    trace_pins_wire_data(
      cek_data_integer_v1.initial_control_v1(17, 11),
      #"c349010000000000000000",
      17,
    ),
    trace_pins_wire_data(cek_data_integer_v1.initial_control_v1(0, 1), #"0b", 0),
  }
}

test proves_cardano_chunked_signed_magnitudes_with_exact_memory() {
  let value = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096
  let positive = cbor.serialise(value)
  let negative = cbor.serialise(-value - 1)
  let terminal = finish_trace(
    cek_data_integer_v1.initial_control_v1(17, bytearray.length(positive)),
    positive,
    17,
  )
  let negative_terminal = finish_trace(
    cek_data_integer_v1.initial_control_v1(17, bytearray.length(negative)),
    negative,
    17,
  )
  expect Some(summary) = cek_data_integer_v1.finalize_v1(terminal)
  expect Some(negative_summary) = cek_data_integer_v1.finalize_v1(
    negative_terminal,
  )
  and {
    bytearray.length(positive) == 71,
    summary.cbor_length == 71,
    summary.memory == 69,
    summary.root == #"2245ce3c839885a8e667481cec15cbe0e91df8a04f816859ae3bb38a8217b6aa",
    cek_data_integer_v1.encode_control_v1(terminal) == #"8601021118471845d8799f860101111847840101184781830058206f2bcd2c7aaabd4f57c1d3b1d0f2ba43fcbe6927f0551a73dd7daf4fdcf7e3051847d87a80ff",
    negative_summary.cbor_length == 71,
    negative_summary.memory == 69,
    summary.root != negative_summary.root,
    trace_pins_wire_data(
      cek_data_integer_v1.initial_control_v1(17, 71),
      positive,
      17,
    ),
  }
}

test rejects_definite_magnitude_over_cardano_chunk_bound() {
  cek_data_integer_v1.parse_syntax_v1(#"c258410100000000000000000000", 68) == None
}

test rejects_a_single_full_indefinite_magnitude_chunk() {
  let initial = cek_data_integer_v1.initial_measure_control_v1(17)
  expect Some(measured) = cek_data_integer_v1.step_v1(
    initial,
    86,
    Some(#"584001"),
  )
  cek_data_integer_v1.step_v1(measured, 86, Some(#"ff")) == None
}

test rejects_a_short_nonfinal_magnitude_chunk() {
  let initial = cek_data_integer_v1.initial_measure_control_v1(17)
  expect Some(full) = cek_data_integer_v1.step_v1(initial, 100, Some(#"584001"))
  expect Some(partial) = cek_data_integer_v1.step_v1(full, 100, Some(#"410141"))
  cek_data_integer_v1.step_v1(partial, 100, Some(#"4101ff")) == None
}

test rejects_oversized_magnitude_chunks() {
  let initial = cek_data_integer_v1.initial_measure_control_v1(17)
  expect Some(full) = cek_data_integer_v1.step_v1(initial, 200, Some(#"584001"))
  cek_data_integer_v1.step_v1(full, 200, Some(#"584101")) == None
}

test rejects_a_zero_leading_chunked_magnitude_byte() {
  cek_data_integer_v1.step_v1(
    cek_data_integer_v1.initial_measure_control_v1(17),
    100,
    Some(#"584000"),
  ) == None
}

test proves_repeated_full_chunks_then_one_partial_magnitude_chunk() {
  let source = cbor.serialise(
    179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216,
  )
  let terminal = finish_trace(
    cek_data_integer_v1.initial_control_v1(17, bytearray.length(source)),
    source,
    17,
  )
  expect Some(summary) = cek_data_integer_v1.finalize_v1(terminal)
  and {
    summary.cbor_length == 137,
    summary.memory == 133,
    trace_pins_wire_data(
      cek_data_integer_v1.initial_control_v1(17, 137),
      source,
      17,
    ),
  }
}
