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
    when cek_data_integer_v1.next_source_span_v1(control) is {
      None -> {
        expect Some(next) = cek_data_integer_v1.step_v1(control, None)
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
    cek_data_integer_v1.step_v1(initial, None) == None,
    cek_data_integer_v1.step_v1(initial, Some(#"0000000000")) == None,
    cek_data_integer_v1.step_v1(initial, Some(#"000000000000")) == None,
  }
}
