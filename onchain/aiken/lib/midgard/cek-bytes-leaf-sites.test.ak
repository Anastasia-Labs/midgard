//// A site that creates a B leaf creates the leaf a direct Data constant of the
//// same bytes has, so equalsData over the two returns True. A B leaf's
//// length, memory and CBOR length follow from its byte length; its root
//// follows the site's own chunk layout, which these rows pin against the
//// direct constant's. This module holds the ledger Data stream and the
//// decoder's short byte strings (Value keys and the bulk list path). The
//// decoder's Flat byte strings and Data constants are in
//// `script-decode-v1.test.ak`, and the staged byte-string results in
//// `validators/fraud-proofs/validation-trace/cek-core-semantic-data-scalar.test.ak`.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_builtin_v1.{
  SemanticBuiltinWitnessV1, SemanticConstantValue, ValueWitnessV1,
}
use midgard/cek_data_bytes_v1
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  boolean_value, inline_constant, pattern,
}
use midgard/fraud_proofs/native_tx/codec.{slice_len}
use midgard/script_decode_bits_v1.{short_bytes_summary}

// ## equalsData

const data_type_cbor = #"9f08ff"

/// The Data value whose payload is `summary`.
fn data_value(summary: DataSummaryV1) -> ValueWitnessV1 {
  SemanticConstantValue {
    type_cbor: data_type_cbor,
    payload: summary,
    memory: summary.memory,
  }
}

/// The direct Data constant of `bytes`, within the direct carriage bound.
fn direct(bytes: ByteArray) -> ValueWitnessV1 {
  inline_constant(data_type_cbor, builtin.b_data(bytes))
}

fn equals_data_returns(
  left: ValueWitnessV1,
  right: ValueWitnessV1,
  verdict: Bool,
) -> Bool {
  cek_builtin_v1.verify_semantic_builtin_v1(
    47,
    [left, right],
    boolean_value(verdict),
    SemanticBuiltinWitnessV1 {
      data_nodes: [],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}

/// equalsData over the Data value with leaf `summary` and the direct
/// constant of `bytes` returns True, and not False.
fn equal_as_data(summary: DataSummaryV1, bytes: ByteArray) -> Bool {
  and {
    equals_data_returns(data_value(summary), direct(bytes), True),
    !equals_data_returns(data_value(summary), direct(bytes), False),
  }
}

/// Byte lengths on both sides of each chunk boundary: the 24-byte CBOR head,
/// the 64-byte CBOR chunk and the 4,095-byte blob chunk, up to the direct
/// carriage bound.
const lengths = [0, 1, 23, 24, 64, 65, 128, 129, 4095, 4096, 8190, 8191, 8900]

// ## The ledger Data stream

fn streamed_from(
  control: cek_data_bytes_v1.CekDataBytesControlV1,
  source: ByteArray,
  source_start: Int,
) -> Option<cek_data_bytes_v1.CekDataBytesControlV1> {
  if control.stage == cek_data_bytes_v1.stage_terminal {
    Some(control)
  } else {
    let source_end = source_start + bytearray.length(source)
    let next = when
      cek_data_bytes_v1.next_source_span_v1(control, source_end)
    is {
      None -> cek_data_bytes_v1.step_v1(control, source_end, None)
      Some(span) ->
        if span.absolute_start - source_start + span.length > bytearray.length(
          source,
        ) {
          None
        } else {
          cek_data_bytes_v1.step_v1(
            control,
            source_end,
            Some(
              slice_len(source, span.absolute_start - source_start, span.length),
            ),
          )
        }
    }
    when next is {
      None -> None
      Some(control) -> streamed_from(control, source, source_start)
    }
  }
}

/// The leaf the ledger Data stream commits for a byte string's CBOR, or None
/// when it refuses the spelling. An indefinite string is measured first.
fn streamed(source: ByteArray) -> Option<DataSummaryV1> {
  let control = if bytearray.at(source, 0) == 0x5f {
    cek_data_bytes_v1.initial_measure_control_v1(17)
  } else {
    cek_data_bytes_v1.initial_control_v1(17, bytearray.length(source))
  }
  when streamed_from(control, source, 17) is {
    None -> None
    Some(terminal) -> cek_data_bytes_v1.finalize_v1(terminal)
  }
}

test the_stream_commits_the_leaf_a_direct_constant_has() {
  list.all(
    lengths,
    fn(length) {
      let bytes = pattern(length, 5)
      when streamed(cbor.serialise(bytes)) is {
        None -> False
        Some(summary) -> equal_as_data(summary, bytes)
      }
    },
  )
}

// ## The decoder's short byte strings

test a_short_byte_string_has_the_leaf_a_direct_constant_has() {
  list.all(
    [0, 1, 23, 24, 28, 32, 64, 65, 255, 256, 4095],
    fn(length) {
      let bytes = pattern(length, 9)
      equal_as_data(short_bytes_summary(bytes), bytes)
    },
  )
}
