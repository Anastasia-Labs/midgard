use aiken/builtin
use midgard/cek_proof_v1
use midgard/cek_source_blob_v1
use midgard/fraud_proofs/native_tx/codec.{slice_len}

const typescript_terminal_control_cbor =
  #"860101111910008401021910008183015820eeae7280d2825a069ee81fdde1b202e15766bb7bf1689a514224772d104bc59d191000d87a80"

fn finish_trace(
  control: cek_source_blob_v1.CekSourceBlobControlV1,
  source: ByteArray,
) -> cek_source_blob_v1.CekSourceBlobControlV1 {
  if control.stage == cek_source_blob_v1.stage_terminal {
    control
  } else {
    when cek_source_blob_v1.next_source_span_v1(control) is {
      None -> {
        expect Some(next) = cek_source_blob_v1.step_v1(control, None)
        finish_trace(next, source)
      }
      Some(span) -> {
        let source_bytes = slice_len(
          source,
          span.absolute_start - control.source_start,
          span.length,
        )
        expect Some(next) = cek_source_blob_v1.step_v1(
          control,
          Some(source_bytes),
        )
        finish_trace(next, source)
      }
    }
  }
}

test streams_an_authenticated_source_span_into_the_canonical_blob_root() {
  let source = builtin.replicate_byte(64, 106)
  let initial = cek_source_blob_v1.initial_control_v1(73, 64)
  expect Some(first_span) = cek_source_blob_v1.next_source_span_v1(initial)
  let terminal = finish_trace(initial, source)
  and {
    first_span.absolute_start == 73,
    first_span.length == 64,
    cek_source_blob_v1.finalize_v1(terminal)
      == Some(cek_proof_v1.hash_blob_chunk_v1(source)),
  }
}

test decodes_the_typescript_multileaf_terminal_control() {
  let control = cek_source_blob_v1.decode_control_v1(
    typescript_terminal_control_cbor,
  )
  and {
    control.stage == cek_source_blob_v1.stage_terminal,
    control.source_start == 17,
    control.source_length == 4096,
    control.frontier.count == 2,
    control.frontier.byte_length == 4096,
    cek_source_blob_v1.finalize_v1(control)
      == Some(
          #"eeae7280d2825a069ee81fdde1b202e15766bb7bf1689a514224772d104bc59d",
        ),
    cek_source_blob_v1.encode_control_v1(control)
      == typescript_terminal_control_cbor,
  }
}

test rejects_missing_and_surplus_source_bytes() {
  let initial = cek_source_blob_v1.initial_control_v1(9, 129)
  and {
    cek_source_blob_v1.step_v1(initial, None) == None,
    cek_source_blob_v1.step_v1(
      initial,
      Some(builtin.replicate_byte(104, 106)),
    ) == None,
    cek_source_blob_v1.step_v1(
      initial,
      Some(builtin.replicate_byte(106, 106)),
    ) == None,
  }
}
