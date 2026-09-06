use aiken/fuzz
use midgard/ledger_output_proof_v1
use midgard/resolve_inputs_control_v1 as raw
use midgard/validation_machine/machine_types.{ResolveInputOutputProofV1}
use midgard/validation_machine/witness

fn control_fixture() -> raw.ResolveInputsControlRawV1 {
  raw.ResolveInputsControlRawV1 {
    compact_cbor: #"00",
    witness_set_compact_cbor: #"01",
    field_preimage_lengths_cbor: #"02",
    context_cbor: #"03",
    cursor: 0,
    accumulator: #"0000000000000000000000000000000000000000000000000000000000000000",
    remaining_schedule_hash: #"1111111111111111111111111111111111111111111111111111111111111111",
    signer_count: 0,
    signer_frontier_commitment: #"2222222222222222222222222222222222222222222222222222222222222222",
    pending_cbor: #"00",
    resolution_schedule_hash: #"3333333333333333333333333333333333333333333333333333333333333333",
  }
}

test raw_no_pending_encoding_equals_generic(
  args via fuzz.tuple3(
    fuzz.list_between(fuzz.bytearray(), 4, 4),
    fuzz.list_between(fuzz.bytearray_fixed(32), 4, 4),
    fuzz.tuple(fuzz.int_at_least(0), fuzz.int_at_least(0)),
  ),
) {
  let (sources, hashes, counters) = args
  expect [compact, ws, lengths, context] = sources
  expect [accumulator, remaining, frontier, schedule] = hashes
  let (cursor, signers) = counters
  let c = raw.ResolveInputsControlRawV1 {
    compact_cbor: compact,
    witness_set_compact_cbor: ws,
    field_preimage_lengths_cbor: lengths,
    context_cbor: context,
    cursor,
    accumulator,
    remaining_schedule_hash: remaining,
    signer_count: signers,
    signer_frontier_commitment: frontier,
    pending_cbor: #"00",
    resolution_schedule_hash: schedule,
  }
  let encoded = witness.encode_resolve_inputs_witness(
    compact,
    ws,
    lengths,
    context,
    cursor,
    accumulator,
    remaining,
    signers,
    frontier,
    None,
    schedule,
  )
  and {
    raw.encode_control_raw(c) == encoded,
    raw.control_no_pending_from_witness(encoded) == c,
  }
}

test raw_pending_initial_encoding_equals_generic(
  args via fuzz.tuple3(
    fuzz.int_at_least(0),
    fuzz.int_at_least(1),
    fuzz.bytearray_fixed(32),
  ),
) {
  let (index, length, commitment) = args
  let c = control_fixture()
  let pending = ResolveInputOutputProofV1 {
    source_kind: 0,
    key: #"aa",
    next_schedule_hash: c.remaining_schedule_hash,
    descriptor_cbor: #"bb",
    output_proof: ledger_output_proof_v1.initial_control_v1(
      index,
      length,
      commitment,
    ),
  }
  let expected = witness.encode_resolve_inputs_witness(
    c.compact_cbor,
    c.witness_set_compact_cbor,
    c.field_preimage_lengths_cbor,
    c.context_cbor,
    c.cursor,
    c.accumulator,
    c.remaining_schedule_hash,
    c.signer_count,
    c.signer_frontier_commitment,
    Some(pending),
    c.resolution_schedule_hash,
  )
  let raw_pending = raw.encode_pending_raw(
    0,
    #"aa",
    c.remaining_schedule_hash,
    #"bb",
    ledger_output_proof_v1.encode_initial_control_v1(index, length, commitment),
  )
  let next = raw.ResolveInputsControlRawV1 { ..c, pending_cbor: raw_pending }
  and {
    raw.encode_control_raw(next) == expected,
    raw.control_raw_from_witness(expected) == next,
  }
}

test no_pending_parser_refuses_other_pending_bytes() fail {
  let c = control_fixture()
  raw.control_no_pending_from_witness(
    raw.encode_control_raw(
      raw.ResolveInputsControlRawV1 { ..c, pending_cbor: #"80" },
    ),
  ) == c
}
