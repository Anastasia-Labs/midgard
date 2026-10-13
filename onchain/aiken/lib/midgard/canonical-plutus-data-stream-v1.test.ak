use aiken/cbor
use aiken/collection/list
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_v1
use midgard/validation_machine_tests/data_stream_driver.{stream_verdict_v1}

fn whole(source: ByteArray) -> Bool {
  stream_verdict_v1(source, 0, bytearray.length(source), 4095, 1_000_000)
}

fn resumed(source: ByteArray) -> Bool {
  let carrier = bytearray.concat(#"5a", bytearray.concat(source, #"a5"))
  stream_verdict_v1(carrier, 1, bytearray.length(source), 68, 1)
}

fn agrees(source: ByteArray, expected: Bool) -> Bool {
  and {
    canonical_plutus_data_v1.is_canonical_plutus_data_v1(source) == expected,
    whole(source) == expected,
    resumed(source) == expected,
  }
}

test smoke_accepts() {
  and {
    agrees(#"00", True),
    agrees(#"9f00ff", True),
    agrees(#"a10000", True),
    agrees(#"d87980", True),
    agrees(#"d8799f0102ff", True),
    agrees(#"a0", True),
    agrees(#"80", True),
  }
}

test smoke_refuses() {
  and {
    agrees(#"9fff", False),
    agrees(#"d8799fff", False),
    agrees(#"bf00ff", False),
    agrees(#"8100", False),
    agrees(#"0000", False),
    agrees(#"9f00", False),
    agrees(#"a100", False),
    agrees(#"1800", False),
  }
}

/// Byte runs that steer random spans through every head the grammar reads.
const tokens: List<ByteArray> =
  [
    #"00", #"17", #"18", #"1817", #"1818", #"19", #"190000", #"1a", #"1b", #"20",
    #"38", #"3818", #"40", #"41", #"4100", #"5818", #"5840", #"5f", #"80", #"81",
    #"9f", #"a0", #"a1", #"a2", #"bf", #"c2", #"c3", #"d8", #"d879", #"d87f",
    #"d880", #"d905", #"d90500", #"d90578", #"d90579", #"d866", #"82", #"1880",
    #"187f", #"ff", #"ff", #"ff", #"60", #"e0",
  ]

fn agree_unknown(source: ByteArray) -> Bool {
  let expected = canonical_plutus_data_v1.is_canonical_plutus_data_v1(source)
  and {
    whole(source) == expected,
    resumed(source) == expected,
  }
}

test stream_agrees_with_one_shot_on_token_spans(
  parts via fuzz.list_between(fuzz.one_of(tokens), 1, 24),
) {
  agree_unknown(list.foldr(parts, #"", bytearray.concat))
}

test stream_agrees_with_one_shot_on_random_bytes(source via fuzz.bytearray()) {
  agree_unknown(source)
}

test stream_accepts_serialised_data(datum via fuzz.data()) {
  agrees(cbor.serialise(datum), True)
}

test stream_agrees_with_one_shot_on_mutated_data(
  args via fuzz.tuple3(fuzz.data(), fuzz.int_at_least(0), fuzz.byte()),
) {
  let (datum, at, byte) = args
  let source = cbor.serialise(datum)
  let index = at % bytearray.length(source)
  let mutated = bytearray.concat(
    bytearray.take(source, index),
    bytearray.push(bytearray.drop(source, index + 1), byte),
  )
  agree_unknown(mutated)
}
