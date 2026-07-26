use aiken/builtin
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, PlutusV3Script,
}
use midgard/script_proof_v1

const cross_language_terminal_control_cbor =
  #"8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000"

fn finish_one_block(
  initial: blake2b_224_trace_v1.Blake2b224TraceControlV1,
  block: ByteArray,
  remaining_rounds: Int,
) -> blake2b_224_trace_v1.Blake2b224TraceControlV1 {
  if remaining_rounds == 14 {
    expect Some(begun) = blake2b_224_trace_v1.step_v1(
      initial,
      Some(block),
    )
    finish_one_block(begun, block, remaining_rounds - 1)
  } else if remaining_rounds > 1 {
    expect Some(next) = blake2b_224_trace_v1.step_v1(initial, None)
    finish_one_block(next, block, remaining_rounds - 1)
  } else {
    expect Some(terminal) = blake2b_224_trace_v1.step_v1(initial, None)
    terminal
  }
}

test complete_partial_block_trace_matches_the_builtin() {
  let message = #"03"
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"",
  }
  let initial = blake2b_224_trace_v1.initial_control_v1(
    bytearray.length(message),
  )
  let terminal = finish_one_block(initial, message, 14)
  and {
    blake2b_224_trace_v1.digest_v1(terminal) == Some(blake2b_224(message)),
    blake2b_224_trace_v1.digest_v1(terminal)
      == Some(script_proof_v1.versioned_script_hash(script)),
  }
}

test complete_full_block_trace_matches_the_builtin() {
  let message = builtin.replicate_byte(128, 107)
  let initial = blake2b_224_trace_v1.initial_control_v1(
    bytearray.length(message),
  )
  let terminal = finish_one_block(initial, message, 14)
  blake2b_224_trace_v1.digest_v1(terminal) == Some(blake2b_224(message))
}

test one_round_successor_is_bounded() {
  let message = builtin.replicate_byte(128, 107)
  let initial = blake2b_224_trace_v1.initial_control_v1(129)
  expect Some(begun) = blake2b_224_trace_v1.step_v1(
    initial,
    Some(message),
  )
  expect Some(after_one_round) = blake2b_224_trace_v1.step_v1(
    begun,
    None,
  )
  and {
    after_one_round.stage == blake2b_224_trace_v1.stage_round,
    after_one_round.round == 1,
    after_one_round.cursor == 0,
    bytearray.length(after_one_round.active_block) == 128,
    bytearray.length(after_one_round.working_value) == 128,
  }
}

test decodes_the_typescript_multiblock_terminal_state() {
  let control = blake2b_224_trace_v1.decode_control_v1(
    cross_language_terminal_control_cbor,
  )
  and {
    control.stage == blake2b_224_trace_v1.stage_terminal,
    control.cursor == 6001,
    control.total_length == 6001,
    blake2b_224_trace_v1.digest_v1(control)
      == Some(
          #"634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098",
        ),
    blake2b_224_trace_v1.encode_control_v1(control)
      == cross_language_terminal_control_cbor,
  }
}

test fails_closed_for_wrong_boundaries_and_malformed_state() {
  let initial = blake2b_224_trace_v1.initial_control_v1(129)
  let malformed = blake2b_224_trace_v1.Blake2b224TraceControlV1 {
    ..initial,
    chaining_value: builtin.replicate_byte(63, 0),
  }
  let forged_initial = blake2b_224_trace_v1.Blake2b224TraceControlV1 {
    ..initial,
    chaining_value: builtin.replicate_byte(64, 0),
  }
  let partial_initial = blake2b_224_trace_v1.initial_control_v1(1)
  expect Some(partial_begun) = blake2b_224_trace_v1.step_v1(
    partial_initial,
    Some(#"03"),
  )
  let nonzero_padding = blake2b_224_trace_v1.Blake2b224TraceControlV1 {
    ..partial_begun,
    active_block: #"03"
      |> bytearray.concat(builtin.replicate_byte(126, 0))
      |> bytearray.concat(#"01"),
  }
  and {
    blake2b_224_trace_v1.step_v1(
      initial,
      Some(builtin.replicate_byte(127, 0)),
    ) == None,
    blake2b_224_trace_v1.step_v1(
      malformed,
      Some(builtin.replicate_byte(128, 0)),
    ) == None,
    blake2b_224_trace_v1.step_v1(
      forged_initial,
      Some(builtin.replicate_byte(128, 0)),
    ) == None,
    blake2b_224_trace_v1.step_v1(nonzero_padding, None) == None,
    blake2b_224_trace_v1.step_v1(initial, None) == None,
  }
}
