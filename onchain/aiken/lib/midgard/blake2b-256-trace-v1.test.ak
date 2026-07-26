use aiken/builtin
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/blake2b_256_trace_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}

const cross_language_terminal_control_cbor =
  #"8901031917711917715840f9ddc8a9845baa65c817312211978ef15f2755c6187cfb471438370299d41f38ea7d74eaac2dcc2326180f6a187e4ce0b6ddae62fabd56ad1643beca144b1fb840004000"

fn finish_one_block(
  initial: blake2b_256_trace_v1.Blake2b256TraceControlV1,
  block: ByteArray,
  remaining_steps: Int,
) -> blake2b_256_trace_v1.Blake2b256TraceControlV1 {
  if remaining_steps == 14 {
    expect Some(begun) = blake2b_256_trace_v1.step_v1(
      initial,
      Some(block),
    )
    finish_one_block(begun, block, remaining_steps - 1)
  } else if remaining_steps > 1 {
    expect Some(next) = blake2b_256_trace_v1.step_v1(initial, None)
    finish_one_block(next, block, remaining_steps - 1)
  } else {
    expect Some(terminal) = blake2b_256_trace_v1.step_v1(initial, None)
    terminal
  }
}

test complete_partial_block_trace_matches_the_builtin_256() {
  let message = #"6b"
  let initial = blake2b_256_trace_v1.initial_control_v1(
    bytearray.length(message),
  )
  let terminal = finish_one_block(initial, message, 14)
  blake2b_256_trace_v1.digest_v1(terminal) == Some(blake2b_256(message))
}

test complete_full_block_trace_matches_the_builtin_256() {
  let message = builtin.replicate_byte(128, 107)
  let initial = blake2b_256_trace_v1.initial_control_v1(
    bytearray.length(message),
  )
  let terminal = finish_one_block(initial, message, 14)
  blake2b_256_trace_v1.digest_v1(terminal) == Some(blake2b_256(message))
}

test streamed_framing_matches_the_cek_blob_leaf_hash() {
  let chunk = builtin.replicate_byte(64, 106)
  let message =
    #"4d69646761726443656b426c6f624368756e6b5631"
      |> bytearray.concat(encode_definite_bytes(chunk))
  let initial = blake2b_256_trace_v1.initial_control_v1(
    bytearray.length(message),
  )
  let terminal = finish_one_block(initial, message, 14)
  blake2b_256_trace_v1.digest_v1(terminal)
    == Some(cek_proof_v1.hash_blob_chunk_v1(chunk))
}

test decodes_the_typescript_multiblock_terminal_state_256() {
  let control = blake2b_256_trace_v1.decode_control_v1(
    cross_language_terminal_control_cbor,
  )
  and {
    control.stage == blake2b_256_trace_v1.stage_terminal,
    control.cursor == 6001,
    control.total_length == 6001,
    blake2b_256_trace_v1.digest_v1(control)
      == Some(
          #"f9ddc8a9845baa65c817312211978ef15f2755c6187cfb471438370299d41f38",
        ),
    blake2b_256_trace_v1.encode_control_v1(control)
      == cross_language_terminal_control_cbor,
  }
}
