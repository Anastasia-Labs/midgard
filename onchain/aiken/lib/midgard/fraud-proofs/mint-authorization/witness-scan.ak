use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/codec.{
  decode_definite_bytes_at, encode_definite_bytes, slice_len,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/script_proof_v1

pub type State {
  policy_id: ByteArray,
  bad_tx_id: ByteArray,
  prior_ledger_root: ByteArray,
  field_length: Int,
  field_chunk_hashes: List<ByteArray>,
  cursor: Int,
  item_index: Int,
  item_count: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Action {
  Advance {
    input_index: Int,
    output_index: Int,
    chunk_reference_indices: List<Int>,
  }
  Finalize { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

pub fn advance(state: State, bytes: ByteArray, remaining: Int) -> State {
  if remaining == 0 || state.item_index == state.item_count {
    state
  } else {
    expect state.item_index < state.item_count
    let Pair(next, item) = decode_definite_bytes_at(bytes, state.cursor)
    expect next <= state.field_length
    expect
      slice_len(bytes, state.cursor, next - state.cursor) == encode_definite_bytes(
        item,
      )
    let Pair(_, script) = decode_midgard_versioned_script_at(item, 0)
    expect encode_midgard_versioned_script(script) == item
    expect script_proof_v1.versioned_script_hash(script) != state.policy_id
    advance(
      State { ..state, cursor: next, item_index: state.item_index + 1 },
      bytes,
      remaining - 1,
    )
  }
}

pub fn authenticate(state: State, parts: List<ByteArray>) -> ByteArray {
  expect list.map(parts, blake2b_256) == state.field_chunk_hashes
  let bytes = list.foldl(
    parts,
    #"",
    fn(part, acc) { bytearray.concat(acc, part) },
  )
  expect bytearray.length(bytes) == state.field_length
  bytes
}

fn fixture(bytes: ByteArray, count: Int) -> State {
  State {
    policy_id: #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    bad_tx_id: #"00",
    prior_ledger_root: #"",
    field_length: bytearray.length(bytes),
    field_chunk_hashes: [blake2b_256(bytes)],
    cursor: 1,
    item_index: 0,
    item_count: count,
  }
}

test exact_absent_witness_scan() {
  let bytes = #"8143820340"
  let final = advance(fixture(bytes, 1), bytes, 16)
  and {
    final.cursor == 5,
    final.item_index == final.item_count,
  }
}

test matching_witness_refused() fail {
  let bytes = #"8143820340"
  let Pair(_, script) = decode_midgard_versioned_script_at(#"820340", 0)
  let state = State {
    ..fixture(bytes, 1),
    policy_id: script_proof_v1.versioned_script_hash(script),
  }
  expect _ = advance(state, bytes, 16)
}

test nonminimal_envelope_refused() fail {
  let bytes = #"815803820340"
  expect _ = advance(fixture(bytes, 1), bytes, 16)
}

test overstated_count_refused() fail {
  let bytes = #"8243820340"
  expect _ = advance(fixture(bytes, 2), bytes, 16)
}

test understated_count_cannot_be_terminal() {
  let bytes = #"814382034043820140"
  let final = advance(fixture(bytes, 1), bytes, 16)
  final.item_index == final.item_count && final.cursor != final.field_length
}

test changed_chunk_refused() fail {
  expect _ = authenticate(fixture(#"8143820340", 1), [#"8143820140"])
}
