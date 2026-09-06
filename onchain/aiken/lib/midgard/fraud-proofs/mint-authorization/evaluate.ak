use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/transaction.{InlineDatum, Input}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor,
}
use midgard/native_script_scan_v1 as scan

pub type State {
  policy_id: ByteArray,
  script_length: Int,
  raw_length: Int,
  signer_start: Int,
  signer_count: Int,
  signer_index: Int,
  preimage_chunk_hashes: List<ByteArray>,
  signer_hashes: List<ByteArray>,
  validity_interval_start: Int,
  validity_interval_end: Int,
  cursor: Int,
  node_count: Int,
  stack_root: ByteArray,
  stack_depth: Int,
  result: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Operation {
  Token
  Frame { frame: scan.NativeScriptFrameV1 }
  Signer
}

pub type Action {
  Advance {
    input_index: Int,
    output_index: Int,
    chunk_reference_indices: List<Int>,
    operations: List<Operation>,
  }
  Finalize { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

pub fn chunks(
  indices: List<Int>,
  reference_inputs: List<Input>,
) -> List<ByteArray> {
  chunks_bounded(indices, reference_inputs, 3)
}

pub fn raw_chunks(
  indices: List<Int>,
  reference_inputs: List<Input>,
) -> List<ByteArray> {
  chunks_bounded(indices, reference_inputs, 5)
}

fn chunks_bounded(
  indices: List<Int>,
  reference_inputs: List<Input>,
  maximum: Int,
) -> List<ByteArray> {
  expect list.length(indices) > 0
  expect list.length(indices) <= maximum
  list.map(
    indices,
    fn(index) {
      expect Some(input) = list.at(reference_inputs, index)
      expect InlineDatum(data) = input.output.datum
      let bytes = builtin.un_b_data(data)
      expect bytearray.length(bytes) > 0
      expect bytearray.length(bytes) <= 15148
      bytes
    },
  )
}

pub fn payload(parts: List<ByteArray>) -> ByteArray {
  list.foldl(parts, #"", fn(part, acc) { bytearray.concat(acc, part) })
}

pub fn authenticate_payload(state: State, parts: List<ByteArray>) -> ByteArray {
  expect list.map(parts, blake2b_256) == state.preimage_chunk_hashes
  let bytes = payload(parts)
  expect bytearray.length(bytes) == state.raw_length
  bytes
}

pub fn token(state: State, bytes: ByteArray) -> State {
  expect state.signer_index == state.signer_count
  expect state.result == -1
  expect state.cursor < state.script_length
  expect Some(token) = scan.token_at_v1(bytes, state.cursor, state.cursor)
  expect token.next_offset <= state.script_length
  let node_count = state.node_count + 1
  expect node_count <= scan.max_native_script_nodes
  if token.kind >= scan.all_node && token.kind <= scan.at_least_node && token.child_count > 0 {
    expect Some(frame) = scan.frame_for_token_v1(token, state.stack_root)
    let depth = state.stack_depth + 1
    expect depth <= scan.max_native_script_depth
    State {
      ..state,
      cursor: token.next_offset,
      node_count,
      stack_root: scan.hash_frame_v1(frame),
      stack_depth: depth,
    }
  } else {
    let valid = if token.kind == scan.signature_node {
      list.has(state.signer_hashes, token.key_hash)
    } else if token.kind == scan.after_node {
      state.validity_interval_start >= 0 && state.validity_interval_start >= token.slot
    } else if
    token.kind == scan.before_node {
      state.validity_interval_end >= 0 && state.validity_interval_end <= token.slot
    } else {
      expect Some(valid) = scan.empty_container_result_v1(token)
      valid
    }
    State {
      ..state,
      cursor: token.next_offset,
      node_count,
      result: if valid {
        1
      } else {
        0
      },
    }
  }
}

pub fn frame(state: State, frame: scan.NativeScriptFrameV1) -> State {
  expect state.signer_index == state.signer_count
  expect state.result == 0 || state.result == 1
  expect state.stack_depth > 0
  expect scan.hash_frame_v1(frame) == state.stack_root
  expect Some(applied) = scan.apply_child_v1(frame, state.result == 1)
  when applied is {
    scan.NativeFramePending { frame: next } ->
      State { ..state, stack_root: scan.hash_frame_v1(next), result: -1 }
    scan.NativeFrameComplete { tail, valid } ->
      State {
        ..state,
        stack_root: tail,
        stack_depth: state.stack_depth - 1,
        result: if valid {
          1
        } else {
          0
        },
      }
  }
}

pub fn signer(state: State, bytes: ByteArray) -> State {
  expect state.signer_index < state.signer_count
  expect state.cursor == 0 && state.node_count == 0 && state.result == -1
  let offset = state.signer_start + state.signer_index * 103
  expect builtin.slice_bytearray(offset, 2, bytes) == #"5865"
  let witness = decode_midgard_address_witness_cbor(
    builtin.slice_bytearray(offset + 2, 101, bytes),
  )
  State {
    ..state,
    signer_index: state.signer_index + 1,
    signer_hashes: [
      utils.get_verification_key_hash(witness.verification_key),
      ..state.signer_hashes
    ],
  }
}

pub fn advance(
  state: State,
  bytes: ByteArray,
  operations: List<Operation>,
) -> State {
  expect list.length(operations) > 0
  expect list.length(operations) <= 16
  list.foldl(
    operations,
    state,
    fn(operation, current) {
      when operation is {
        Signer -> signer(current, bytes)
        Token -> token(current, bytes)
        Frame { frame: parent } -> frame(current, parent)
      }
    },
  )
}

fn fixture(bytes: ByteArray) -> State {
  State {
    policy_id: #"00",
    script_length: bytearray.length(bytes),
    raw_length: bytearray.length(bytes),
    signer_start: 0,
    signer_count: 0,
    signer_index: 0,
    preimage_chunk_hashes: [blake2b_256(bytes)],
    signer_hashes: [],
    validity_interval_start: -1,
    validity_interval_end: -1,
    cursor: 0,
    node_count: 0,
    stack_root: #"",
    stack_depth: 0,
    result: -1,
  }
}

test batch_false_exact_terminal() {
  let bytes = #"820281820280"
  let parent = scan.NativeScriptFrameV1 {
    tail: #"",
    kind: 2,
    child_count: 1,
    remaining: 1,
    valid_count: 0,
    required: 0,
  }
  let state = advance(
    fixture(bytes),
    bytes,
    [Token, Token, Frame { frame: parent }],
  )
  and {
    state.cursor == 6,
    state.node_count == 2,
    state.stack_depth == 0,
    state.stack_root == #"",
    state.result == 0,
  }
}

test batch_honest_any_preserves_true() {
  let bytes = #"820281820180"
  let parent = scan.NativeScriptFrameV1 {
    tail: #"",
    kind: 2,
    child_count: 1,
    remaining: 1,
    valid_count: 0,
    required: 0,
  }
  advance(fixture(bytes), bytes, [Token, Token, Frame { frame: parent }]).result == 1
}

test batch_forged_frame_refused() fail {
  let bytes = #"820281820280"
  let parent = scan.NativeScriptFrameV1 {
    tail: #"",
    kind: 1,
    child_count: 1,
    remaining: 1,
    valid_count: 0,
    required: 0,
  }
  expect _ = advance(
    fixture(bytes),
    bytes,
    [Token, Token, Frame { frame: parent }],
  )
  True
}

test batch_empty_refused() fail {
  expect _ = advance(fixture(#"820280"), #"820280", [])
  True
}

test token_noncanonical_refused() fail {
  expect _ = token(fixture(#"82180280"), #"82180280")
  True
}

test payload_reordered_refused() fail {
  let state = State {
    ..fixture(#"0102"),
    preimage_chunk_hashes: [blake2b_256(#"01"), blake2b_256(#"02")],
  }
  expect _ = authenticate_payload(state, [#"02", #"01"])
  True
}

test timelock_start_exact_and_adjacent() {
  let bytes = #"82041864"
  let initial = fixture(bytes)
  and {
    token(State { ..initial, validity_interval_start: 99 }, bytes).result == 0,
    token(State { ..initial, validity_interval_start: 100 }, bytes).result == 1,
    token(State { ..initial, validity_interval_start: 101 }, bytes).result == 1,
    token(initial, bytes).result == 0,
  }
}

test timelock_end_exact_and_adjacent() {
  let bytes = #"82051864"
  let initial = fixture(bytes)
  and {
    token(State { ..initial, validity_interval_end: 99 }, bytes).result == 1,
    token(State { ..initial, validity_interval_end: 100 }, bytes).result == 1,
    token(State { ..initial, validity_interval_end: 101 }, bytes).result == 0,
    token(initial, bytes).result == 0,
  }
}

fn signer_fixture() -> Pair<State, ByteArray> {
  let key_hash = utils.get_verification_key_hash(
    #"0000000000000000000000000000000000000000000000000000000000000000",
  )
  let script = #"8200581c" |> bytearray.concat(key_hash)
  let raw = script
    |> bytearray.concat(
        #"8158658258200000000000000000000000000000000000000000000000000000000000000000584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
      )
  Pair(
    State {
      ..fixture(script),
      raw_length: bytearray.length(raw),
      signer_start: bytearray.length(script) + 1,
      signer_count: 1,
      preimage_chunk_hashes: [blake2b_256(raw)],
    },
    raw,
  )
}

test collected_signer_makes_matching_signature_honest() {
  let Pair(state, raw) = signer_fixture()
  let next = advance(state, raw, [Signer, Token])
  next.signer_index == 1 && list.length(next.signer_hashes) == 1 && next.result == 1
}

test native_token_before_signer_completion_refused() fail {
  let Pair(state, raw) = signer_fixture()
  expect _ = advance(state, raw, [Token])
  True
}

test signer_past_exact_count_refused() fail {
  let Pair(state, raw) = signer_fixture()
  expect _ = advance(state, raw, [Signer, Signer])
  True
}

test changed_signer_carriage_refused() fail {
  let Pair(state, raw) = signer_fixture()
  expect _ = authenticate_payload(state, [raw |> bytearray.concat(#"00")])
  True
}
