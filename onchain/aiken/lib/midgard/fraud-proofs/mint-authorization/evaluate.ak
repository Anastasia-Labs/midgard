use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/transaction.{InlineDatum, Input}
use midgard/computation_thread as ct
use midgard/native_script_scan_v1 as scan

pub type State {
  policy_id: ByteArray,
  script_length: Int,
  script_chunk_hashes: List<ByteArray>,
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
  expect list.length(indices) > 0
  expect list.length(indices) <= 3
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
  expect list.map(parts, blake2b_256) == state.script_chunk_hashes
  let bytes = payload(parts)
  expect bytearray.length(bytes) == state.script_length
  bytes
}

pub fn token(state: State, bytes: ByteArray) -> State {
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
    script_chunk_hashes: [blake2b_256(bytes)],
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
    script_chunk_hashes: [blake2b_256(#"01"), blake2b_256(#"02")],
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
