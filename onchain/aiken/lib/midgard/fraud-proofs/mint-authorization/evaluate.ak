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
  let count = list.length(indices)
  expect count > 0
  expect count <= maximum
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

/// Concatenates the chunks in order while checking each against its committed
/// hash, in one pass. Both lists must end together, so the chunk list equals
/// the committed hash list element by element and in length.
fn authenticated_payload(
  parts: List<ByteArray>,
  hashes: List<ByteArray>,
  acc: ByteArray,
) -> ByteArray {
  when parts is {
    [] -> {
      expect [] = hashes
      acc
    }
    [part, ..rest] -> {
      expect [hash, ..hashes] = hashes
      expect blake2b_256(part) == hash
      authenticated_payload(rest, hashes, bytearray.concat(acc, part))
    }
  }
}

pub fn authenticate_payload(state: State, parts: List<ByteArray>) -> ByteArray {
  let bytes = authenticated_payload(parts, state.preimage_chunk_hashes, #"")
  expect bytearray.length(bytes) == state.raw_length
  bytes
}

/// Applies one batch of operations. The state record is read once and the
/// fields an operation can change travel as loop arguments, so each operation
/// neither rebuilds the record nor re-reads it; the record is rebuilt once at
/// the end of the batch.
pub fn advance(
  state: State,
  bytes: ByteArray,
  operations: List<Operation>,
) -> State {
  let count = list.length(operations)
  expect count > 0
  expect count <= 16
  let
      State {
        script_length,
        signer_start,
        signer_count,
        signer_index,
        signer_hashes,
        validity_interval_start,
        validity_interval_end,
        cursor,
        node_count,
        stack_root,
        stack_depth,
        result,
        ..
      }
    = state
  run(
    operations,
    state,
    bytes,
    script_length,
    signer_start,
    signer_count,
    validity_interval_start,
    validity_interval_end,
    signer_index,
    signer_hashes,
    cursor,
    node_count,
    stack_root,
    stack_depth,
    result,
  )
}

fn run(
  operations: List<Operation>,
  state: State,
  bytes: ByteArray,
  script_length: Int,
  signer_start: Int,
  signer_count: Int,
  validity_interval_start: Int,
  validity_interval_end: Int,
  signer_index: Int,
  signer_hashes: List<ByteArray>,
  cursor: Int,
  node_count: Int,
  stack_root: ByteArray,
  stack_depth: Int,
  result: Int,
) -> State {
  when operations is {
    [] ->
      State {
        ..state,
        signer_index,
        signer_hashes,
        cursor,
        node_count,
        stack_root,
        stack_depth,
        result,
      }
    [operation, ..rest] ->
      when operation is {
        Signer -> {
          expect signer_index < signer_count
          expect cursor == 0 && node_count == 0 && result == -1
          let offset = signer_start + signer_index * 103
          expect builtin.slice_bytearray(offset, 2, bytes) == #"5865"
          let witness = decode_midgard_address_witness_cbor(
            builtin.slice_bytearray(offset + 2, 101, bytes),
          )
          run(
            rest,
            state,
            bytes,
            script_length,
            signer_start,
            signer_count,
            validity_interval_start,
            validity_interval_end,
            signer_index + 1,
            [
              utils.get_verification_key_hash(witness.verification_key),
              ..signer_hashes
            ],
            cursor,
            node_count,
            stack_root,
            stack_depth,
            result,
          )
        }
        Token -> {
          expect signer_index == signer_count
          expect result == -1
          expect cursor < script_length
          expect Some(token) = scan.token_at_v1(bytes, cursor, cursor)
          expect token.next_offset <= script_length
          let node_count = node_count + 1
          expect node_count <= scan.max_native_script_nodes
          if token.kind >= scan.all_node && token.kind <= scan.at_least_node && token.child_count > 0 {
            expect Some(frame) = scan.frame_for_token_v1(token, stack_root)
            let depth = stack_depth + 1
            expect depth <= scan.max_native_script_depth
            run(
              rest,
              state,
              bytes,
              script_length,
              signer_start,
              signer_count,
              validity_interval_start,
              validity_interval_end,
              signer_index,
              signer_hashes,
              token.next_offset,
              node_count,
              scan.hash_frame_v1(frame),
              depth,
              result,
            )
          } else {
            let valid = if token.kind == scan.signature_node {
              list.has(signer_hashes, token.key_hash)
            } else if token.kind == scan.after_node {
              validity_interval_start >= 0 && validity_interval_start >= token.slot
            } else if
            token.kind == scan.before_node {
              validity_interval_end >= 0 && validity_interval_end <= token.slot
            } else {
              expect Some(valid) = scan.empty_container_result_v1(token)
              valid
            }
            run(
              rest,
              state,
              bytes,
              script_length,
              signer_start,
              signer_count,
              validity_interval_start,
              validity_interval_end,
              signer_index,
              signer_hashes,
              token.next_offset,
              node_count,
              stack_root,
              stack_depth,
              if valid {
                1
              } else {
                0
              },
            )
          }
        }
        Frame { frame: parent } -> {
          expect signer_index == signer_count
          expect result == 0 || result == 1
          expect stack_depth > 0
          expect scan.hash_frame_v1(parent) == stack_root
          expect Some(applied) = scan.apply_child_v1(parent, result == 1)
          when applied is {
            scan.NativeFramePending { frame: next } ->
              run(
                rest,
                state,
                bytes,
                script_length,
                signer_start,
                signer_count,
                validity_interval_start,
                validity_interval_end,
                signer_index,
                signer_hashes,
                cursor,
                node_count,
                scan.hash_frame_v1(next),
                stack_depth,
                -1,
              )
            scan.NativeFrameComplete { tail, valid } ->
              run(
                rest,
                state,
                bytes,
                script_length,
                signer_start,
                signer_count,
                validity_interval_start,
                validity_interval_end,
                signer_index,
                signer_hashes,
                cursor,
                node_count,
                tail,
                stack_depth - 1,
                if valid {
                  1
                } else {
                  0
                },
              )
          }
        }
      }
  }
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
  expect _ = advance(fixture(#"82180280"), #"82180280", [Token])
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
    advance(State { ..initial, validity_interval_start: 99 }, bytes, [Token]).result == 0,
    advance(State { ..initial, validity_interval_start: 100 }, bytes, [Token]).result == 1,
    advance(State { ..initial, validity_interval_start: 101 }, bytes, [Token]).result == 1,
    advance(initial, bytes, [Token]).result == 0,
  }
}

test timelock_end_exact_and_adjacent() {
  let bytes = #"82051864"
  let initial = fixture(bytes)
  and {
    advance(State { ..initial, validity_interval_end: 99 }, bytes, [Token]).result == 1,
    advance(State { ..initial, validity_interval_end: 100 }, bytes, [Token]).result == 1,
    advance(State { ..initial, validity_interval_end: 101 }, bytes, [Token]).result == 0,
    advance(initial, bytes, [Token]).result == 0,
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

test payload_prefix_of_the_committed_chunks_refused() fail {
  // The parts authenticate as the first committed chunk and match the claimed
  // raw length, but a committed chunk is left over.
  let state = State {
    ..fixture(#"0102"),
    preimage_chunk_hashes: [blake2b_256(#"0102"), blake2b_256(#"03")],
  }
  expect _ = authenticate_payload(state, [#"0102"])
  True
}

test signer_past_the_count_with_a_carried_witness_refused() fail {
  // A second witness follows the first, so the slice at the next signer
  // offset reads `58 65`; only the count bound refuses the extra signer.
  let Pair(state, raw) = signer_fixture()
  let witness = builtin.slice_bytearray(state.signer_start, 103, raw)
  let two = raw |> bytearray.concat(witness)
  expect _ = advance(state, two, [Signer, Signer])
  True
}
