//// The hops of `cek_core_staged_write_bits_loop`: writeBits (tag 80) over an
//// index list of any length, beside an input of at most 4,096 bytes.
////
//// The roots hop commits the index list compacted: its payload is the Data
//// summary of a `ListData` node, whose items root heads a chain of list nodes,
//// one per index. The first hop reveals that node and the input's bytes, each
//// re-hashed to its committed summary. Every hop then opens the next indices:
//// they, prepended in order onto the summary of the rest of the list the hop
//// names, must give the root the previous hop left, so a hop opens exactly the
//// next indices and hands on exactly the rest. The list ends at the empty
//// list's root.
////
//// A success (arm 31, after the budget hop, which only routes it) sets the
//// indices' bits in the carried bytes hop by hop; writeBits sets every listed
//// bit to the same value, so this is one call over the whole list, and an
//// index outside the input fails the hop. The hop that opens the end of the
//// list checks the result and the charge, which is read from the revealed
//// index count.
////
//// A failure (arm 36, after the failure roots hop, which routes writeBits
//// over an input of at most 512 words here) walks the list the same way and
//// ends, uncharged, on the hop whose indices hold one outside the input. A
//// walk that reaches the end of the list without one is refused.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_constant_v1.{
  ByteStringConstant, IntegerConstant, ListConstant, encode_constant_type_v1,
}
use midgard/cek_cost_v1
use midgard/cek_data_v1.{
  DataSequenceSummaryV1, DataSummaryV1, IntegerData, ListData,
  bytes_data_summary_v1, bytes_memory_size_v1, empty_data_list_root_v1,
  hash_data_node_v1, integer_memory_size_v1, prepend_data_list_summary_v1,
}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_arm_v1.{boolean_result_v1}
use midgard/cek_staged_blob_v1.{flat_root_v1}
use midgard/computation_thread as ct

/// What the first hop reveals: the input's bytes and the items of the index
/// list's `ListData` node (its CBOR length and memory are the committed
/// summary's).
pub type WriteBitsStartV1 {
  input: ByteArray,
  items_count: Int,
  items_root: ByteArray,
}

/// One hop: the start on the first hop, the indices it opens, in order, and
/// the summary of the rest of the list.
pub type WriteBitsHopV1 {
  start: Option<WriteBitsStartV1>,
  items: List<Int>,
  tail: DataSequenceSummaryV1,
}

/// A success's state between hops: the bytes so far, the root of the rest of
/// the list and the list's index count.
pub type WriteBitsStateV1 {
  out: ByteArray,
  root: ByteArray,
  count: Int,
}

/// A failure's state between hops: the input's length and the root of the
/// rest of the list.
pub type WriteBitsWalkV1 {
  length: Int,
  root: ByteArray,
}

pub type WriteBitsAction {
  WriteBitsHop {
    input_index: Int,
    output_index: Int,
    witness: Data,
    end: Int,
    hop: WriteBitsHopV1,
  }
}

pub type WriteBitsRedeemer =
  ct.StepRedeemer<WriteBitsAction>

const bytes_type_cbor = encode_constant_type_v1(ByteStringConstant)

const indices_type_cbor =
  encode_constant_type_v1(ListConstant { element: IntegerConstant })

/// The summary of integer `value` as a list item.
fn integer_item_v1(value: Int) -> DataSummaryV1 {
  let integer_cbor = cbor.serialise(value)
  let cbor_length = builtin.length_of_bytearray(integer_cbor)
  let memory = 4 + integer_memory_size_v1(value)
  DataSummaryV1 {
    root: hash_data_node_v1(
      IntegerData { cbor_root: flat_root_v1(integer_cbor), cbor_length, memory },
    ),
    cbor_length,
    memory,
  }
}

/// The root of `items` prepended in order onto `tail`.
fn opened_root_v1(items: List<Int>, tail: DataSequenceSummaryV1) -> ByteArray {
  list.foldr(
    items,
    tail,
    fn(value, rest) {
      prepend_data_list_summary_v1(integer_item_v1(value), rest)
    },
  ).root
}

/// `bytes` are the input's committed bytes.
fn input_is_v1(input: ValueWitnessV1, bytes: ByteArray) -> Bool {
  expect SemanticConstantValue { payload, .. } = input
  payload == bytes_data_summary_v1(
    builtin.length_of_bytearray(bytes),
    flat_root_v1(bytes),
  )
}

/// The index list's `ListData` node is the one the list's payload commits.
fn indices_open_v1(indices: ValueWitnessV1, start: WriteBitsStartV1) -> Bool {
  expect SemanticConstantValue { payload, .. } = indices
  payload.root == hash_data_node_v1(
    ListData {
      items_count: start.items_count,
      items_root: start.items_root,
      cbor_length: payload.cbor_length,
      memory: payload.memory,
    },
  )
}

fn type_of(value: ValueWitnessV1) -> ByteArray {
  expect SemanticConstantValue { type_cbor, .. } = value
  type_cbor
}

/// The hop opens the next indices past `cursor`, at least one, and ends after
/// them.
fn opens_v1(cursor: Int, items: List<Int>, end: Int) -> Bool {
  let opened = list.length(items)
  opened > 0 && end == cursor + opened
}

/// One hop of a success: returns the state to carry on with, or `None` once
/// the list ends with the result and the charge checked.
pub fn advance_write_bits_v1(
  bound: chain.CoreBound,
  inner: Data,
  hop: WriteBitsHopV1,
  from: Option<Pair<Int, Data>>,
  end: Int,
) -> Option<Data> {
  expect facts: chain.SemanticRoots = inner
  expect [input, indices, value] = facts.arguments
  let Pair(cursor, state) = when from is {
    None -> {
      expect Some(start) = hop.start
      // writeBits itself refuses an input past 4,096 bytes on every hop.
      expect type_of(input) == bytes_type_cbor
      expect input_is_v1(input, start.input)
      expect type_of(indices) == indices_type_cbor
      expect indices_open_v1(indices, start)
      Pair(
        0,
        WriteBitsStateV1 {
          out: start.input,
          root: start.items_root,
          count: start.items_count,
        },
      )
    }
    Some(Pair(cursor, data)) -> {
      expect state: WriteBitsStateV1 = data
      Pair(cursor, state)
    }
  }
  expect opens_v1(cursor, hop.items, end)
  expect opened_root_v1(hop.items, hop.tail) == state.root
  let set = value == boolean_result_v1(True)
  expect set || value == boolean_result_v1(False)
  let out = builtin.write_bits(state.out, hop.items, set)
  if hop.tail.root == empty_data_list_root_v1() {
    let length = builtin.length_of_bytearray(out)
    // The output has the input's length, and a Boolean's memory is 1.
    let memory = bytes_memory_size_v1(length)
    expect
      facts.result == SemanticConstantValue {
        type_cbor: bytes_type_cbor,
        payload: bytes_data_summary_v1(length, flat_root_v1(out)),
        memory,
      }
    let budget = cek_cost_v1.write_bits_budget_v1([memory, state.count, 1])
    expect
      bound.post == machine.exact_state(
        bound.pre,
        machine.mode_return,
        facts.roots.result_root,
        proof.empty_environment_root_v1,
        bound.pre.continuation_root,
        0,
        budget.cpu,
        budget.memory,
      )
    None
  } else {
    let next: Data = WriteBitsStateV1 {
      out,
      root: hop.tail.root,
      count: state.count,
    }
    Some(next)
  }
}

/// One hop of a failure: returns the state to carry on with, or `None` on the
/// hop whose indices hold one outside the input, with the post-state halted
/// uncharged.
pub fn advance_write_bits_failure_v1(
  bound: chain.CoreBound,
  witness: Data,
  hop: WriteBitsHopV1,
  from: Option<Pair<Int, Data>>,
  end: Int,
) -> Option<Data> {
  let Pair(cursor, walk) = when from is {
    None -> {
      expect Pair(_, [_, arguments_data, _]) = builtin.un_constr_data(witness)
      expect arguments: List<ValueWitnessV1> = arguments_data
      expect [input, indices, _] = arguments
      expect Some(start) = hop.start
      expect input_is_v1(input, start.input)
      expect indices_open_v1(indices, start)
      Pair(
        0,
        WriteBitsWalkV1 {
          length: builtin.length_of_bytearray(start.input),
          root: start.items_root,
        },
      )
    }
    Some(Pair(cursor, data)) -> {
      expect walk: WriteBitsWalkV1 = data
      Pair(cursor, walk)
    }
  }
  expect opens_v1(cursor, hop.items, end)
  expect opened_root_v1(hop.items, hop.tail) == walk.root
  let bits = 8 * walk.length
  if list.any(hop.items, fn(index) { index < 0 || index >= bits }) {
    expect
      bound.post == machine.exact_state(
        bound.pre,
        machine.mode_halt_error,
        proof.hash_error_term_v1(),
        proof.empty_environment_root_v1,
        proof.empty_continuation_root_v1,
        machine.error_builtin_failure,
        0,
        0,
      )
    None
  } else {
    expect hop.tail.root != empty_data_list_root_v1()
    let next: Data = WriteBitsWalkV1 {
      length: walk.length,
      root: hop.tail.root,
    }
    Some(next)
  }
}
