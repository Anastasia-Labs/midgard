//// Test-fixture support for `cek_core_semantic_serialise_loop`: a
//// serialiseData step's witness and facts, its result's bytes staged as runs,
//// the hops an honest prover plans, and the transaction each hop spends. It is
//// referenced only by tests and is not reachable from any on-chain validator
//// entry point.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_semantic_chain as chain
use midgard/cek_serialise_loop_v1.{
  CarryV1, SerialiseHop, SerialiseHopV1, SerialiseRedeemer, SerialiseStateV1,
  advance_serialise_v1, initial_state,
}
use midgard/cek_staged_blob_v1.{concat_balanced_v1, flat_root_of_pieces_v1}
use midgard/computation_thread as ct
use midgard/data_serialise_walk_v1.{
  Advanced, Entry, Finished, Frame, Refused, chain as stack_chain, run,
}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  loop_facts, other_input, runs_of, staging_input,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Values

pub const data_type_cbor = #"9f08ff"

pub const bytes_type_cbor = #"9f01ff"

/// A Data constant as the roots hop commits it, from its summary.
pub fn data_value(summary: DataSummaryV1) -> ValueWitnessV1 {
  SemanticConstantValue {
    type_cbor: data_type_cbor,
    payload: summary,
    memory: summary.memory,
  }
}

/// The byte string of the concatenation of `pieces`, root-only.
pub fn bytes_result(pieces: List<ByteArray>) -> ValueWitnessV1 {
  let length = pieces_length(pieces)
  SemanticConstantValue {
    type_cbor: bytes_type_cbor,
    payload: cek_data_v1.bytes_data_summary_v1(
      length,
      flat_root_of_pieces_v1(pieces),
    ),
    memory: cek_data_v1.bytes_memory_size_v1(length),
  }
}

pub fn pieces_length(pieces: List<ByteArray>) -> Int {
  list.foldl(
    pieces,
    0,
    fn(piece, total) { total + builtin.length_of_bytearray(piece) },
  )
}

/// The empty semantic material: the loop reads the result's bytes from
/// staging instead.
pub fn empty_material() -> Data {
  builtin.constr_data(
    0,
    [
      builtin.list_data([]),
      builtin.list_data([]),
      builtin.list_data([]),
      builtin.list_data([]),
    ],
  )
}

/// The arm 31 witness of serialiseData (tag 51).
pub fn serialise_witness(
  argument: ValueWitnessV1,
  result: ValueWitnessV1,
) -> Data {
  let arguments_data: Data = [argument]
  let result_data: Data = result
  builtin.constr_data(
    31,
    [builtin.i_data(51), arguments_data, result_data, empty_material()],
  )
}

/// The facts the roots and result hops commit.
pub fn serialise_facts(argument: ValueWitnessV1, result: ValueWitnessV1) -> Data {
  let facts: Data = chain.SemanticRoots {
    roots: chain.BuiltinRoots {
      arguments_root: #"",
      arguments_count: 1,
      result_root: #"",
      builtin_root: #"",
    },
    arguments: [argument],
    result,
  }
  facts
}

// ## Staging

/// The 4,095-byte chunks of the concatenation of `pieces`, the last one
/// shorter, never building a byte string longer than a piece.
pub fn chunks_of_pieces(pieces: List<ByteArray>) -> List<ByteArray> {
  carry_chunks(pieces, #"")
}

fn carry_chunks(pieces: List<ByteArray>, carry: ByteArray) -> List<ByteArray> {
  when pieces is {
    [] ->
      if carry == #"" {
        []
      } else {
        [carry]
      }
    [piece, ..rest] -> {
      let length = builtin.length_of_bytearray(piece)
      let need = 4095 - builtin.length_of_bytearray(carry)
      if length < need {
        carry_chunks(rest, builtin.append_bytearray(carry, piece))
      } else {
        [
          builtin.append_bytearray(
            carry,
            builtin.slice_bytearray(0, need, piece),
          ),
          ..carry_chunks(
            [builtin.slice_bytearray(need, length - need, piece), ..rest],
            #"",
          )
        ]
      }
    }
  }
}

/// A serialiseData step over staged result bytes: its witness, its facts,
/// the reference inputs (one other input first, then the staging outputs in
/// byte order, which is also their ledger order here), the staging outputs'
/// references and their runs, both in byte order.
pub type SerialiseCaseV1 {
  witness: Data,
  facts: Data,
  reference_inputs: List<Input>,
  refs: List<OutputReference>,
  runs: List<List<ByteArray>>,
}

/// The case for an argument and a result whose bytes are `staged` (which
/// need not be the result's bytes, for a dishonest prover).
pub fn case_of(
  argument: ValueWitnessV1,
  result: ValueWitnessV1,
  staged: List<ByteArray>,
) -> SerialiseCaseV1 {
  let runs = runs_of(chunks_of_pieces(staged))
  let inputs = list.indexed_map(
    runs,
    fn(index, run) { staging_input(10 + index, run) },
  )
  SerialiseCaseV1 {
    witness: serialise_witness(argument, result),
    facts: serialise_facts(argument, result),
    reference_inputs: [other_input(1), ..inputs],
    refs: list.map(inputs, fn(input) { input.output_reference }),
    runs,
  }
}

/// The honest case of `data`, whose leaves are within the semantic summary's
/// bound: the argument is `data`'s summary and the result its CBOR.
pub fn honest_case(data: Data) -> SerialiseCaseV1 {
  let bytes = cbor.serialise(data)
  case_of(
    data_value(cek_data_v1.semantic_data_summary_v1(data)),
    bytes_result([bytes]),
    [bytes],
  )
}

// ## Planning

/// One planned hop: where it starts, its end, what it reads and the state it
/// hands on (`None` for the hop that ends the loop).
pub type PlannedHopV1 {
  from: Option<Pair<Int, Data>>,
  end: Int,
  hop: SerialiseHopV1,
  next: Option<Data>,
}

fn take(entries: List<Entry>, count: Int) -> Pair<List<Entry>, List<Entry>> {
  if count == 0 {
    Pair([], entries)
  } else {
    expect [entry, ..rest] = entries
    let Pair(taken, left) = take(rest, count - 1)
    Pair([entry, ..taken], left)
  }
}

type Trial {
  Fits { steps: Int, popped: Int, finished: Bool }
  Short
}

fn common_prefix(left: List<Entry>, right: List<Entry>) -> Int {
  when left is {
    [l, ..ls] ->
      when right is {
        [r, ..rs] ->
          if l == r {
            1 + common_prefix(ls, rs)
          } else {
            0
          }
        [] -> 0
      }
    [] -> 0
  }
}

/// Steps one at a time over `window` from `at` until `limit` steps, the
/// value's end, or a step the window cannot hold. `floor` is the fewest
/// entries of the starting stack's bottom every step so far kept.
fn trial(
  window: ByteArray,
  at: Int,
  frame: Frame,
  stack: List<Entry>,
  size: Int,
  limit: Int,
  used: Int,
  start: Int,
  floor: Int,
  last: Bool,
) -> Trial {
  if used == limit {
    Fits { steps: used, popped: start - floor, finished: False }
  } else {
    when run(window, at, frame, stack, size, 1) is {
      Advanced { offset, frame: next_frame, stack: next_stack } -> {
        let kept = common_prefix(list.reverse(stack), list.reverse(next_stack))
        trial(
          window,
          offset,
          next_frame,
          next_stack,
          size,
          limit,
          used + 1,
          start,
          if kept < floor {
            kept
          } else {
            floor
          },
          last,
        )
      }
      Finished { .. } ->
        if last {
          // The last step pops the root's entry.
          Fits { steps: used + 1, popped: start, finished: True }
        } else {
          Short
        }
      Refused ->
        if used > 0 && !last {
          Fits { steps: used, popped: start - floor, finished: False }
        } else {
          Short
        }
    }
  }
}

fn window_of(
  case: SerialiseCaseV1,
  state: SerialiseStateV1,
  fresh: List<Int>,
) -> ByteArray {
  let tail = when state.carry is {
    None -> []
    Some(CarryV1 { run: carried, offset }) -> {
      expect Some(index) = list.index_of(case.refs, carried)
      expect Some(chunks) = list.at(case.runs, index)
      let bytes = concat_balanced_v1(chunks)
      [
        builtin.slice_bytearray(
          offset,
          builtin.length_of_bytearray(bytes) - offset,
          bytes,
        ),
      ]
    }
  }
  concat_balanced_v1(
    list.concat(
      tail,
      list.flat_map(
        fresh,
        fn(index) {
          expect Some(chunks) = list.at(case.runs, index)
          chunks
        },
      ),
    ),
  )
}

/// The hop an honest prover runs from `state` with `stack` (the whole stack,
/// top first) and `next_run` the first run not absorbed: at most `limit`
/// steps, the carried tail first and one more run whenever the window runs
/// short.
fn plan_hop(
  case: SerialiseCaseV1,
  state: SerialiseStateV1,
  stack: List<Entry>,
  next_run: Int,
  fresh: Int,
  limit: Int,
) -> Pair<SerialiseHopV1, Pair<Int, Bool>> {
  let count = list.length(case.runs)
  let indices = list.range(next_run, next_run + fresh - 1)
  let window = window_of(case, state, indices)
  let depth = list.length(stack)
  let last = next_run + fresh == count
  when
    trial(
      window,
      0,
      state.frame,
      stack,
      builtin.length_of_bytearray(window),
      limit,
      0,
      depth,
      depth,
      last,
    )
  is {
    Fits { steps, popped, finished } -> {
      let Pair(restore, below) = take(stack, popped)
      let carried = when state.carry is {
        None -> []
        Some(CarryV1 { run: carried, .. }) -> {
          expect Some(index) = list.index_of(case.refs, carried)
          [index]
        }
      }
      Pair(
        SerialiseHopV1 {
          refs: case.refs,
          order: list.concat(carried, indices),
          restore,
          base: stack_chain(below, #""),
        },
        Pair(steps, finished),
      )
    }
    Short -> {
      expect next_run + fresh < count
      plan_hop(case, state, stack, next_run, fresh + 1, limit)
    }
  }
}

fn stack_after(
  case: SerialiseCaseV1,
  state: SerialiseStateV1,
  stack: List<Entry>,
  order: List<Int>,
  steps: Int,
) -> List<Entry> {
  let fresh = when state.carry is {
    None -> order
    Some(_) -> {
      expect [_, ..rest] = order
      rest
    }
  }
  let window = window_of(case, state, fresh)
  expect Advanced { stack: next_stack, .. } = run(
    window,
    0,
    state.frame,
    stack,
    builtin.length_of_bytearray(window),
    steps,
  )
  next_stack
}

fn plan_from(
  case: SerialiseCaseV1,
  cursor: Int,
  state: SerialiseStateV1,
  stack: List<Entry>,
  next_run: Int,
  limit: Int,
) -> List<PlannedHopV1> {
  let from = if cursor == 0 {
    None
  } else {
    let data: Data = state
    Some(Pair(cursor, data))
  }
  let Pair(hop, Pair(steps, finished)) = plan_hop(
    case,
    state,
    stack,
    next_run,
    if state.carry == None && next_run < list.length(case.runs) {
      1
    } else {
      0
    },
    limit,
  )
  let end = cursor + steps
  let fresh = list.length(hop.order) - if state.carry == None {
    0
  } else {
    1
  }
  if finished {
    [PlannedHopV1 { from, end, hop, next: None }]
  } else {
    expect Some(next) = advance_serialise_v1(
      case.facts,
      hop,
      case.reference_inputs,
      from,
      end,
    )
    expect next_state: SerialiseStateV1 = next
    [
      PlannedHopV1 { from, end, hop, next: Some(next) },
      ..plan_from(
        case,
        end,
        next_state,
        stack_after(case, state, stack, hop.order, steps),
        next_run + fresh,
        limit,
      )
    ]
  }
}

/// The hops an honest prover runs, at most `limit` walk steps each.
pub fn plan(case: SerialiseCaseV1, limit: Int) -> List<PlannedHopV1> {
  plan_from(case, 0, initial_state, [], 0, limit)
}

// ## Transactions

/// One hop as the loop validator receives it.
pub type SerialiseStepV1 {
  datum: Option<chain.CoreDatum>,
  redeemer: SerialiseRedeemer,
  transaction: Transaction,
}

const ct_asset = "cek-core-staged"

fn bound_of(witness: Data, facts: Data) -> chain.CoreBound {
  let state = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: #"",
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: state,
    post: state,
    witness_hash: blake2b_256(cbor.serialise(witness)),
    arm: 31,
    group: 5,
    progress: 3,
    facts,
  }
}

/// The transaction of hop `planned` of `case`, over `reference_inputs`.
pub fn hop_step_over(
  case: SerialiseCaseV1,
  planned: PlannedHopV1,
  reference_inputs: List<Input>,
) -> SerialiseStepV1 {
  let PlannedHopV1 { from, end, hop, next } = planned
  let bound = bound_of(
    case.witness,
    when from is {
      None -> case.facts
      Some(Pair(cursor, state)) -> loop_facts(case.facts, cursor, state)
    },
  )
  let transaction = when next is {
    None ->
      fixture.continue_mid_step_l1_tx_v1(
        ct_asset,
        chain.CoreBound { ..bound, facts: case.facts, progress: 4 },
      )
    Some(state) ->
      fixture.continue_same_step_l1_tx_v1(
        ct_asset,
        chain.CoreBound { ..bound, facts: loop_facts(case.facts, end, state) },
      )
  }
  SerialiseStepV1 {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    redeemer: ct.Continue(
      SerialiseHop {
        input_index: 0,
        output_index: 0,
        witness: case.witness,
        end,
        hop,
      },
    ),
    transaction: Transaction {
      ..transaction,
      reference_inputs,
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

pub fn hop_step(case: SerialiseCaseV1, planned: PlannedHopV1) -> SerialiseStepV1 {
  hop_step_over(case, planned, case.reference_inputs)
}
