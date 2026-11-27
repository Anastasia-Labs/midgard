//// Test-fixture support for `cek_core_staged_write_bits_loop`: a writeBits
//// step's witness, facts and machine states, success and failure, the hops
//// an honest prover plans over its index list, and the transaction each hop
//// spends. The list's summaries are computed with the generic Data summary,
//// not the loop's own encoder. It is referenced only by tests and is not
//// reachable from any on-chain validator entry point.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Transaction}
use midgard/cek_builtin_v1.{
  SemanticConstantValue, ValueWitnessV1, result_root_v1, value_memory_size_v1,
}
use midgard/cek_cost_v1
use midgard/cek_data_v1.{
  DataSequenceSummaryV1, empty_data_list_summary_v1,
  prepend_data_list_summary_v1, semantic_data_summary_v1,
}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
use midgard/cek_semantic_chain as chain
use midgard/cek_write_bits_loop_v1.{
  WriteBitsHop, WriteBitsHopV1, WriteBitsRedeemer, WriteBitsStartV1,
  WriteBitsStateV1, WriteBitsWalkV1,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  boolean_value, bytes_value, compact, inline_constant, loop_facts, material,
  no_staging, semantic_bytes,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Values

pub const indices_type_cbor = #"9f0500ff"

/// The index list `indices`, as the witness carries it.
pub fn indices_value(indices: List<Int>) -> ValueWitnessV1 {
  inline_constant(
    indices_type_cbor,
    builtin.list_data(list.map(indices, builtin.i_data)),
  )
}

/// The summaries of every suffix of `indices`, the whole list first and the
/// empty list last.
pub fn suffixes(indices: List<Int>) -> List<DataSequenceSummaryV1> {
  when indices is {
    [] -> [empty_data_list_summary_v1()]
    [index, ..rest] -> {
      let tails = suffixes(rest)
      expect [tail, ..] = tails
      [
        prepend_data_list_summary_v1(
          semantic_data_summary_v1(builtin.i_data(index)),
          tail,
        ),
        ..tails
      ]
    }
  }
}

// ## Cases

/// A writeBits step: the witness, the facts the roots and result hops commit
/// (none for a failure), the machine states, the input's bytes, the index
/// list and the summaries of its suffixes.
pub type WriteBitsCaseV1 {
  witness: Data,
  facts: Data,
  pre: machine.MachineStateV1,
  post: machine.MachineStateV1,
  arm: Int,
  input: ByteArray,
  indices: List<Int>,
  tails: List<DataSequenceSummaryV1>,
}

fn pre_state() -> machine.MachineStateV1 {
  machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: #"",
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
}

/// The arguments of writeBits setting (or clearing) bits `indices` of
/// `input`, as the witness carries them.
pub fn write_bits_arguments(
  input: ByteArray,
  indices: List<Int>,
  set: Bool,
) -> List<ValueWitnessV1> {
  [bytes_value(input, False), indices_value(indices), boolean_value(set)]
}

/// A success claiming `result` over `arguments`, the post-state charging
/// `cpu` and `memory`.
pub fn success_case_claiming(
  input: ByteArray,
  indices: List<Int>,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  cpu: Int,
  memory: Int,
) -> WriteBitsCaseV1 {
  let compacted = list.map(arguments, compact)
  let result_root = result_root_v1(result)
  let facts: Data = chain.SemanticRoots {
    roots: chain.BuiltinRoots {
      arguments_root: #"",
      arguments_count: 3,
      result_root,
      builtin_root: #"",
    },
    arguments: compacted,
    result,
  }
  let arguments_data: Data = arguments
  let result_data: Data = result
  let material_data: Data = material(no_staging(), [])
  let pre = pre_state()
  WriteBitsCaseV1 {
    witness: builtin.constr_data(
      31,
      [builtin.i_data(80), arguments_data, result_data, material_data],
    ),
    facts,
    pre,
    post: machine.exact_state(
      pre,
      machine.mode_return,
      result_root,
      proof.empty_environment_root_v1,
      pre.continuation_root,
      0,
      cpu,
      memory,
    ),
    arm: 31,
    input,
    indices,
    tails: suffixes(indices),
  }
}

/// The charge of writeBits over these arguments and `count` indices.
pub fn charge(
  arguments: List<ValueWitnessV1>,
  count: Int,
) -> cek_cost_v1.BuiltinBudgetV1 {
  expect [input, _, value] = list.map(arguments, compact)
  cek_cost_v1.builtin_budget_v1(
    80,
    [value_memory_size_v1(input), count, value_memory_size_v1(value)],
  )
}

/// The honest success of writeBits over these arguments.
pub fn success_case(
  input: ByteArray,
  indices: List<Int>,
  set: Bool,
) -> WriteBitsCaseV1 {
  let arguments = write_bits_arguments(input, indices, set)
  let budget = charge(arguments, list.length(indices))
  success_case_claiming(
    input,
    indices,
    arguments,
    semantic_bytes(builtin.write_bits(input, indices, set), False),
    budget.cpu,
    budget.memory,
  )
}

/// The failure of writeBits over these arguments, halting with the post-state
/// charging `cpu`.
pub fn failure_case_charging(
  input: ByteArray,
  indices: List<Int>,
  arguments: List<ValueWitnessV1>,
  cpu: Int,
) -> WriteBitsCaseV1 {
  let arguments_data: Data = arguments
  let material_data: Data = material(no_staging(), [])
  let pre = pre_state()
  WriteBitsCaseV1 {
    witness: builtin.constr_data(
      36,
      [builtin.i_data(80), arguments_data, material_data],
    ),
    facts: builtin.constr_data(0, []),
    pre,
    post: machine.exact_state(
      pre,
      machine.mode_halt_error,
      proof.hash_error_term_v1(),
      proof.empty_environment_root_v1,
      proof.empty_continuation_root_v1,
      machine.error_builtin_failure,
      cpu,
      0,
    ),
    arm: 36,
    input,
    indices,
    tails: suffixes(indices),
  }
}

/// The failure of writeBits over these arguments, compacted as the roots hop
/// commits them, uncharged.
pub fn failure_case(input: ByteArray, indices: List<Int>) -> WriteBitsCaseV1 {
  failure_case_charging(
    input,
    indices,
    list.map(write_bits_arguments(input, indices, True), compact),
    0,
  )
}

// ## Planning

/// One planned hop: where it starts, its end, what it opens and the state it
/// hands on (`None` for the hop that ends the loop).
pub type PlannedHopV1 {
  from: Option<Pair<Int, Data>>,
  end: Int,
  hop: WriteBitsHopV1,
  next: Option<Data>,
}

fn take(items: List<Int>, count: Int) -> Pair<List<Int>, List<Int>> {
  if count == 0 {
    Pair([], items)
  } else {
    when items is {
      [] -> Pair([], [])
      [item, ..rest] -> {
        let Pair(taken, left) = take(rest, count - 1)
        Pair([item, ..taken], left)
      }
    }
  }
}

/// The first hop's start for `case`.
pub fn start_of(case: WriteBitsCaseV1) -> WriteBitsStartV1 {
  expect [whole, ..] = case.tails
  WriteBitsStartV1 {
    input: case.input,
    items_count: whole.length,
    items_root: whole.root,
  }
}

/// Whether some index of `items` is outside `length` bytes.
pub fn outside(items: List<Int>, length: Int) -> Bool {
  list.any(items, fn(index) { index < 0 || index >= 8 * length })
}

/// The hops of `case` opening `sizes` indices each in turn (the last size
/// repeated), the state each hands on computed from the list, not by the loop.
/// A failure's walk ends at the hop holding an index outside the input.
pub fn plan_sizes(case: WriteBitsCaseV1, sizes: List<Int>) -> List<PlannedHopV1> {
  plan_from(case, sizes, 0, case.input, case.indices, case.tails)
}

/// The hops of `case`, `size` indices each.
pub fn plan(case: WriteBitsCaseV1, size: Int) -> List<PlannedHopV1> {
  plan_sizes(case, [size])
}

fn plan_from(
  case: WriteBitsCaseV1,
  sizes: List<Int>,
  cursor: Int,
  out: ByteArray,
  rest: List<Int>,
  tails: List<DataSequenceSummaryV1>,
) -> List<PlannedHopV1> {
  expect [size, ..more] = sizes
  expect [current, ..] = tails
  let Pair(items, left) = take(rest, size)
  let opened = list.length(items)
  let tails_left = list.drop(tails, opened)
  expect [tail, ..] = tails_left
  expect [whole, ..] = case.tails
  let end = cursor + opened
  let from = if cursor == 0 {
    None
  } else if case.arm == 31 {
    let data: Data = WriteBitsStateV1 {
      out,
      root: current.root,
      count: whole.length,
    }
    Some(Pair(cursor, data))
  } else {
    let data: Data = WriteBitsWalkV1 {
      length: builtin.length_of_bytearray(case.input),
      root: current.root,
    }
    Some(Pair(cursor, data))
  }
  let hop = WriteBitsHopV1 {
    start: if cursor == 0 {
      Some(start_of(case))
    } else {
      None
    },
    items,
    tail,
  }
  let ends = if case.arm == 31 {
    left == []
  } else {
    outside(items, builtin.length_of_bytearray(case.input))
  }
  if ends {
    [PlannedHopV1 { from, end, hop, next: None }]
  } else {
    // An index outside the input is left out of the bytes handed on, so a
    // success over it is refused by the loop, not by its planning.
    let next_out = if case.arm == 31 && !outside(
      items,
      builtin.length_of_bytearray(out),
    ) {
      builtin.write_bits(out, items, write_bits_value(case))
    } else {
      out
    }
    let next: Data = if case.arm == 31 {
      let state: Data = WriteBitsStateV1 {
        out: next_out,
        root: tail.root,
        count: whole.length,
      }
      state
    } else {
      let walk: Data = WriteBitsWalkV1 {
        length: builtin.length_of_bytearray(case.input),
        root: tail.root,
      }
      walk
    }
    let planned = PlannedHopV1 { from, end, hop, next: Some(next) }
    if left == [] {
      // A failure's walk over a list with every index inside the input.
      [planned]
    } else {
      [
        planned,
        ..plan_from(
          case,
          if more == [] {
            sizes
          } else {
            more
          },
          end,
          next_out,
          left,
          tails_left,
        )
      ]
    }
  }
}

fn write_bits_value(case: WriteBitsCaseV1) -> Bool {
  expect facts: chain.SemanticRoots = case.facts
  expect [_, _, value] = facts.arguments
  value == compact(boolean_value(True))
}

// ## Transactions

/// One hop as the loop validator receives it.
pub type WriteBitsStepV1 {
  datum: Option<chain.CoreDatum>,
  redeemer: WriteBitsRedeemer,
  transaction: Transaction,
}

const ct_asset = "cek-core-staged"

/// The bound of `case` with `facts`.
pub fn bound_of(case: WriteBitsCaseV1, facts: Data) -> chain.CoreBound {
  chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: case.pre,
    post: case.post,
    witness_hash: blake2b_256(cbor.serialise(case.witness)),
    arm: case.arm,
    group: if case.arm == 31 {
      5
    } else {
      7
    },
    progress: if case.arm == 31 {
      3
    } else {
      1
    },
    facts,
  }
}

/// The transaction of hop `planned` of `case`, with the bound edited by
/// `edit` in both the datum and the hand-on.
pub fn hop_step_with(
  case: WriteBitsCaseV1,
  planned: PlannedHopV1,
  edit: fn(chain.CoreBound) -> chain.CoreBound,
) -> WriteBitsStepV1 {
  let PlannedHopV1 { from, end, hop, next } = planned
  let bound = edit(
    bound_of(
      case,
      when from is {
        None -> case.facts
        Some(Pair(cursor, state)) -> loop_facts(case.facts, cursor, state)
      },
    ),
  )
  let transaction = when next is {
    None ->
      fixture.continue_mid_step_l1_tx_v1(
        ct_asset,
        chain.CoreBound {
          ..bound,
          facts: case.facts,
          progress: bound.progress + 1,
        },
      )
    Some(state) ->
      fixture.continue_same_step_l1_tx_v1(
        ct_asset,
        chain.CoreBound { ..bound, facts: loop_facts(case.facts, end, state) },
      )
  }
  WriteBitsStepV1 {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    redeemer: ct.Continue(
      WriteBitsHop {
        input_index: 0,
        output_index: 0,
        witness: case.witness,
        end,
        hop,
      },
    ),
    transaction: Transaction {
      ..transaction,
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

pub fn hop_step(case: WriteBitsCaseV1, planned: PlannedHopV1) -> WriteBitsStepV1 {
  hop_step_with(case, planned, fn(bound) { bound })
}
