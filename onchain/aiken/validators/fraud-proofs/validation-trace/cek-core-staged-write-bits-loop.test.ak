//// Transaction-level rows for the writeBits loop (tag 80, semantics index
//// 12): a success after the budget hop (arm 31) and a failure after the
//// failure roots hop (arm 36), each walking the index list hop by hop. Any
//// split an honest prover picks completes; a list with an index outside the
//// input can only fail, at the hop that opens it, and a list without one can
//// only succeed, at the hop that opens its end.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Transaction}
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget_hop
use fraud_proofs/validation_trace/cek_core_semantic_failure_roots as failure_roots
use fraud_proofs/validation_trace/cek_core_staged_write_bits_loop as write_bits_loop
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_cost_v1 as cost
use midgard/cek_data_v1
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_arm_v1.{StagedMaterialV1}
use midgard/cek_write_bits_loop_v1.{
  WriteBitsHopV1, WriteBitsStartV1, WriteBitsStateV1, WriteBitsWalkV1,
  advance_write_bits_failure_v1, advance_write_bits_v1,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  boolean_value, bytes_value, compact, failure_step_at, inline_constant,
  integer_value, loop_facts, no_staging, pattern, semantic_bytes, staged_step_at,
  string_type_cbor,
}
use midgard/fraud_proofs/cek_write_bits_fixture_v1.{
  PlannedHopV1, WriteBitsCaseV1, WriteBitsStepV1, bound_of, charge, failure_case,
  failure_case_charging, hop_step, hop_step_with, indices_value, plan,
  plan_sizes, success_case, success_case_claiming, suffixes,
  write_bits_arguments,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

fn spend(step: WriteBitsStepV1) -> Bool {
  write_bits_loop.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// Every planned hop of `case`, `size` indices each, is accepted.
fn completes(case: WriteBitsCaseV1, size: Int) -> Bool {
  list.all(plan(case, size), fn(planned) { spend(hop_step(case, planned)) })
}

fn nth(hops: List<PlannedHopV1>, index: Int) -> PlannedHopV1 {
  expect Some(planned) = list.at(hops, index)
  planned
}

/// The hop `planned` of `case`, handing on the state the loop computes from
/// it, as a prover writes it: only the loop's own checks can refuse it.
fn spend_as_computed(case: WriteBitsCaseV1, planned: PlannedHopV1) -> Bool {
  let bound = bound_of(
    case,
    when planned.from is {
      None -> case.facts
      Some(Pair(cursor, state)) -> loop_facts(case.facts, cursor, state)
    },
  )
  let next = if case.arm == 31 {
    advance_write_bits_v1(
      bound,
      case.facts,
      planned.hop,
      planned.from,
      planned.end,
    )
  } else {
    advance_write_bits_failure_v1(
      bound,
      case.witness,
      planned.hop,
      planned.from,
      planned.end,
    )
  }
  spend(hop_step(case, PlannedHopV1 { ..planned, next }))
}

/// `planned` with its hop edited by `edit`.
fn with_hop(
  planned: PlannedHopV1,
  edit: fn(WriteBitsHopV1) -> WriteBitsHopV1,
) -> PlannedHopV1 {
  PlannedHopV1 { ..planned, hop: edit(planned.hop) }
}

fn with_start(
  planned: PlannedHopV1,
  edit: fn(WriteBitsStartV1) -> WriteBitsStartV1,
) -> PlannedHopV1 {
  with_hop(
    planned,
    fn(hop) {
      expect Some(start) = hop.start
      WriteBitsHopV1 { ..hop, start: Some(edit(start)) }
    },
  )
}

// ## Cases

/// 40 indices over a 64-byte input, some repeated, out of order.
fn mid_indices() -> List<Int> {
  list.map(list.range(0, 39), fn(i) { i * 37 % 512 })
    |> list.concat([5, 5, 511, 0])
}

fn mid_case() -> WriteBitsCaseV1 {
  success_case(pattern(64, 3), mid_indices(), True)
}

/// Seven indices a hop over the mid case: seven hops.
fn mid_plan() -> List<PlannedHopV1> {
  plan(mid_case(), 7)
}

/// The mid indices with index `bad` placed at position 20 (in the third hop
/// of seven indices).
fn with_bad(bad: Int) -> List<Int> {
  list.concat(
    list.take(mid_indices(), 20),
    [bad, ..list.drop(mid_indices(), 20)],
  )
}

fn bad_case(bad: Int) -> WriteBitsCaseV1 {
  failure_case(pattern(64, 3), with_bad(bad))
}

// ## Success: completion

test completes_a_short_list_in_one_hop() {
  let case = success_case(pattern(8, 0), [0, 9, 63], True)
  expect [only] = plan(case, 10)
  spend(hop_step(case, only))
}

test completes_under_any_hop_split() {
  list.all([1, 3, 7, 13, 44, 100], fn(size) { completes(mid_case(), size) })
}

test completes_a_clearing_list() {
  completes(success_case(pattern(64, 3), mid_indices(), False), 9)
}

test completes_under_uneven_hop_sizes() {
  let case = mid_case()
  list.all(
    plan_sizes(case, [1, 9, 2, 20, 3]),
    fn(planned) { spend(hop_step(case, planned)) },
  )
}

test completes_one_index_a_hop_over_4096_bytes() {
  completes(success_case(pattern(4096, 1), [32767, 0, 16000], True), 1)
}

test the_mid_plan_has_several_hops() {
  list.length(mid_plan()) == 7
}

// ## Success: the first hop binds the list and the input

test accepts_the_honest_first_hop() {
  spend_as_computed(mid_case(), nth(mid_plan(), 0))
}

/// The `ListData` node of another list.
test refuses_another_list_node() fail {
  expect [_, other, ..] = suffixes(mid_indices())
  spend_as_computed(
    mid_case(),
    with_start(
      nth(mid_plan(), 0),
      fn(start) { WriteBitsStartV1 { ..start, items_root: other.root } },
    ),
  )
}

test refuses_a_wrong_index_count() fail {
  spend_as_computed(
    mid_case(),
    with_start(
      nth(mid_plan(), 0),
      fn(start) {
        WriteBitsStartV1 { ..start, items_count: start.items_count + 1 }
      },
    ),
  )
}

test refuses_other_input_bytes_of_the_same_length() fail {
  spend_as_computed(
    mid_case(),
    with_start(
      nth(mid_plan(), 0),
      fn(start) { WriteBitsStartV1 { ..start, input: pattern(64, 4) } },
    ),
  )
}

test refuses_a_first_hop_without_a_start() fail {
  spend_as_computed(
    mid_case(),
    with_hop(
      nth(mid_plan(), 0),
      fn(hop) { WriteBitsHopV1 { ..hop, start: None } },
    ),
  )
}

/// A case whose arguments are `arguments`, claiming writeBits' honest result
/// and charge over `input` and `indices`.
fn claiming_over(
  input: ByteArray,
  indices: List<Int>,
  arguments: List<ValueWitnessV1>,
) -> WriteBitsCaseV1 {
  let budget = charge(arguments, list.length(indices))
  success_case_claiming(
    input,
    indices,
    arguments,
    semantic_bytes(builtin.write_bits(input, indices, True), False),
    budget.cpu,
    budget.memory,
  )
}

/// The first hop of a case over these arguments.
fn first_hop_over(
  input: ByteArray,
  indices: List<Int>,
  arguments: List<ValueWitnessV1>,
) -> Bool {
  let case = claiming_over(input, indices, arguments)
  spend_as_computed(case, nth(plan(case, 100), 0))
}

/// The first of one-index hops over these arguments: it reads them without
/// reaching the result or the charge.
fn first_of_many_hops_over(
  input: ByteArray,
  indices: List<Int>,
  arguments: List<ValueWitnessV1>,
) -> Bool {
  let case = claiming_over(input, indices, arguments)
  spend_as_computed(case, nth(plan(case, 1), 0))
}

test accepts_the_honest_arguments_on_a_first_of_many_hops() {
  first_of_many_hops_over(
    pattern(8, 0),
    [1, 2],
    write_bits_arguments(pattern(8, 0), [1, 2], True),
  )
}

/// writeBits fails on a Data input, whose payload is the byte string's.
test refuses_a_data_input() fail {
  let input = pattern(8, 0)
  first_of_many_hops_over(
    input,
    [1, 2],
    [
      inline_constant(#"9f08ff", builtin.b_data(input)),
      indices_value([1, 2]),
      boolean_value(True),
    ],
  )
}

test accepts_the_honest_arguments() {
  first_hop_over(
    pattern(8, 0),
    [1, 2],
    write_bits_arguments(pattern(8, 0), [1, 2], True),
  )
}

/// writeBits fails on a string input.
test refuses_a_string_input() fail {
  let input = pattern(8, 0)
  first_hop_over(
    input,
    [1, 2],
    [
      inline_constant(string_type_cbor, builtin.b_data(input)),
      indices_value([1, 2]),
      boolean_value(True),
    ],
  )
}

/// writeBits fails on a list of Data, whose payload is the integer list's.
test refuses_an_index_list_of_data() fail {
  let input = pattern(8, 0)
  first_hop_over(
    input,
    [1, 2],
    [
      bytes_value(input, False),
      inline_constant(
        #"9f0508ff",
        builtin.list_data([builtin.i_data(1), builtin.i_data(2)]),
      ),
      boolean_value(True),
    ],
  )
}

/// writeBits fails on a value that is not a Boolean.
test refuses_a_value_that_is_not_a_boolean() fail {
  let input = pattern(8, 0)
  first_of_many_hops_over(
    input,
    [1, 2],
    [bytes_value(input, False), indices_value([1, 2]), integer_value(1)],
  )
}

/// writeBits fails on an input past 4,096 bytes, even with its indices in it.
test refuses_an_input_past_4096_bytes() fail {
  let input = pattern(4097, 0)
  let arguments = [
    semantic_bytes(input, False),
    indices_value([0]),
    boolean_value(True),
  ]
  let budget = charge(arguments, 1)
  let case = success_case_claiming(
    input,
    [0],
    arguments,
    semantic_bytes(input, False),
    budget.cpu,
    budget.memory,
  )
  spend_as_computed(case, nth(plan(case, 1), 0))
}

test accepts_an_input_of_4096_bytes() {
  let input = pattern(4096, 0)
  first_hop_over(
    input,
    [0, 32767],
    [
      semantic_bytes(input, False),
      indices_value([0, 32767]),
      boolean_value(True),
    ],
  )
}

// ## Success: each hop opens exactly the next indices

test accepts_the_honest_middle_hop() {
  spend_as_computed(mid_case(), nth(mid_plan(), 2))
}

/// Two indices of the hop swapped: the bytes they set are the same.
test refuses_reordered_indices() fail {
  spend_as_computed(
    mid_case(),
    with_hop(
      nth(mid_plan(), 2),
      fn(hop) {
        expect [a, b, ..rest] = hop.items
        WriteBitsHopV1 { ..hop, items: [b, a, ..rest] }
      },
    ),
  )
}

/// The hop's first index skipped, with the tail it truly has.
test refuses_a_skipped_index() fail {
  spend_as_computed(
    mid_case(),
    with_hop(
      nth(mid_plan(), 2),
      fn(hop) {
        expect [_, ..rest] = hop.items
        WriteBitsHopV1 { ..hop, items: rest }
      },
    ),
  )
}

/// The previous hop's indices opened again from the state it handed on.
test refuses_a_replayed_slice() fail {
  let before = nth(mid_plan(), 1)
  let planned = nth(mid_plan(), 2)
  spend_as_computed(
    mid_case(),
    PlannedHopV1 {
      ..planned,
      hop: WriteBitsHopV1 {
        ..planned.hop,
        items: before.hop.items,
        tail: before.hop.tail,
      },
    },
  )
}

/// The hop's indices onto the tail of a later hop: the indices between are
/// skipped.
test refuses_a_wrong_tail() fail {
  let later = nth(mid_plan(), 4)
  spend_as_computed(
    mid_case(),
    with_hop(
      nth(mid_plan(), 2),
      fn(hop) { WriteBitsHopV1 { ..hop, tail: later.hop.tail } },
    ),
  )
}

/// The hop's tail with one more index than the list has.
test refuses_a_tail_of_another_length() fail {
  spend_as_computed(
    mid_case(),
    with_hop(
      nth(mid_plan(), 2),
      fn(hop) {
        WriteBitsHopV1 {
          ..hop,
          tail: cek_data_v1.DataSequenceSummaryV1 {
            ..hop.tail,
            length: hop.tail.length + 1,
          },
        }
      },
    ),
  )
}

/// The middle hop claiming the list ends after it.
test refuses_an_early_end() fail {
  spend_as_computed(
    mid_case(),
    with_hop(
      nth(mid_plan(), 2),
      fn(hop) {
        WriteBitsHopV1 { ..hop, tail: cek_data_v1.empty_data_list_summary_v1() }
      },
    ),
  )
}

test refuses_an_empty_hop() fail {
  let planned = nth(mid_plan(), 2)
  expect Some(Pair(cursor, _)) = planned.from
  spend_as_computed(
    mid_case(),
    PlannedHopV1 {
      ..planned,
      end: cursor,
      hop: WriteBitsHopV1 {
        ..planned.hop,
        items: [],
        tail: nth(mid_plan(), 1).hop.tail,
      },
    },
  )
}

test refuses_an_end_past_the_indices_opened() fail {
  let planned = nth(mid_plan(), 2)
  spend_as_computed(
    mid_case(),
    PlannedHopV1 { ..planned, end: planned.end + 1 },
  )
}

test refuses_a_hand_on_of_other_bytes() fail {
  let planned = nth(mid_plan(), 2)
  expect Some(next) = planned.next
  expect state: WriteBitsStateV1 = next
  let edited: Data = WriteBitsStateV1 { ..state, out: pattern(64, 9) }
  spend(hop_step(mid_case(), PlannedHopV1 { ..planned, next: Some(edited) }))
}

test refuses_a_last_hop_that_carries_on() fail {
  let last = nth(mid_plan(), 6)
  spend(
    hop_step(mid_case(), PlannedHopV1 { ..last, next: nth(mid_plan(), 5).next }),
  )
}

test refuses_a_middle_hop_that_ends() fail {
  spend(hop_step(mid_case(), PlannedHopV1 { ..nth(mid_plan(), 2), next: None }))
}

// ## Success: the hop that ends the list checks the result and the charge

test accepts_the_honest_last_hop() {
  spend_as_computed(mid_case(), nth(mid_plan(), 6))
}

/// The case over the mid indices claiming `result` and charging `cpu` and
/// `memory`, at its last hop.
fn last_hop_claiming(result: ValueWitnessV1, cpu: Int, memory: Int) -> Bool {
  let input = pattern(64, 3)
  let case = success_case_claiming(
    input,
    mid_indices(),
    write_bits_arguments(input, mid_indices(), True),
    result,
    cpu,
    memory,
  )
  spend_as_computed(case, nth(plan(case, 7), 6))
}

fn honest_result() -> ValueWitnessV1 {
  semantic_bytes(builtin.write_bits(pattern(64, 3), mid_indices(), True), False)
}

fn honest_charge() -> cost.BuiltinBudgetV1 {
  charge(write_bits_arguments(pattern(64, 3), mid_indices(), True), 44)
}

test accepts_the_honest_result_and_charge() {
  last_hop_claiming(
    honest_result(),
    honest_charge().cpu,
    honest_charge().memory,
  )
}

test refuses_a_wrong_result() fail {
  last_hop_claiming(
    semantic_bytes(
      builtin.write_bits(pattern(64, 3), mid_indices(), False),
      False,
    ),
    honest_charge().cpu,
    honest_charge().memory,
  )
}

/// The result of the list without its last hop's indices.
test refuses_the_result_of_a_shorter_list() fail {
  last_hop_claiming(
    semantic_bytes(
      builtin.write_bits(pattern(64, 3), list.take(mid_indices(), 42), True),
      False,
    ),
    honest_charge().cpu,
    honest_charge().memory,
  )
}

test refuses_a_wrong_cpu_charge() fail {
  last_hop_claiming(
    honest_result(),
    honest_charge().cpu + 1,
    honest_charge().memory,
  )
}

test refuses_a_wrong_memory_charge() fail {
  last_hop_claiming(
    honest_result(),
    honest_charge().cpu,
    honest_charge().memory + 1,
  )
}

/// The charge of one index fewer.
test refuses_the_charge_of_another_count() fail {
  let fewer = charge(
    write_bits_arguments(pattern(64, 3), mid_indices(), True),
    43,
  )
  last_hop_claiming(honest_result(), fewer.cpu, fewer.memory)
}

test refuses_an_uncharged_success() fail {
  last_hop_claiming(honest_result(), 0, 0)
}

// ## Exactly one result

/// A success over a list holding an index outside the input: the hop that
/// opens it cannot apply it.
test refuses_a_success_with_an_index_outside_the_input() fail {
  let input = pattern(64, 3)
  let indices = with_bad(512)
  let case = success_case_claiming(
    input,
    indices,
    write_bits_arguments(input, indices, True),
    honest_result(),
    honest_charge().cpu,
    honest_charge().memory,
  )
  spend_as_computed(case, nth(plan(case, 7), 2))
}

test accepts_the_hops_before_an_index_outside_the_input() {
  let input = pattern(64, 3)
  let indices = with_bad(512)
  let case = success_case_claiming(
    input,
    indices,
    write_bits_arguments(input, indices, True),
    honest_result(),
    honest_charge().cpu,
    honest_charge().memory,
  )
  and {
    spend_as_computed(case, nth(plan(case, 7), 0)),
    spend_as_computed(case, nth(plan(case, 7), 1)),
  }
}

test refuses_a_success_with_a_negative_index() fail {
  let input = pattern(64, 3)
  let indices = with_bad(-1)
  let case = success_case_claiming(
    input,
    indices,
    write_bits_arguments(input, indices, True),
    honest_result(),
    honest_charge().cpu,
    honest_charge().memory,
  )
  spend_as_computed(case, nth(plan(case, 7), 2))
}

/// The failure walk of the honest mid list reaches its end without an index
/// outside the input.
test refuses_a_failure_over_indices_all_inside_the_input() fail {
  let case = failure_case(pattern(64, 3), mid_indices())
  list.all(plan(case, 7), fn(planned) { spend_as_computed(case, planned) })
}

test refuses_a_failure_that_ends_where_the_list_ends() fail {
  let case = failure_case(pattern(64, 3), mid_indices())
  let hops = plan(case, 7)
  // The walk's planner never ends this list; claim the last hop ends it.
  let last = nth(hops, list.length(hops) - 1)
  spend(hop_step(case, PlannedHopV1 { ..last, next: None }))
}

test the_failure_walk_of_an_in_range_list_has_every_hop() {
  list.length(plan(failure_case(pattern(64, 3), mid_indices()), 7)) == 7
}

// ## Failure: the walk ends at the hop that opens an index outside the input

test proves_a_failure_at_the_hop_holding_the_index() {
  let case = bad_case(512)
  let hops = plan(case, 7)
  and {
    list.length(hops) == 3,
    list.all(hops, fn(planned) { spend(hop_step(case, planned)) }),
  }
}

test proves_a_failure_on_a_negative_index() {
  let case = bad_case(-1)
  list.all(plan(case, 7), fn(planned) { spend(hop_step(case, planned)) })
}

test proves_a_failure_on_an_index_far_past_the_input() {
  let case = bad_case(18446744073709551616)
  list.all(plan(case, 7), fn(planned) { spend_as_computed(case, planned) })
}

test proves_a_failure_on_the_first_hop() {
  let case = failure_case(pattern(8, 0), [64, 1, 2])
  expect [only] = plan(case, 10)
  spend(hop_step(case, only))
}

test proves_a_failure_over_compacted_arguments() {
  let input = pattern(64, 3)
  let indices = with_bad(512)
  let case = failure_case_charging(
    input,
    indices,
    list.map(write_bits_arguments(input, indices, False), compact),
    0,
  )
  list.all(plan(case, 7), fn(planned) { spend_as_computed(case, planned) })
}

/// The loop reads only the compacted arguments the roots hop commits.
test refuses_a_failure_over_inline_arguments() fail {
  let input = pattern(64, 3)
  let indices = with_bad(512)
  let case = failure_case_charging(
    input,
    indices,
    write_bits_arguments(input, indices, True),
    0,
  )
  spend_as_computed(case, nth(plan(case, 7), 0))
}

/// The last index of the input is inside it: the walk goes on.
test refuses_a_failure_at_the_last_bit() fail {
  let case = bad_case(511)
  spend(hop_step(case, PlannedHopV1 { ..nth(plan(case, 7), 2), next: None }))
}

test the_walk_goes_on_past_the_last_bit() {
  let case = bad_case(511)
  spend_as_computed(case, nth(plan(case, 7), 2))
}

/// The hop before the one holding the index claims the failure.
test refuses_a_failure_at_an_earlier_hop() fail {
  let case = bad_case(512)
  spend(hop_step(case, PlannedHopV1 { ..nth(plan(case, 7), 1), next: None }))
}

/// The hop that holds the index carries on.
test refuses_a_walk_past_the_index() fail {
  let case = bad_case(512)
  let hops = plan(case, 7)
  let planned = nth(hops, 2)
  let next: Data = WriteBitsWalkV1 { length: 64, root: planned.hop.tail.root }
  spend(hop_step(case, PlannedHopV1 { ..planned, next: Some(next) }))
}

test refuses_a_charged_failure() fail {
  let input = pattern(64, 3)
  let indices = with_bad(512)
  let case = failure_case_charging(
    input,
    indices,
    list.map(write_bits_arguments(input, indices, True), compact),
    1,
  )
  list.all(plan(case, 7), fn(planned) { spend_as_computed(case, planned) })
}

/// An index outside the input forged into an in-range list's hop.
test refuses_a_forged_index_outside_the_input() fail {
  let case = failure_case(pattern(64, 3), mid_indices())
  spend_as_computed(
    case,
    with_hop(
      nth(plan(case, 7), 2),
      fn(hop) {
        expect [_, ..rest] = hop.items
        WriteBitsHopV1 { ..hop, items: [512, ..rest] }
      },
    ),
  )
}

/// A shorter input revealed for the failure: index 300 is past 32 bytes.
test refuses_a_failure_over_a_shorter_input() fail {
  let case = failure_case(pattern(64, 3), mid_indices())
  spend_as_computed(
    case,
    with_start(
      nth(plan(case, 7), 0),
      fn(start) { WriteBitsStartV1 { ..start, input: pattern(32, 3) } },
    ),
  )
}

/// The walk of another list, one with an index outside the input, claimed
/// over an in-range list.
test refuses_the_walk_of_another_list() fail {
  let case = failure_case(pattern(64, 3), mid_indices())
  list.all(
    plan(bad_case(512), 7),
    fn(planned) { spend_as_computed(case, planned) },
  )
}

test refuses_another_list_node_for_a_failure() fail {
  let case = bad_case(512)
  expect [_, other, ..] = suffixes(with_bad(512))
  spend_as_computed(
    case,
    with_start(
      nth(plan(case, 7), 0),
      fn(start) { WriteBitsStartV1 { ..start, items_root: other.root } },
    ),
  )
}

test refuses_a_failure_hop_that_skips_an_index() fail {
  let case = bad_case(512)
  spend_as_computed(
    case,
    with_hop(
      nth(plan(case, 7), 1),
      fn(hop) {
        expect [_, ..rest] = hop.items
        WriteBitsHopV1 { ..hop, items: rest }
      },
    ),
  )
}

test refuses_an_empty_failure_hop() fail {
  let case = bad_case(512)
  let planned = nth(plan(case, 7), 1)
  expect Some(Pair(cursor, _)) = planned.from
  spend_as_computed(
    case,
    PlannedHopV1 {
      ..planned,
      end: cursor,
      hop: WriteBitsHopV1 {
        ..planned.hop,
        items: [],
        tail: nth(plan(case, 7), 0).hop.tail,
      },
    },
  )
}

test refuses_a_failure_end_past_the_indices_opened() fail {
  let case = bad_case(512)
  let planned = nth(plan(case, 7), 1)
  spend_as_computed(case, PlannedHopV1 { ..planned, end: planned.end + 1 })
}

// ## The bound

/// Hop 2 of the mid plan (or of the failure) over its bound edited by `edit`.
fn spend_bound(
  case: WriteBitsCaseV1,
  edit: fn(chain.CoreBound) -> chain.CoreBound,
) -> Bool {
  spend(hop_step_with(case, nth(plan(case, 7), 1), edit))
}

test accepts_the_honest_bounds() {
  and {
    spend_bound(mid_case(), fn(bound) { bound }),
    spend_bound(bad_case(512), fn(bound) { bound }),
  }
}

test refuses_a_success_of_another_group() fail {
  spend_bound(mid_case(), fn(bound) { chain.CoreBound { ..bound, group: 4 } })
}

test refuses_a_failure_of_another_group() fail {
  spend_bound(
    bad_case(512),
    fn(bound) { chain.CoreBound { ..bound, group: 5 } },
  )
}

test refuses_a_step_of_another_arm() fail {
  spend_bound(bad_case(512), fn(bound) { chain.CoreBound { ..bound, arm: 30 } })
}

test refuses_a_success_at_another_progress() fail {
  spend_bound(
    mid_case(),
    fn(bound) { chain.CoreBound { ..bound, progress: 1 } },
  )
}

test refuses_a_failure_at_another_progress() fail {
  spend_bound(
    bad_case(512),
    fn(bound) { chain.CoreBound { ..bound, progress: 3 } },
  )
}

fn in_mode(bound: chain.CoreBound) -> chain.CoreBound {
  chain.CoreBound {
    ..bound,
    pre: machine.MachineStateV1 {
      ..bound.pre,
      mode: machine.mode_semantic_builtin,
    },
  }
}

test refuses_a_success_in_another_mode() fail {
  spend_bound(mid_case(), in_mode)
}

test refuses_a_failure_in_another_mode() fail {
  spend_bound(bad_case(512), in_mode)
}

test refuses_a_witness_that_is_not_bound() fail {
  let case = mid_case()
  let step = hop_step(case, nth(mid_plan(), 1))
  expect ct.Continue(action) = step.redeemer
  spend(
    WriteBitsStepV1 {
      ..step,
      redeemer: ct.Continue(
        cek_write_bits_loop_v1.WriteBitsHop {
          ..action,
          witness: builtin.constr_data(31, []),
        },
      ),
    },
  )
}

// ## Routing

/// Every builtin tag the budget hop routes to a semantics entry.
fn routed_tags() -> List<Int> {
  list.concat(
    list.range(0, 20),
    list.concat(
      list.range(22, 25),
      list.concat(
        list.range(29, 37),
        list.concat(
          list.range(39, 42),
          list.concat(
            list.range(44, 51),
            [53, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 82, 83, 84, 85, 86],
          ),
        ),
      ),
    ),
  )
}

/// The loop does not read the tag: it relies on semantics entry 12 taking
/// writeBits (tag 80) alone.
test only_write_bits_routes_to_the_write_bits_loop() {
  list.all(
    routed_tags(),
    fn(tag) {
      if tag == 80 {
        chain.semantic_group(tag) == 12
      } else {
        chain.semantic_group(tag) != 12
      }
    },
  )
}

const loop_hash = #"5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e5e"

const other_hash = #"5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f"

/// The budget hop over an arm 31 writeBits step whose post-state charges
/// `cpu`, with semantics entry `index` the fixture's next step.
fn budget_routes(index: Int, cpu: Int) -> Bool {
  let arguments = write_bits_arguments(pattern(8, 0), [1, 2], True)
  let step = staged_step_at(
    80,
    arguments,
    semantic_bytes(builtin.write_bits(pattern(8, 0), [1, 2], True), False),
    cek_staged_arm_v1.StagedMaterialV1 { runs: no_staging().runs, sources: [] },
    [],
    machine.mode_builtin,
    5,
    2,
  )
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let charged = chain.CoreBound {
    ..bound,
    post: machine.MachineStateV1 { ..bound.post, cpu: bound.post.cpu + cpu },
  }
  let next: Data = chain.CoreBound { ..charged, progress: 3 }
  budget_hop.main.spend(
    list.map(
      list.range(0, 12),
      fn(i) {
        if i == index {
          fixture.next_step_script_hash
        } else {
          other_hash
        }
      },
    ),
    fixture.computation_thread_policy_id,
    Some(ct.StepDatum { ..datum, data: Some(charged) }),
    step.redeemer,
    fixture.own_out_ref(),
    Transaction {
      ..step.transaction,
      outputs: fixture.continue_mid_step_l1_tx_v1("cek-core-staged", next).outputs,
    },
  )
}

/// The loop charges writeBits: the budget hop routes it over any post-state.
test budget_hop_routes_write_bits_to_the_loop_uncharged() {
  and {
    budget_routes(12, 0),
    budget_routes(12, 123_456),
  }
}

test budget_hop_routes_write_bits_to_entry_12_only() fail {
  budget_routes(11, 0)
}

/// The budget hop over a direct (arm 30) writeBits step whose post-state
/// charges `extra` cpu past the builtin's own charge.
fn direct_budget(extra: Int) -> Bool {
  let input = pattern(8, 0)
  let arguments = write_bits_arguments(input, [1, 2], True)
  let result = bytes_value(builtin.write_bits(input, [1, 2], True), False)
  let arguments_data: Data = arguments
  let result_data: Data = result
  let witness = builtin.constr_data(
    30,
    [builtin.i_data(80), arguments_data, result_data],
  )
  let step = staged_step_at(
    80,
    arguments,
    result,
    StagedMaterialV1 { runs: no_staging().runs, sources: [] },
    [],
    machine.mode_builtin,
    3,
    1,
  )
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let budget = cek_builtin_v1.direct_builtin_budget_v1(80, arguments)
  let facts: Data = chain.BuiltinRoots {
    arguments_root: #"",
    arguments_count: 3,
    result_root: #"",
    builtin_root: #"",
  }
  let direct = chain.CoreBound {
    ..bound,
    arm: 30,
    witness_hash: blake2b_256(cbor.serialise(witness)),
    facts,
    post: machine.exact_state(
      bound.pre,
      machine.mode_return,
      #"",
      proof.empty_environment_root_v1,
      bound.pre.continuation_root,
      0,
      budget.cpu + extra,
      budget.memory,
    ),
  }
  let next: Data = chain.CoreBound { ..direct, progress: 2 }
  budget_hop.main.spend(
    list.map(
      list.range(0, 12),
      fn(i) {
        if i == 0 {
          fixture.next_step_script_hash
        } else {
          other_hash
        }
      },
    ),
    fixture.computation_thread_policy_id,
    Some(ct.StepDatum { ..datum, data: Some(direct) }),
    ct.Continue(chain.VerifyArm { input_index: 0, output_index: 0, witness }),
    fixture.own_out_ref(),
    Transaction {
      ..step.transaction,
      outputs: fixture.continue_mid_step_l1_tx_v1("cek-core-staged", next).outputs,
    },
  )
}

/// Only an arm 31 writeBits step skips the budget hop's charge: a direct one
/// is still charged there.
test budget_hop_charges_a_direct_write_bits_step() {
  direct_budget(0)
}

test budget_hop_refuses_a_direct_write_bits_step_charged_wrongly() fail {
  direct_budget(1)
}

/// The failure roots hop over an arm 36 writeBits step over `input`: the
/// fixture's next step is the loop's hash when `to_loop`, the staged failure
/// hop's otherwise.
fn failure_routes(input: ValueWitnessV1, to_loop: Bool) -> Bool {
  let arguments = [input, indices_value([0]), boolean_value(True)]
  let Pair(root, count) = cek_builtin_v1.arguments_root_v1(arguments)
  let pre = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: proof.hash_builtin_value_v1(80, 0, count, root),
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let step_material: Data = StagedMaterialV1 {
    runs: no_staging().runs,
    sources: [],
  }
  let step = failure_step_at(80, arguments, step_material, [], 36, 7, 0)
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let at_roots = chain.CoreBound { ..bound, pre, post: bound.post }
  let next: Data = chain.CoreBound { ..at_roots, progress: 1 }
  let Pair(staged_hop, loop_hop) = if to_loop {
    Pair(other_hash, fixture.next_step_script_hash)
  } else {
    Pair(fixture.next_step_script_hash, other_hash)
  }
  failure_roots.main.spend(
    other_hash,
    staged_hop,
    loop_hop,
    fixture.computation_thread_policy_id,
    Some(ct.StepDatum { ..datum, data: Some(at_roots) }),
    step.redeemer,
    fixture.own_out_ref(),
    Transaction {
      ..step.transaction,
      outputs: fixture.continue_mid_step_l1_tx_v1("cek-core-staged", next).outputs,
    },
  )
}

test routes_a_write_bits_failure_over_4096_bytes_to_the_loop() {
  and {
    failure_routes(semantic_bytes(pattern(4096, 0), False), True),
    failure_routes(compact(bytes_value(pattern(1, 0), False)), True),
  }
}

/// The loop reads only compacted arguments: an inline input goes on to the
/// staged failure hop, as any input other than a compacted one does.
test routes_a_write_bits_failure_over_an_inline_input_to_the_staged_hop() {
  and {
    failure_routes(bytes_value(pattern(4096, 0), False), False),
    failure_routes(bytes_value(pattern(1, 0), False), False),
  }
}

test does_not_route_a_write_bits_failure_over_an_inline_input_to_the_loop() fail {
  failure_routes(bytes_value(pattern(1, 0), False), True)
}

test does_not_route_a_write_bits_failure_over_4096_bytes_elsewhere() fail {
  failure_routes(semantic_bytes(pattern(4096, 0), False), False)
}

test routes_a_write_bits_failure_past_4096_bytes_to_the_staged_hop() {
  failure_routes(semantic_bytes(pattern(4097, 0), False), False)
}

test does_not_route_a_write_bits_failure_past_4096_bytes_to_the_loop() fail {
  failure_routes(semantic_bytes(pattern(4097, 0), False), True)
}

// ## The longest hops

/// 85 indices a hop over a 4,096-byte input: the hop the planner sizes to
/// its budget opens about this many.
const max_hop_indices = 85

fn max_indices() -> List<Int> {
  list.map(list.range(0, 2 * max_hop_indices - 1), fn(i) { i * 193 % 32768 })
}

fn max_case() -> WriteBitsCaseV1 {
  success_case(pattern(4096, 7), max_indices(), True)
}

fn max_failure_case() -> WriteBitsCaseV1 {
  failure_case(
    pattern(4096, 7),
    list.concat(list.take(max_indices(), 2 * max_hop_indices - 1), [32768]),
  )
}

fn max_first_step() -> WriteBitsStepV1 {
  hop_step(max_case(), nth(plan(max_case(), max_hop_indices), 0))
}

fn max_last_step() -> WriteBitsStepV1 {
  hop_step(max_case(), nth(plan(max_case(), max_hop_indices), 1))
}

fn max_failure_step() -> WriteBitsStepV1 {
  hop_step(
    max_failure_case(),
    nth(plan(max_failure_case(), max_hop_indices), 1),
  )
}

fn built(step: WriteBitsStepV1) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}

test write_bits_loop_first_hop_at_the_longest_input() {
  spend(max_first_step())
}

test write_bits_loop_first_hop_fixture_only_at_the_longest_input() {
  built(max_first_step())
}

test write_bits_loop_last_hop_at_the_longest_input() {
  spend(max_last_step())
}

test write_bits_loop_last_hop_fixture_only_at_the_longest_input() {
  built(max_last_step())
}

test write_bits_loop_failure_hop_at_the_longest_input() {
  spend(max_failure_step())
}

test write_bits_loop_failure_hop_fixture_only_at_the_longest_input() {
  built(max_failure_step())
}

/// writeBits itself refuses an input past 4,096 bytes, so the loop, which
/// applies it on every hop, has no check of its own.
test write_bits_refuses_an_input_past_4096_bytes() fail {
  builtin.write_bits(pattern(4097, 0), [0], True) != #""
}

test the_max_case_needs_two_hops() {
  and {
    list.length(plan(max_case(), max_hop_indices)) == 2,
    list.length(plan(max_failure_case(), max_hop_indices)) == 2,
  }
}
