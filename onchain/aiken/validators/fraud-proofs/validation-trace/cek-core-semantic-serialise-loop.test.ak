//// Transaction-level rows for the serialiseData loop (arm 31, group 5,
//// semantics index 10). The result's bytes are staged as runs; each hop reads
//// a window of them with the resumable canonical reader, and the hop that
//// summarises the value checks the argument's summary and the result's root,
//// length and memory. Any hop split an honest prover picks completes; every
//// other byte string, staging or hand-on is refused.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/transaction.{Input, Transaction}
use fraud_proofs/validation_trace/cek_core_semantic_serialise_loop as serialise_loop
use midgard/cek_builtin_v1.{SemanticConstantValue}
use midgard/cek_data_v1
use midgard/cek_machine_v1 as machine
use midgard/cek_semantic_chain as chain
use midgard/cek_serialise_loop_v1.{
  CarryV1, SerialiseHop, SerialiseHopV1, SerialiseStateV1, advance_serialise_v1,
}
use midgard/cek_staged_blob_v1.{flat_root_v1}
use midgard/computation_thread as ct
use midgard/data_serialise_walk_v1.{Item, Leaf}
use midgard/fraud_proofs/cek_serialise_fixture_v1.{
  PlannedHopV1, SerialiseCaseV1, SerialiseStepV1, bytes_result, case_of,
  data_value, honest_case, hop_step, hop_step_over, plan,
}
use midgard/fraud_proofs/cek_staged_fixture_v1.{loop_facts, staging_input}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

fn spend(step: SerialiseStepV1) -> Bool {
  serialise_loop.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// Hop `planned` handing on the state the loop computes from it, as a prover
/// writes it: only the loop's own checks can refuse the hop.
fn spend_as_computed(case: SerialiseCaseV1, planned: PlannedHopV1) -> Bool {
  let next = advance_serialise_v1(
    case.facts,
    planned.hop,
    case.reference_inputs,
    planned.from,
    planned.end,
  )
  spend(hop_step(case, PlannedHopV1 { ..planned, next }))
}

/// Every planned hop of `case` at `limit` steps per hop is accepted.
fn completes(case: SerialiseCaseV1, limit: Int) -> Bool {
  list.all(plan(case, limit), fn(planned) { spend(hop_step(case, planned)) })
}

// ## Values

fn int(value: Int) -> Data {
  builtin.i_data(value)
}

fn block(length: Int) -> ByteArray {
  list.foldl(
    list.range(1, length),
    #"",
    fn(i, acc) { bytearray.push(acc, i % 256) },
  )
}

fn bytes_of(length: Int) -> Data {
  builtin.b_data(block(length))
}

fn nested(depth: Int) -> Data {
  if depth == 0 {
    int(0)
  } else {
    builtin.list_data([nested(depth - 1)])
  }
}

fn flat(count: Int) -> Data {
  builtin.list_data(list.map(list.range(1, count), fn(_) { int(0) }))
}

fn power(base: Int, exponent: Int) -> Int {
  if exponent == 0 {
    1
  } else {
    base * power(base, exponent - 1)
  }
}

fn mixed() -> Data {
  builtin.constr_data(
    1280,
    [
      builtin.map_data(
        [
          Pair(bytes_of(70), nested(4)),
          Pair(int(power(2, 64)), builtin.constr_data(2, [flat(5)])),
          Pair(bytes_of(300), int(-power(256, 100))),
        ],
      ),
      int(-power(2, 512)),
      builtin.constr_data(18446744073709551615, [nested(3)]),
    ],
  )
}

/// Eight records shaped like transaction inputs.
fn input_records() -> Data {
  builtin.list_data(
    list.map(
      list.range(0, 7),
      fn(i) { builtin.constr_data(0, [bytes_of(32), int(i)]) },
    ),
  )
}

/// A value whose CBOR is past the old one-step bound (9,215 bytes).
fn past_the_bound() -> Data {
  builtin.list_data([bytes_of(5000), bytes_of(4200), int(7)])
}

/// A byte string past the semantic summary's leaf bound, with its case.
fn long_leaf_case(length: Int) -> SerialiseCaseV1 {
  let content = block(length)
  let bytes = cbor.serialise(builtin.b_data(content))
  case_of(
    data_value(cek_data_v1.bytes_data_summary_v1(length, flat_root_v1(content))),
    bytes_result([bytes]),
    [bytes],
  )
}

// ## Completion

test completes_a_small_value_in_one_hop() {
  let case = honest_case(int(0))
  expect [only] = plan(case, 100)
  spend(hop_step(case, only))
}

test completes_a_mixed_value_under_any_hop_split() {
  let case = honest_case(mixed())
  list.all([1, 2, 7, 40, 10_000], fn(limit) { completes(case, limit) })
}

test completes_a_value_past_the_old_bound() {
  let case = honest_case(past_the_bound())
  and {
    list.length(case.runs) == 1,
    completes(case, 3),
    completes(case, 10_000),
  }
}

test completes_a_list_of_128_items() {
  completes(honest_case(flat(128)), 50)
}

test completes_a_value_nested_128_deep() {
  completes(honest_case(nested(128)), 50)
}

test completes_eight_input_records() {
  completes(honest_case(input_records()), 20)
}

test completes_a_leaf_past_the_summary_bound_across_runs() {
  let case = long_leaf_case(30000)
  and {
    list.length(case.runs) == 3,
    completes(case, 2),
    completes(case, 1_000),
  }
}

/// A hop may name its staging outputs in any ledger order: `order` gives
/// byte order.
test reads_runs_named_out_of_ledger_order() {
  let case = long_leaf_case(30000)
  let planned = whole_hop(case)
  expect [a, b, c] = case.refs
  expect [other, ia, ib, ic] = case.reference_inputs
  let hop = SerialiseHopV1 { ..planned.hop, refs: [c, b, a], order: [2, 1, 0] }
  spend(
    hop_step_over(case, PlannedHopV1 { ..planned, hop }, [other, ic, ib, ia]),
  )
}

// ## Refused bytes

/// The single hop over every staged run of `case`, from the start, claiming
/// to end the loop, with as many steps as the step bound allows.
fn whole_hop(case: SerialiseCaseV1) -> PlannedHopV1 {
  let length = list.foldl(
    list.flat_map(case.runs, fn(run) { run }),
    0,
    fn(chunk, total) { total + bytearray.length(chunk) },
  )
  PlannedHopV1 {
    from: None,
    end: 16 * cek_data_v1.bytes_memory_size_v1(length),
    hop: SerialiseHopV1 {
      refs: case.refs,
      order: list.range(0, list.length(case.runs) - 1),
      restore: [],
      base: #"",
    },
    next: None,
  }
}

/// The step of `data` whose result claims the bytes `staged`.
fn claims(data: Data, staged: ByteArray) -> Bool {
  let case = case_of(
    data_value(cek_data_v1.semantic_data_summary_v1(data)),
    bytes_result([staged]),
    [staged],
  )
  spend(hop_step(case, whole_hop(case)))
}

test the_canonical_bytes_end_the_loop_in_one_hop() {
  claims(mixed(), cbor.serialise(mixed()))
}

test refuses_a_non_minimal_integer_head() fail {
  claims(int(23), #"1817")
}

test refuses_a_definite_long_byte_string() fail {
  claims(bytes_of(65), bytearray.concat(#"5841", block(65)))
}

test refuses_an_indefinite_short_byte_string() fail {
  claims(bytes_of(3), #"5f43010203ff")
}

test refuses_a_byte_string_with_a_short_inner_chunk() fail {
  claims(
    bytes_of(70),
    #"5f"
      |> bytearray.concat(cbor.serialise(builtin.b_data(block(6))))
      |> bytearray.concat(
          cbor.serialise(
            builtin.b_data(builtin.slice_bytearray(6, 64, block(70))),
          ),
        )
      |> bytearray.concat(#"ff"),
  )
}

test refuses_a_bignum_below_nine_bytes() fail {
  claims(int(power(2, 63)), #"c2488000000000000000")
}

test refuses_a_definite_field_list() fail {
  claims(builtin.constr_data(0, [int(0)]), #"d8798100")
}

test refuses_a_large_tag_form_for_a_small_index() fail {
  claims(builtin.constr_data(7, []), #"d866820780")
}

test refuses_an_indefinite_map() fail {
  claims(builtin.map_data([Pair(int(0), int(1))]), #"bf0001ff")
}

test refuses_trailing_bytes() fail {
  claims(mixed(), bytearray.concat(cbor.serialise(mixed()), #"00"))
}

test refuses_a_truncated_tail() fail {
  let bytes = cbor.serialise(mixed())
  claims(
    mixed(),
    builtin.slice_bytearray(0, bytearray.length(bytes) - 1, bytes),
  )
}

/// The canonical bytes of another value.
test refuses_the_bytes_of_another_value() fail {
  claims(mixed(), cbor.serialise(nested(3)))
}

/// The canonical bytes of another value of the same memory: only the
/// summary tells them apart.
test refuses_the_bytes_of_another_value_of_the_same_memory() fail {
  expect
    cek_data_v1.semantic_data_summary_v1(int(0)).memory == cek_data_v1.semantic_data_summary_v1(
      int(1),
    ).memory
  claims(int(0), cbor.serialise(int(1)))
}

/// The 200 bytes of `block(200)` as an indefinite byte string whose last,
/// 8-byte chunk has the head `last_head`.
fn chunked_block(last_head: ByteArray) -> ByteArray {
  let content = block(200)
  let full = fn(offset) {
    bytearray.concat(#"5840", builtin.slice_bytearray(offset, 64, content))
  }
  #"5f"
    |> bytearray.concat(full(0))
    |> bytearray.concat(full(64))
    |> bytearray.concat(full(128))
    |> bytearray.concat(last_head)
    |> bytearray.concat(builtin.slice_bytearray(192, 8, content))
    |> bytearray.concat(#"ff")
}

fn short_chunk_value() -> Data {
  builtin.list_data([int(1), bytes_of(200), int(2), int(3)])
}

/// `short_chunk_value()` staged with its last leaf chunk behind the
/// non-minimal head 0x58 0x08 instead of 0x48.
fn long_head_case() -> SerialiseCaseV1 {
  let value = short_chunk_value()
  let wrap = fn(leaf) {
    #"9f01" |> bytearray.concat(leaf) |> bytearray.concat(#"0203ff")
  }
  expect cbor.serialise(value) == wrap(chunked_block(#"48"))
  let staged = wrap(chunked_block(#"5808"))
  case_of(
    data_value(cek_data_v1.semantic_data_summary_v1(value)),
    bytes_result([staged]),
    [staged],
  )
}

fn but_last(items: List<a>) -> List<a> {
  list.take(items, list.length(items) - 1)
}

/// The reader reads a short chunk behind a 0x58 head: every hop before the
/// one that ends the loop, as many as the honest staging has, is accepted.
test reads_a_short_chunk_behind_a_long_head_up_to_the_exit() {
  let case = long_head_case()
  let hops = plan(case, 1)
  expect
    list.length(hops) == list.length(plan(honest_case(short_chunk_value()), 1))
  expect list.length(hops) > 3
  list.all(but_last(hops), fn(planned) { spend(hop_step(case, planned)) })
}

/// The hop that ends the loop refuses it: the summary read counts the extra
/// head byte, so it is not the argument's.
test refuses_a_short_chunk_behind_a_long_head_at_the_exit() fail {
  let case = long_head_case()
  expect Some(last) = list.last(plan(case, 1))
  spend(hop_step(case, last))
}

// ## Refused exits

fn exit_with(argument, result) -> Bool {
  let bytes = cbor.serialise(mixed())
  let case = case_of(argument, result, [bytes])
  spend(hop_step(case, whole_hop(case)))
}

fn mixed_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.semantic_data_summary_v1(mixed())
}

test refuses_an_argument_of_another_type() fail {
  exit_with(
    SemanticConstantValue {
      type_cbor: #"9f00ff",
      payload: mixed_summary(),
      memory: mixed_summary().memory,
    },
    bytes_result([cbor.serialise(mixed())]),
  )
}

test refuses_an_argument_with_the_wrong_memory() fail {
  exit_with(
    SemanticConstantValue {
      type_cbor: #"9f08ff",
      payload: mixed_summary(),
      memory: mixed_summary().memory + 1,
    },
    bytes_result([cbor.serialise(mixed())]),
  )
}

fn with_result(
  edit: fn(SemanticConstantValueParts) -> SemanticConstantValueParts,
) -> Bool {
  expect SemanticConstantValue { type_cbor, payload, memory } = bytes_result(
    [cbor.serialise(mixed())],
  )
  let parts = edit(SemanticConstantValueParts { type_cbor, payload, memory })
  exit_with(
    data_value(mixed_summary()),
    SemanticConstantValue {
      type_cbor: parts.type_cbor,
      payload: parts.payload,
      memory: parts.memory,
    },
  )
}

type SemanticConstantValueParts {
  type_cbor: ByteArray,
  payload: cek_data_v1.DataSummaryV1,
  memory: Int,
}

test accepts_the_honest_result() {
  with_result(fn(parts) { parts })
}

test refuses_a_result_of_another_type() fail {
  with_result(
    fn(parts) { SemanticConstantValueParts { ..parts, type_cbor: #"9f02ff" } },
  )
}

test refuses_a_result_with_the_wrong_memory() fail {
  with_result(
    fn(parts) {
      SemanticConstantValueParts { ..parts, memory: parts.memory + 1 }
    },
  )
}

test refuses_a_result_with_the_wrong_cbor_length() fail {
  with_result(
    fn(parts) {
      SemanticConstantValueParts {
        ..parts,
        payload: cek_data_v1.DataSummaryV1 {
          ..parts.payload,
          cbor_length: parts.payload.cbor_length + 1,
        },
      }
    },
  )
}

// ## Refused staging

/// A case of `mixed()` in several runs, with steps small enough to need
/// several hops.
fn multi_run_case() -> SerialiseCaseV1 {
  long_leaf_case(30000)
}

test refuses_runs_in_the_wrong_order() fail {
  let case = multi_run_case()
  let hop = whole_hop(case).hop
  spend(
    hop_step(
      case,
      PlannedHopV1 {
        ..whole_hop(case),
        hop: SerialiseHopV1 { ..hop, order: [1, 0, 2] },
      },
    ),
  )
}

test refuses_a_run_read_twice() fail {
  let case = multi_run_case()
  let hop = whole_hop(case).hop
  spend(
    hop_step(
      case,
      PlannedHopV1 {
        ..whole_hop(case),
        hop: SerialiseHopV1 { ..hop, order: [0, 1, 1, 2] },
      },
    ),
  )
}

test refuses_a_loop_that_skips_its_last_run() fail {
  let case = multi_run_case()
  let hop = whole_hop(case).hop
  spend(
    hop_step(
      case,
      PlannedHopV1 {
        ..whole_hop(case),
        hop: SerialiseHopV1 { ..hop, order: [0, 1] },
      },
    ),
  )
}

test the_whole_staging_ends_the_loop_in_one_hop() {
  let case = multi_run_case()
  spend(hop_step(case, whole_hop(case)))
}

/// The leaf's bytes staged in chunks that are not 4,095 bytes: the flat root
/// is over 4,095-byte chunks, so the staging is refused.
test refuses_a_mis_split_staging() fail {
  let content = block(9000)
  let bytes = cbor.serialise(builtin.b_data(content))
  let length = bytearray.length(bytes)
  let chunks = [
    builtin.slice_bytearray(0, 4000, bytes),
    builtin.slice_bytearray(4000, length - 4000, bytes),
  ]
  let input = staging_input(10, chunks)
  let argument = data_value(
    cek_data_v1.bytes_data_summary_v1(9000, flat_root_v1(content)),
  )
  let base = case_of(argument, bytes_result([bytes]), [bytes])
  let case = SerialiseCaseV1 {
    ..base,
    reference_inputs: [input],
    refs: [input.output_reference],
    runs: [chunks],
  }
  spend(hop_step(case, whole_hop(case)))
}

// ## Hops and hand-ons

fn mid_case() -> SerialiseCaseV1 {
  honest_case(mixed())
}

fn mid_plan() -> List<PlannedHopV1> {
  plan(mid_case(), 7)
}

fn nth(hops: List<PlannedHopV1>, index: Int) -> PlannedHopV1 {
  expect Some(planned) = list.at(hops, index)
  planned
}

test the_mid_plan_has_several_hops() {
  list.length(mid_plan()) > 5
}

/// A hop that reads nothing hands on the state it started from.
test refuses_a_hop_that_ends_at_its_cursor() fail {
  let planned = nth(mid_plan(), 2)
  expect Some(Pair(cursor, _)) = planned.from
  spend_as_computed(mid_case(), PlannedHopV1 { ..planned, end: cursor })
}

test refuses_a_first_hop_that_ends_at_zero() fail {
  let planned = nth(mid_plan(), 0)
  spend_as_computed(mid_case(), PlannedHopV1 { ..planned, end: 0 })
}

/// A hop replayed from the state it handed on: its range is empty.
test refuses_a_replayed_hop() fail {
  let planned = nth(mid_plan(), 2)
  expect Some(next) = planned.next
  spend(
    hop_step(
      mid_case(),
      PlannedHopV1 { ..planned, from: Some(Pair(planned.end, next)) },
    ),
  )
}

test refuses_a_hop_past_the_step_bound() fail {
  let case = honest_case(int(0))
  let planned = whole_hop(case)
  // 16 steps per result word; `00` is one word.
  spend(hop_step(case, PlannedHopV1 { ..planned, end: 17 }))
}

test admits_a_hop_at_the_step_bound() {
  let case = honest_case(int(0))
  spend(hop_step(case, PlannedHopV1 { ..whole_hop(case), end: 16 }))
}

test refuses_a_last_hop_that_carries_on() fail {
  let hops = mid_plan()
  let last = nth(hops, list.length(hops) - 1)
  let before = nth(hops, list.length(hops) - 2)
  spend(hop_step(mid_case(), PlannedHopV1 { ..last, next: before.next }))
}

test refuses_a_hop_that_claims_the_end_early() fail {
  let planned = nth(mid_plan(), 2)
  spend(hop_step(mid_case(), PlannedHopV1 { ..planned, next: None }))
}

fn with_next(
  planned: PlannedHopV1,
  edit: fn(SerialiseStateV1) -> SerialiseStateV1,
) -> Bool {
  expect Some(next) = planned.next
  expect state: SerialiseStateV1 = next
  let edited: Data = edit(state)
  spend(hop_step(mid_case(), PlannedHopV1 { ..planned, next: Some(edited) }))
}

test accepts_the_honest_hand_on() {
  with_next(nth(mid_plan(), 3), fn(state) { state })
}

test refuses_a_hand_on_with_a_wrong_stack_root() fail {
  with_next(
    nth(mid_plan(), 3),
    fn(state) { SerialiseStateV1 { ..state, stack_root: #"00" } },
  )
}

test refuses_a_hand_on_with_a_wrong_carry() fail {
  with_next(
    nth(mid_plan(), 3),
    fn(state) {
      SerialiseStateV1 {
        ..state,
        carry: when state.carry is {
          Some(CarryV1 { run, offset }) ->
            Some(CarryV1 { run, offset: offset + 1 })
          None -> Some(CarryV1 { run: nth_ref(mid_case(), 0), offset: 0 })
        },
      }
    },
  )
}

test refuses_a_hand_on_with_a_wrong_frontier() fail {
  with_next(
    nth(mid_plan(), 3),
    fn(state) {
      SerialiseStateV1 {
        ..state,
        frontier: data_serialise_walk_v1.FrontierV1 {
          ..state.frontier,
          length: state.frontier.length + 1,
        },
      }
    },
  )
}

/// A restore that does not chain to the stack root: one more entry under
/// the honest ones.
test refuses_a_wrong_restore() fail {
  let planned = nth(mid_plan(), 3)
  let extra = Item(cek_data_v1.semantic_data_summary_v1(int(0)))
  spend_as_computed(
    mid_case(),
    PlannedHopV1 {
      ..planned,
      hop: SerialiseHopV1 {
        ..planned.hop,
        restore: list.concat(planned.hop.restore, [extra]),
      },
    },
  )
}

test refuses_a_wrong_base() fail {
  let planned = nth(mid_plan(), 3)
  spend_as_computed(
    mid_case(),
    PlannedHopV1 {
      ..planned,
      hop: SerialiseHopV1 { ..planned.hop, base: #"00" },
    },
  )
}

/// 200 byte strings of 61 bytes: each item is 63 bytes of CBOR, which
/// divides a run's 12,285 bytes, so the items lie at the same offsets in
/// every run and a tail read from another run still parses.
fn aligned_items() -> Data {
  builtin.list_data(
    list.map(
      list.range(1, 200),
      fn(i) { builtin.b_data(bytearray.push(block(60), i)) },
    ),
  )
}

/// A hop whose tail does not name the carried run.
test refuses_a_tail_from_another_run() fail {
  let case = honest_case(aligned_items())
  expect list.length(case.runs) == 2
  let hops = plan(case, 3)
  let planned = list.foldl(
    hops,
    None,
    fn(planned, found) {
      when found is {
        Some(_) -> found
        None ->
          when planned.from is {
            Some(Pair(_, data)) -> {
              expect state: SerialiseStateV1 = data
              when state.carry is {
                Some(CarryV1 { run, .. }) ->
                  if run != nth_ref(case, 0) {
                    Some(planned)
                  } else {
                    None
                  }
                None -> None
              }
            }
            None -> None
          }
      }
    },
  )
  expect Some(planned) = planned
  expect [_, ..rest] = planned.hop.order
  spend_as_computed(
    case,
    PlannedHopV1 {
      ..planned,
      hop: SerialiseHopV1 { ..planned.hop, order: [0, ..rest] },
    },
  )
}

/// A hop that names a run past the one its reading stops in: the tail it
/// hands on would not lie in its last run.
test refuses_a_hop_that_names_a_run_it_does_not_reach() fail {
  let case = long_leaf_case(30000)
  spend_as_computed(case, PlannedHopV1 { ..whole_hop(case), end: 3 })
}

fn nth_ref(case: SerialiseCaseV1, index: Int) {
  expect Some(reference) = list.at(case.refs, index)
  reference
}

test a_long_leaf_hands_on_a_leaf_frame() {
  let case = long_leaf_case(30000)
  expect Some(next) = nth(plan(case, 3), 0).next
  expect state: SerialiseStateV1 = next
  when state.frame is {
    Leaf { .. } -> True
    _ -> False
  }
}

test refuses_a_leaf_hand_on_with_wrong_pending_bytes() fail {
  let case = long_leaf_case(30000)
  let planned = nth(plan(case, 3), 0)
  expect Some(next) = planned.next
  expect state: SerialiseStateV1 = next
  expect Leaf { parent, tag, leaf, pending, length, cbor_length, all_ff } = state.frame
  let edited: Data = SerialiseStateV1 {
    ..state,
    frame: Leaf {
      parent,
      tag,
      leaf,
      pending: bytearray.push(pending, 0),
      length,
      cbor_length,
      all_ff,
    },
  }
  spend(hop_step(case, PlannedHopV1 { ..planned, next: Some(edited) }))
}

test refuses_a_hop_output_that_is_not_the_loop_facts() fail {
  let planned = nth(mid_plan(), 2)
  let step = hop_step(mid_case(), planned)
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  expect Some(next) = planned.next
  let output: Data = chain.CoreBound {
    ..bound,
    facts: loop_facts(mid_case().facts, planned.end + 1, next),
  }
  spend(
    SerialiseStepV1 {
      ..step,
      transaction: Transaction {
        ..step.transaction,
        outputs: fixture.continue_same_step_l1_tx_v1("cek-core-staged", output).outputs,
      },
    },
  )
}

test refuses_a_witness_that_is_not_bound() fail {
  let planned = nth(mid_plan(), 0)
  let step = hop_step(mid_case(), planned)
  expect ct.Continue(SerialiseHop { input_index, output_index, end, hop, .. }) = step.redeemer
  spend(
    SerialiseStepV1 {
      ..step,
      redeemer: ct.Continue(
        SerialiseHop {
          input_index,
          output_index,
          witness: builtin.constr_data(31, []),
          end,
          hop,
        },
      ),
    },
  )
}

/// Hop 2 of the mid plan over its bound edited by `edit`, in the datum it
/// spends and the hand-on alike.
fn spend_bound(edit: fn(chain.CoreBound) -> chain.CoreBound) -> Bool {
  let planned = nth(mid_plan(), 2)
  let step = hop_step(mid_case(), planned)
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  expect Some(next) = planned.next
  let edited = edit(bound)
  let output: Data = chain.CoreBound {
    ..edited,
    facts: loop_facts(mid_case().facts, planned.end, next),
  }
  spend(
    SerialiseStepV1 {
      ..step,
      datum: Some(ct.StepDatum { ..datum, data: Some(edited) }),
      transaction: Transaction {
        ..step.transaction,
        outputs: fixture.continue_same_step_l1_tx_v1("cek-core-staged", output).outputs,
      },
    },
  )
}

test accepts_the_honest_bound() {
  spend_bound(fn(bound) { bound })
}

test refuses_a_step_of_another_group() fail {
  spend_bound(fn(bound) { chain.CoreBound { ..bound, group: 4 } })
}

test refuses_a_step_in_another_mode() fail {
  spend_bound(
    fn(bound) {
      chain.CoreBound {
        ..bound,
        pre: machine.MachineStateV1 {
          ..bound.pre,
          mode: machine.mode_semantic_builtin,
        },
      }
    },
  )
}

test refuses_a_step_of_another_arm() fail {
  spend_bound(fn(bound) { chain.CoreBound { ..bound, arm: 30 } })
}

// ## Routing

/// Every builtin tag the budget hop routes to a semantics entry. For any
/// other tag `semantic_group` fails, so the step reaches no entry.
fn routed_tags() -> List<Int> {
  list.concat(
    list.range(0, 20),
    list.concat(
      list.range(22, 25),
      list.concat(
        list.range(29, 37),
        list.concat(
          list.range(39, 42),
          list.concat(list.range(44, 51), [71, 72, 86]),
        ),
      ),
    ),
  )
}

/// The loop does not check the builtin tag itself: it relies on semantics
/// entry 10 taking serialiseData (tag 51) alone. Arms other than 31 route to
/// entry 0.
test only_serialise_data_routes_to_the_serialise_loop() {
  list.all(
    routed_tags(),
    fn(tag) {
      if tag == 51 {
        chain.semantic_group(tag) == 10
      } else {
        chain.semantic_group(tag) != 10
      }
    },
  )
}

test routes_no_semantics_for_an_unrouted_tag() fail {
  chain.semantic_group(52) >= 0
}
