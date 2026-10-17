//// InputSets' descending merge of fields 0 and 1: from one honest pre-state
//// exactly one successor verifies. The vectors are shared with the TypeScript
//// twin's test (`demo/midgard-validation/tests/input-set-merge.test.ts`): the
//// same fields give the same controls, schedule and duplicate step.

use aiken/collection/list
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/rejection_reason_v1
use midgard/validation_machine/input_sets as vm_input_sets
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{h_a, h_b, h_c}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, exact_rejection_post, initial_signatures_witness,
  inline_field_carriage, inline_field_door, input_sets_pre,
  input_sets_scan_witness,
}
use midgard/validation_trace_v1

type Scan {
  spend_count: Int,
  reference_count: Int,
  spend_seen: Int,
  reference_seen: Int,
  last_key: ByteArray,
  last_field: Int,
  last_item: Int,
  cached_other_head: ByteArray,
  schedule: ByteArray,
}

fn key(tx_id: ByteArray) -> ByteArray {
  encode_midgard_tx_input(MidgardTxInput { tx_id, output_index: 0 })
}

fn transaction(
  spend: List<ByteArray>,
  reference: List<ByteArray>,
) -> MidgardTransaction {
  let base = empty_v1_transaction()
  let input = fn(tx_id) { MidgardTxInput { tx_id, output_index: 0 } }
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: spend |> list.map(input),
      reference_inputs: reference |> list.map(input),
    },
  }
}

fn witness_of(tx: MidgardTransaction, scan: Scan) -> ByteArray {
  input_sets_scan_witness(
    tx,
    scan.spend_count,
    scan.reference_count,
    scan.spend_seen,
    scan.reference_seen,
    scan.last_key,
    scan.last_field,
    scan.last_item,
    scan.cached_other_head,
    scan.schedule,
  )
}

fn pre_of(tx: MidgardTransaction, scan: Scan) {
  input_sets_pre(tx, 2, witness_of(tx, scan))
}

fn advance(tx: MidgardTransaction, from: Scan, to: Scan) {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre_of(tx, from),
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.InputSets,
      3,
      witness_of(tx, to),
    ),
  }
}

fn to_signatures(tx: MidgardTransaction, from: Scan) {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre_of(tx, from),
    phase: validation_trace_v1.Signatures,
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      3,
      initial_signatures_witness(tx),
    ),
  }
}

fn duplicate(tx: MidgardTransaction, from: Scan) {
  exact_rejection_post(
    pre_of(tx, from),
    rejection_reason_v1.DuplicateInput {
      first_field_index: -1,
      first_item_index: -1,
      second_field_index: -1,
      second_item_index: -1,
    },
  )
}

fn step(
  tx: MidgardTransaction,
  from: Scan,
  claimed_successor: validation_trace_v1.ValidationMachineStateV1,
  carriage: FieldCarriageV1,
) -> Bool {
  vm_input_sets.verify_input_sets_item_semantics_v1(
    pre_of(tx, from),
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: witness_of(tx, from),
      claimed_successor,
    },
    inline_field_door(),
    carriage,
  )
}

fn empty() -> ByteArray {
  vm_witness.empty_resolution_schedule_hash()
}

fn node(field: Int, item: ByteArray, next: ByteArray) -> ByteArray {
  vm_witness.resolution_schedule_node_hash(field, item, next)
}

fn start(reference_count: Int) -> Scan {
  Scan {
    spend_count: -1,
    reference_count,
    spend_seen: 0,
    reference_seen: 0,
    last_key: #"",
    last_field: -1,
    last_item: -1,
    cached_other_head: #"",
    schedule: empty(),
  }
}

// Honest run: field 0 = [a, c], field 1 = [b]. Seed, then c (0, 1), b (1, 0),
// a (0, 0), then Signatures with the ascending schedule.
fn run_spend() -> List<ByteArray> {
  [key(h_a), key(h_c)]
}

fn run_reference() -> List<ByteArray> {
  [key(h_b)]
}

fn run_tx() -> MidgardTransaction {
  transaction([h_a, h_c], [h_b])
}

fn seeded() -> Scan {
  Scan { ..start(-1), spend_count: 2, cached_other_head: key(h_c) }
}

fn after_c() -> Scan {
  Scan {
    spend_count: 2,
    reference_count: 1,
    spend_seen: 1,
    reference_seen: 0,
    last_key: key(h_c),
    last_field: 0,
    last_item: 1,
    cached_other_head: key(h_b),
    schedule: node(0, key(h_c), empty()),
  }
}

fn after_b() -> Scan {
  Scan {
    ..after_c(),
    reference_seen: 1,
    last_key: key(h_b),
    last_field: 1,
    last_item: 0,
    cached_other_head: #"",
    schedule: node(1, key(h_b), after_c().schedule),
  }
}

test input_set_merge_seed_caches_the_field_zero_tail() {
  let tx = run_tx()
  step(
    tx,
    start(-1),
    advance(tx, start(-1), seeded()),
    inline_field_carriage(run_spend()),
  )
}

test input_set_merge_consumes_the_larger_head() {
  let tx = run_tx()
  step(
    tx,
    seeded(),
    advance(tx, seeded(), after_c()),
    inline_field_carriage(run_reference()),
  )
}

test input_set_merge_refuses_the_smaller_head() {
  let tx = run_tx()
  let skipped = Scan {
    ..seeded(),
    reference_count: 1,
    reference_seen: 1,
    last_key: key(h_b),
    last_field: 1,
    last_item: 0,
    cached_other_head: key(h_c),
    schedule: node(1, key(h_b), empty()),
  }
  !step(
    tx,
    seeded(),
    advance(tx, seeded(), skipped),
    inline_field_carriage(run_reference()),
  )
}

test input_set_merge_refuses_a_skipped_item() {
  let tx = run_tx()
  let skipped = Scan {
    ..after_c(),
    spend_seen: 2,
    last_key: key(h_a),
    last_item: 0,
    schedule: node(0, key(h_a), after_c().schedule),
  }
  !step(
    tx,
    after_c(),
    advance(tx, after_c(), skipped),
    inline_field_carriage(run_spend()),
  )
}

test input_set_merge_refuses_a_reopened_item() {
  let tx = run_tx()
  let reopened = Scan {
    ..after_c(),
    spend_seen: 2,
    last_item: 1,
    schedule: node(0, key(h_c), after_c().schedule),
  }
  !step(
    tx,
    after_c(),
    advance(tx, after_c(), reopened),
    inline_field_carriage(run_spend()),
  )
}

test input_set_merge_opens_only_the_derived_field() fail {
  let tx = run_tx()
  step(
    tx,
    seeded(),
    advance(tx, seeded(), after_c()),
    inline_field_carriage(run_spend()),
  )
}

test input_set_merge_consumes_the_cached_head() {
  let tx = run_tx()
  step(
    tx,
    after_c(),
    advance(tx, after_c(), after_b()),
    inline_field_carriage(run_spend()),
  )
}

test input_set_merge_reopens_the_other_field_and_completes() {
  let tx = run_tx()
  let final_schedule = node(0, key(h_a), after_b().schedule)
  and {
    step(
      tx,
      after_b(),
      to_signatures(tx, after_b()),
      inline_field_carriage(run_spend()),
    ),
    final_schedule == vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

test input_set_merge_refuses_a_duplicate_on_a_distinct_run() {
  let tx = run_tx()
  !step(
    tx,
    after_c(),
    duplicate(tx, after_c()),
    inline_field_carriage(run_spend()),
  )
}

// A single field consumes its tail directly when field 1 is empty.
test input_set_merge_without_reference_inputs_needs_no_seed() {
  let tx = transaction([h_a, h_b], [])
  let after = Scan {
    ..start(0),
    spend_count: 2,
    spend_seen: 1,
    last_key: key(h_b),
    last_field: 0,
    last_item: 1,
    schedule: node(0, key(h_b), empty()),
  }
  step(
    tx,
    start(0),
    advance(tx, start(0), after),
    inline_field_carriage([key(h_a), key(h_b)]),
  )
}

// Field 0 = [a, b, b], field 1 = [c, c]: c (1, 1) is consumed, then field 1's
// next head c equals `last_key`. The duplicate is (1, 1) and (1, 0).
fn repeat_tx() -> MidgardTransaction {
  transaction([h_a, h_b, h_b], [h_c, h_c])
}

fn repeat_pre() -> Scan {
  Scan {
    spend_count: 3,
    reference_count: 2,
    spend_seen: 0,
    reference_seen: 1,
    last_key: key(h_c),
    last_field: 1,
    last_item: 1,
    cached_other_head: key(h_b),
    schedule: node(1, key(h_c), empty()),
  }
}

test input_set_merge_reaches_the_field_one_repeat() {
  let tx = repeat_tx()
  let seeded_repeat = Scan {
    ..start(-1),
    spend_count: 3,
    cached_other_head: key(h_b),
  }
  step(
    tx,
    seeded_repeat,
    advance(tx, seeded_repeat, repeat_pre()),
    inline_field_carriage([key(h_c), key(h_c)]),
  )
}

test input_set_merge_rejects_a_repeat_equal_to_the_last_key() {
  let tx = repeat_tx()
  step(
    tx,
    repeat_pre(),
    duplicate(tx, repeat_pre()),
    inline_field_carriage([key(h_c), key(h_c)]),
  )
}

test input_set_merge_refuses_to_consume_a_repeat() {
  let tx = repeat_tx()
  let consumed = Scan {
    ..repeat_pre(),
    reference_seen: 2,
    last_item: 0,
    schedule: node(1, key(h_c), repeat_pre().schedule),
  }
  !step(
    tx,
    repeat_pre(),
    advance(tx, repeat_pre(), consumed),
    inline_field_carriage([key(h_c), key(h_c)]),
  )
}

// Field 0 = [a, c], field 1 = [b, c]: the seed caches c (0, 1) and field 1's
// head is c (1, 1). The duplicate is (0, 1) and (1, 1).
test input_set_merge_rejects_equal_heads_across_fields() {
  let tx = transaction([h_a, h_c], [h_b, h_c])
  let tied = Scan { ..start(-1), spend_count: 2, cached_other_head: key(h_c) }
  and {
    step(
      tx,
      tied,
      duplicate(tx, tied),
      inline_field_carriage([key(h_b), key(h_c)]),
    ),
    !step(
      tx,
      tied,
      advance(
        tx,
        tied,
        Scan {
          ..tied,
          reference_count: 2,
          spend_seen: 1,
          last_key: key(h_c),
          last_field: 0,
          last_item: 1,
          cached_other_head: key(h_c),
          schedule: node(0, key(h_c), empty()),
        },
      ),
      inline_field_carriage([key(h_b), key(h_c)]),
    ),
  }
}
