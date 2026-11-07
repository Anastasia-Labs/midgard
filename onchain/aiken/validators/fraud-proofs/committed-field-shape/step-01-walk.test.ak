//// Step 01 as a walk (`docs/spec/midgard-tx.md` §12.8): one batch of §5.1's
//// item heads per transaction, at this script until the walk ends and at
//// step 02 with the slot's shape verdict once it has one.
////
//// Every selector runs the compiled validator against a full walk transaction
//// built by `walk_tx`, and each negative differs from a passing twin in the one
//// member it names. Slot 0 (stride 40) is where a grammatical field convicts,
//// so most selectors walk there; slot 2 (walked) is the honest twin.

use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use fraud_proofs/committed_field_shape/step_01 as step_01_validator
use midgard/fraud_proofs/canonical_decodability/rule.{envelope_batch_items} as canonical_decodability_rule
use midgard/fraud_proofs/canonical_decodability/walk_fixture.{
  array_head, claim_for, committed_block, one_byte_items, one_byte_run,
  stranger_key_hash, three_byte_item, two_byte_item, walk_tx,
  worst_batch_preimage,
} as canonical_decodability_walk_fixture
use midgard/fraud_proofs/committed_field_shape/rule.{
  shape_verdict_admissible, shape_verdict_not_an_envelope,
  shape_verdict_wrong_stride,
}
use midgard/fraud_proofs/committed_field_shape/step_01.{Datum, WalkState}
use midgard/fraud_proofs/committed_field_shape/walk_fixture.{
  address_witnesses_field_index, continue_redeemer, fresh_step_datum,
  outputs_field_index, resumed_step_datum, spend_inputs_field_index,
  verdict_state_data, walk_state, walk_state_data,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_field_access_v1.{max_transaction_aggregate_field_bytes}

const b: Int = envelope_batch_items

/// Step 02's hash, as the instance under test is parameterised with it.
const step_02_hash: ScriptHash = fixture.next_step_script_hash

/// Step 01's own hash: the address of the thread input `walk_tx` spends.
const own_hash: ScriptHash = fixture.step_script_hash

/// The largest item count a field reaches: §5.4's 32,768-byte bound filled
/// with one-byte items under a three-byte array head.
const max_item_count: Int = max_transaction_aggregate_field_bytes - 3

fn run_walk_step_on(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  preimage: ByteArray,
  datum: Datum,
  successor_hash: ScriptHash,
  output_state: Data,
  signer: VerificationKeyHash,
) -> Bool {
  step_01_validator.main.spend(
    step_02_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    fixture.test_certificate_policy_id,
    Some(datum),
    continue_redeemer(block, claim_for(block, field_index, preimage)),
    fixture.own_out_ref(),
    walk_tx(block, field_index, preimage, successor_hash, output_state, signer),
  )
}

/// A thread's first step over `preimage` at slot `field_index`, producing
/// `output_state` at `successor_hash`, signed by the prover.
fn first_step_at(
  field_index: Int,
  preimage: ByteArray,
  successor_hash: ScriptHash,
  output_state: fn(fixture.NativeBlockFixtureV1, Int) -> Data,
) -> Bool {
  let block = committed_block(field_index, preimage)
  run_walk_step_on(
    block,
    field_index,
    preimage,
    fresh_step_datum(),
    successor_hash,
    output_state(block, field_index),
    fixture.fraud_prover_key_hash,
  )
}

fn first_step(
  preimage: ByteArray,
  successor_hash: ScriptHash,
  output_state: fn(fixture.NativeBlockFixtureV1, Int) -> Data,
) -> Bool {
  first_step_at(
    spend_inputs_field_index,
    preimage,
    successor_hash,
    output_state,
  )
}

/// A later step at slot `field_index` resuming `state` (built against the
/// block) over `preimage`.
fn resumed_step_at(
  field_index: Int,
  preimage: ByteArray,
  state: fn(fixture.NativeBlockFixtureV1, Int) -> WalkState,
  successor_hash: ScriptHash,
  output_state: fn(fixture.NativeBlockFixtureV1, Int) -> Data,
  signer: VerificationKeyHash,
) -> Bool {
  let block = committed_block(field_index, preimage)
  run_walk_step_on(
    block,
    field_index,
    preimage,
    resumed_step_datum(state(block, field_index)),
    successor_hash,
    output_state(block, field_index),
    signer,
  )
}

fn resumed_step(
  preimage: ByteArray,
  state: fn(fixture.NativeBlockFixtureV1, Int) -> WalkState,
  successor_hash: ScriptHash,
  output_state: fn(fixture.NativeBlockFixtureV1, Int) -> Data,
) -> Bool {
  resumed_step_at(
    spend_inputs_field_index,
    preimage,
    state,
    successor_hash,
    output_state,
    fixture.fraud_prover_key_hash,
  )
}

fn walking(
  remaining: Int,
  offset: Int,
) -> fn(fixture.NativeBlockFixtureV1, Int) -> WalkState {
  fn(block, field_index) { walk_state(block, field_index, remaining, offset) }
}

fn walking_data(
  remaining: Int,
  offset: Int,
) -> fn(fixture.NativeBlockFixtureV1, Int) -> Data {
  fn(block, field_index) {
    walk_state_data(walk_state(block, field_index, remaining, offset))
  }
}

fn verdict(code: Int) -> fn(fixture.NativeBlockFixtureV1, Int) -> Data {
  fn(block, field_index) { verdict_state_data(block, field_index, code) }
}

/// `count` one-byte items, the one at `bad_index` replaced by `bad_item`.
fn one_byte_items_with(
  count: Int,
  bad_index: Int,
  bad_item: ByteArray,
) -> ByteArray {
  bytearray.concat(
    array_head(count),
    bytearray.concat(
      one_byte_run(bad_index),
      bytearray.concat(bad_item, one_byte_run(count - bad_index - 1)),
    ),
  )
}

// ---------------------------------------------------------------------------
// Batch boundaries
// ---------------------------------------------------------------------------

test committed_field_shape_step_01_walk_one_item_under_a_batch_convicts() {
  first_step(
    one_byte_items(b - 1),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

test committed_field_shape_step_01_walk_exactly_a_batch_convicts() {
  first_step(
    one_byte_items(b),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// One item over a batch: the first step continues at this script with one
/// item owed from the byte after the first `B`.
test committed_field_shape_step_01_walk_one_item_over_a_batch_continues() {
  first_step(one_byte_items(b + 1), own_hash, walking_data(1, 3 + b))
}

/// ... and the resumed step carries the slot's shape verdict to step 02.
test committed_field_shape_step_01_walk_one_item_over_a_batch_resumes_to_a_conviction() {
  resumed_step(
    one_byte_items(b + 1),
    walking(1, 3 + b),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// The same bytes at a walked slot resume to the admissible verdict.
test committed_field_shape_step_01_walk_resumes_to_admissible_at_a_walked_slot() {
  resumed_step_at(
    outputs_field_index,
    one_byte_items(b + 1),
    walking(1, 3 + b),
    step_02_hash,
    verdict(shape_verdict_admissible),
    fixture.fraud_prover_key_hash,
  )
}

/// A bad item first in the first batch ends the walk at once.
test committed_field_shape_step_01_walk_bad_item_first_in_the_first_batch() {
  first_step(
    one_byte_items_with(b + 1, 0, #"00"),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// A bad item last in the first batch ends the walk at once.
test committed_field_shape_step_01_walk_bad_item_last_in_the_first_batch() {
  first_step(
    one_byte_items_with(b + 1, b - 1, #"00"),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// A resumed batch whose first item is bad ends the walk from that batch.
test committed_field_shape_step_01_walk_bad_item_first_in_a_resumed_batch() {
  resumed_step(
    one_byte_items_with(b + 2, b, #"00"),
    walking(2, 3 + b),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// A resumed batch whose last item is bad ends the walk from that batch.
test committed_field_shape_step_01_walk_bad_item_last_in_a_resumed_batch() {
  resumed_step(
    one_byte_items_with(2 * b, 2 * b - 1, #"00"),
    walking(b, 3 + b),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// A first batch that reads its full share and lands on the last byte with an
/// item still owed carries its verdict to step 02 at once.
test committed_field_shape_step_01_walk_items_owed_at_the_last_byte_end_at_once() {
  first_step(
    bytearray.concat(array_head(b + 1), one_byte_run(b)),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// ... and may not keep its thread here instead.
test committed_field_shape_step_01_walk_rejects_a_cursor_past_the_last_byte() fail {
  first_step(
    bytearray.concat(array_head(b + 1), one_byte_run(b)),
    own_hash,
    walking_data(1, 3 + b),
  )
}

/// Eight item heads read at once that land on the last byte with items still
/// owed: no envelope, at the slot where a grammatical reading would convict.
test committed_field_shape_step_01_walk_nine_declared_eight_carried() {
  first_step(
    #"894040404040404040",
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// Seventeen declared, sixteen carried.
test committed_field_shape_step_01_walk_seventeen_declared_sixteen_carried() {
  first_step(
    bytearray.concat(array_head(17), one_byte_run(16)),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// 255 declared under a two-byte head, eight carried.
test committed_field_shape_step_01_walk_many_declared_eight_carried() {
  first_step(
    bytearray.concat(#"98ff", one_byte_run(8)),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// Trailing bytes after the last declared item, seen by the resumed batch.
test committed_field_shape_step_01_walk_trailing_bytes_after_a_resume() {
  resumed_step(
    bytearray.concat(one_byte_items(b + 1), #"40"),
    walking(1, 3 + b),
    step_02_hash,
    verdict(shape_verdict_not_an_envelope),
  )
}

/// A fixed-stride witness-set field walks too: the state names slot 7, and the
/// resumed step re-derives the witness set before it reads a byte.
test committed_field_shape_step_01_walk_resumes_a_witness_set_field() {
  resumed_step_at(
    address_witnesses_field_index,
    one_byte_items(b + 1),
    walking(1, 3 + b),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
    fixture.fraud_prover_key_hash,
  )
}

// ---------------------------------------------------------------------------
// The largest field, under tier-3 carriage
// ---------------------------------------------------------------------------

/// The thread's first step over the largest item count a field reaches.
test committed_field_shape_step_01_walk_maximum_item_count_first_step() {
  first_step(
    one_byte_items(max_item_count),
    own_hash,
    walking_data(max_item_count - b, 3 + b),
  )
}

/// A middle step of the same walk.
test committed_field_shape_step_01_walk_maximum_item_count_middle_step() {
  let walked = 16 * b
  resumed_step(
    one_byte_items(max_item_count),
    walking(max_item_count - walked, 3 + walked),
    own_hash,
    walking_data(max_item_count - walked - b, 3 + walked + b),
  )
}

/// Its last step, which carries the conviction to step 02.
test committed_field_shape_step_01_walk_maximum_item_count_last_step() {
  let walked = max_item_count / b * b
  resumed_step(
    one_byte_items(max_item_count),
    walking(max_item_count - walked, 3 + walked),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// The same field at a walked slot ends admissible.
test committed_field_shape_step_01_walk_maximum_item_count_last_step_at_a_walked_slot() {
  let walked = max_item_count / b * b
  resumed_step_at(
    outputs_field_index,
    one_byte_items(max_item_count),
    walking(max_item_count - walked, 3 + walked),
    step_02_hash,
    verdict(shape_verdict_admissible),
    fixture.fraud_prover_key_hash,
  )
}

// ---------------------------------------------------------------------------
// The worst batch — the budget rows
// ---------------------------------------------------------------------------

/// A resumed batch at the worst shape a 32,768-byte field can present: `B`
/// items after one one-byte item, every one at the two-byte head or wider.
fn worst_resumed_preimage() -> ByteArray {
  let three_byte_count = ( max_transaction_aggregate_field_bytes - 4 - 26 * b ) / 233
  bytearray.concat(
    array_head(b + 1),
    bytearray.concat(
      #"40",
      bytearray.concat(
        fixture.repeated_bytes_v1(three_byte_item(), three_byte_count),
        fixture.repeated_bytes_v1(two_byte_item(), b - three_byte_count),
      ),
    ),
  )
}

/// **Budget row.** The thread's first step at the worst batch, under tier-3
/// carriage, at the fixed-stride slot whose stride arithmetic the ending batch
/// runs.
test committed_field_shape_step_01_walk_first_step_at_the_worst_batch() {
  first_step(
    worst_batch_preimage(),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// **Budget control** for the row above: the same block, claim and
/// transaction built, with the validator not run.
test committed_field_shape_step_01_walk_fixture_only_first_step_at_the_worst_batch() {
  let preimage = worst_batch_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  fixture_only(
    block,
    preimage,
    fresh_step_datum(),
    step_02_hash,
    verdict(shape_verdict_wrong_stride)(block, spend_inputs_field_index),
  )
}

/// **Budget row.** A resumed step at the worst batch, under tier-3 carriage.
test committed_field_shape_step_01_walk_resumed_step_at_the_worst_batch() {
  resumed_step(
    worst_resumed_preimage(),
    walking(b, 4),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// **Budget control** for the row above.
test committed_field_shape_step_01_walk_fixture_only_resumed_step_at_the_worst_batch() {
  let preimage = worst_resumed_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  fixture_only(
    block,
    preimage,
    resumed_step_datum(walking(b, 4)(block, spend_inputs_field_index)),
    step_02_hash,
    verdict(shape_verdict_wrong_stride)(block, spend_inputs_field_index),
  )
}

fn fixture_only(
  block: fixture.NativeBlockFixtureV1,
  preimage: ByteArray,
  datum: Datum,
  successor_hash: ScriptHash,
  output_state: Data,
) -> Bool {
  let redeemer = continue_redeemer(
    block,
    claim_for(block, spend_inputs_field_index, preimage),
  )
  let tx = walk_tx(
    block,
    spend_inputs_field_index,
    preimage,
    successor_hash,
    output_state,
    fixture.fraud_prover_key_hash,
  )
  let redeemer_data: Data = redeemer
  let datum_data: Data = datum
  and {
    bytearray.length(preimage) > 32_000,
    redeemer_data != datum_data,
    tx.extra_signatories == [fixture.fraud_prover_key_hash],
  }
}

/// Both budget rows read the shape they are named for.
test committed_field_shape_step_01_walk_worst_batch_shapes() {
  let first = worst_batch_preimage()
  let resumed = worst_resumed_preimage()
  and {
    bytearray.length(first) <= max_transaction_aggregate_field_bytes,
    bytearray.length(resumed) <= max_transaction_aggregate_field_bytes,
    max_transaction_aggregate_field_bytes - bytearray.length(first) < 233,
    max_transaction_aggregate_field_bytes - bytearray.length(resumed) < 233,
  }
}

// ---------------------------------------------------------------------------
// Forged successors — on the thread's first step
// ---------------------------------------------------------------------------

/// A walk with items owed cannot jump to step 02, with any verdict.
test committed_field_shape_step_01_walk_rejects_an_early_step_02() fail {
  first_step(
    one_byte_items(b + 1),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// ... nor carry its cursor to step 02's address.
test committed_field_shape_step_01_walk_rejects_a_cursor_at_step_02() fail {
  first_step(one_byte_items(b + 1), step_02_hash, walking_data(1, 3 + b))
}

/// ... nor to a third script.
test committed_field_shape_step_01_walk_rejects_a_cursor_at_a_foreign_script() fail {
  first_step(
    one_byte_items(b + 1),
    fixture.hub_oracle_script_hash,
    walking_data(1, 3 + b),
  )
}

/// A finished walk cannot keep its thread at this script.
test committed_field_shape_step_01_walk_rejects_a_verdict_kept_here() fail {
  first_step(one_byte_items(b), own_hash, verdict(shape_verdict_wrong_stride))
}

/// ... nor carry its verdict to a third script.
test committed_field_shape_step_01_walk_rejects_a_verdict_at_a_foreign_script() fail {
  first_step(
    one_byte_items(b),
    fixture.hub_oracle_script_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// The verdict step 02 receives is the slot's shape verdict, not §12.7's
/// verdict over the same bytes (grammatical, code 0, which this family reads
/// as admissible).
test committed_field_shape_step_01_walk_rejects_the_envelope_verdict_in_place_of_the_shape_verdict() fail {
  first_step(one_byte_items(b), step_02_hash, verdict(shape_verdict_admissible))
}

/// The owed count is derived, not chosen.
test committed_field_shape_step_01_walk_rejects_a_forged_remaining_count() fail {
  first_step(one_byte_items(b + 2), own_hash, walking_data(1, 3 + b))
}

/// The resume offset is derived, not chosen.
test committed_field_shape_step_01_walk_rejects_a_forged_resume_offset() fail {
  first_step(one_byte_items(b + 1), own_hash, walking_data(1, 4 + b))
}

/// The cursor names the slot the bytes were opened at.
test committed_field_shape_step_01_walk_rejects_a_forged_cursor_field() fail {
  first_step(
    one_byte_items(b + 1),
    own_hash,
    fn(block, _field_index) { walk_state_data(walk_state(block, 1, 1, 3 + b)) },
  )
}

/// The cursor names the transaction the bytes were opened from.
test committed_field_shape_step_01_walk_rejects_a_forged_cursor_transaction() fail {
  first_step(
    one_byte_items(b + 1),
    own_hash,
    fn(_block, field_index) {
      walk_state_data(
        WalkState {
          bad_tx_id: fixture.absent_native_tx_id,
          field_index,
          remaining: 1,
          offset: 3 + b,
        },
      )
    },
  )
}

// ---------------------------------------------------------------------------
// Forged successors — on a resumed step
// ---------------------------------------------------------------------------

/// A resumed walk with items still owed cannot jump to step 02.
test committed_field_shape_step_01_walk_rejects_an_early_step_02_after_a_resume() fail {
  resumed_step(
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    step_02_hash,
    verdict(shape_verdict_wrong_stride),
  )
}

/// ... nor carry its cursor to step 02's address.
test committed_field_shape_step_01_walk_rejects_a_resumed_cursor_at_step_02() fail {
  resumed_step(
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    step_02_hash,
    walking_data(1, 3 + 2 * b),
  )
}

/// ... nor to a third script.
test committed_field_shape_step_01_walk_rejects_a_resumed_cursor_at_a_foreign_script() fail {
  resumed_step(
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    fixture.hub_oracle_script_hash,
    walking_data(1, 3 + 2 * b),
  )
}

/// A walk finished by a resumed batch cannot keep its thread at this script.
test committed_field_shape_step_01_walk_rejects_a_finished_walk_kept_here() fail {
  resumed_step(
    one_byte_items(b + 1),
    walking(1, 3 + b),
    own_hash,
    walking_data(0, 4 + b),
  )
}

/// A resumed step cannot forward a verdict its batch did not earn.
test committed_field_shape_step_01_walk_rejects_a_fabricated_resumed_verdict() fail {
  resumed_step(
    one_byte_items(b + 1),
    walking(1, 3 + b),
    step_02_hash,
    verdict(shape_verdict_admissible),
  )
}

/// The cursor a resumed step writes is derived too.
test committed_field_shape_step_01_walk_rejects_a_forged_resumed_cursor() fail {
  resumed_step(
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    own_hash,
    walking_data(1, 4 + 2 * b),
  )
}

/// A cursor is resumed only over the slot it was written for.
test committed_field_shape_step_01_walk_rejects_a_resume_at_another_field() fail {
  let preimage = one_byte_items(b + 1)
  let block = committed_block(spend_inputs_field_index, preimage)
  run_walk_step_on(
    block,
    spend_inputs_field_index,
    preimage,
    resumed_step_datum(walk_state(block, 1, 1, 3 + b)),
    step_02_hash,
    verdict_state_data(
      block,
      spend_inputs_field_index,
      shape_verdict_wrong_stride,
    ),
    fixture.fraud_prover_key_hash,
  )
}

/// A cursor is resumed only over the transaction it was written for.
test committed_field_shape_step_01_walk_rejects_a_resume_of_another_transaction() fail {
  let preimage = one_byte_items(b + 1)
  let block = committed_block(spend_inputs_field_index, preimage)
  run_walk_step_on(
    block,
    spend_inputs_field_index,
    preimage,
    resumed_step_datum(
      WalkState {
        bad_tx_id: fixture.absent_native_tx_id,
        field_index: spend_inputs_field_index,
        remaining: 1,
        offset: 3 + b,
      },
    ),
    step_02_hash,
    verdict_state_data(
      block,
      spend_inputs_field_index,
      shape_verdict_wrong_stride,
    ),
    fixture.fraud_prover_key_hash,
  )
}

// ---------------------------------------------------------------------------
// Who may take a step
// ---------------------------------------------------------------------------

/// A loop step is the prover's: the same resumed step signed by any other key
/// is refused.
test committed_field_shape_step_01_walk_rejects_a_stranger_on_a_loop_step() fail {
  resumed_step_at(
    spend_inputs_field_index,
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    own_hash,
    walking_data(1, 3 + 2 * b),
    stranger_key_hash,
  )
}

/// Its signed twin passes, so the refusal above is the signature's.
test committed_field_shape_step_01_walk_prover_takes_a_loop_step() {
  resumed_step_at(
    spend_inputs_field_index,
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    own_hash,
    walking_data(1, 3 + 2 * b),
    fixture.fraud_prover_key_hash,
  )
}
