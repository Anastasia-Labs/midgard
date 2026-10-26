//// Step 01 as a walk (`docs/spec/midgard-tx.md` §12.7): one batch of §5.1's
//// item heads per transaction, at this script until the walk ends and at
//// step 02 once it has a verdict.
////
//// Every selector runs the compiled validator against a full walk transaction
//// built by `walk_fixture.walk_tx`, and each negative differs from a passing
//// twin in the one member it names.

use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use fraud_proofs/canonical_decodability/step_01 as step_01_validator
use midgard/fraud_proofs/canonical_decodability/rule.{
  envelope_batch_items, verdict_grammatical, verdict_missing_item_header,
  verdict_not_an_item_header, verdict_trailing_bytes,
}
use midgard/fraud_proofs/canonical_decodability/step_01.{Datum, WalkState}
use midgard/fraud_proofs/canonical_decodability/walk_fixture.{
  array_head, claim_for, committed_block, continue_redeemer, fresh_step_datum,
  one_byte_items, one_byte_run, outputs_field_index, resumed_step_datum,
  script_witnesses_field_index, stranger_key_hash, three_byte_item,
  two_byte_item, verdict_state_data, walk_state, walk_state_data, walk_tx,
  worst_batch_preimage,
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

fn run_walk_step(
  field_index: Int,
  preimage: ByteArray,
  datum: Datum,
  successor_hash: ScriptHash,
  output_state: Data,
  signer: VerificationKeyHash,
) -> Bool {
  let block = committed_block(field_index, preimage)
  run_walk_step_on(
    block,
    field_index,
    preimage,
    datum,
    successor_hash,
    output_state,
    signer,
  )
}

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

/// A thread's first step over `preimage` at slot 2, producing `output_state` at
/// `successor_hash`, signed by the prover.
fn first_step(
  preimage: ByteArray,
  successor_hash: ScriptHash,
  output_state: fn(fixture.NativeBlockFixtureV1) -> Data,
) -> Bool {
  let block = committed_block(outputs_field_index, preimage)
  run_walk_step_on(
    block,
    outputs_field_index,
    preimage,
    fresh_step_datum(),
    successor_hash,
    output_state(block),
    fixture.fraud_prover_key_hash,
  )
}

/// A later step resuming `state` (built against the block) over `preimage` at
/// slot 2.
fn resumed_step(
  preimage: ByteArray,
  state: fn(fixture.NativeBlockFixtureV1) -> WalkState,
  successor_hash: ScriptHash,
  output_state: fn(fixture.NativeBlockFixtureV1) -> Data,
  signer: VerificationKeyHash,
) -> Bool {
  let block = committed_block(outputs_field_index, preimage)
  run_walk_step_on(
    block,
    outputs_field_index,
    preimage,
    resumed_step_datum(state(block)),
    successor_hash,
    output_state(block),
    signer,
  )
}

fn walking(
  remaining: Int,
  offset: Int,
) -> fn(fixture.NativeBlockFixtureV1) -> WalkState {
  fn(block) { walk_state(block, outputs_field_index, remaining, offset) }
}

fn walking_data(
  remaining: Int,
  offset: Int,
) -> fn(fixture.NativeBlockFixtureV1) -> Data {
  fn(block) {
    walk_state_data(walk_state(block, outputs_field_index, remaining, offset))
  }
}

fn verdict(code: Int) -> fn(fixture.NativeBlockFixtureV1) -> Data {
  fn(block) { verdict_state_data(block, outputs_field_index, code) }
}

// ---------------------------------------------------------------------------
// Batch boundaries
// ---------------------------------------------------------------------------

test canonical_decodability_step_01_walk_one_item_under_a_batch_finishes() {
  first_step(one_byte_items(b - 1), step_02_hash, verdict(verdict_grammatical))
}

test canonical_decodability_step_01_walk_exactly_a_batch_finishes() {
  first_step(one_byte_items(b), step_02_hash, verdict(verdict_grammatical))
}

/// One item over a batch: the first step continues at this script with one
/// item owed from the byte after the first `B`.
test canonical_decodability_step_01_walk_one_item_over_a_batch_continues() {
  first_step(one_byte_items(b + 1), own_hash, walking_data(1, 3 + b))
}

/// ... and the resumed step finishes it.
test canonical_decodability_step_01_walk_one_item_over_a_batch_resumes_to_a_verdict() {
  resumed_step(
    one_byte_items(b + 1),
    walking(1, 3 + b),
    step_02_hash,
    verdict(verdict_grammatical),
    fixture.fraud_prover_key_hash,
  )
}

/// A resumed batch whose first item is bad convicts from that batch.
test canonical_decodability_step_01_walk_bad_item_first_in_a_resumed_batch() {
  let preimage = bytearray.concat(
    array_head(b + 2),
    bytearray.concat(one_byte_run(b), #"0040"),
  )
  resumed_step(
    preimage,
    walking(2, 3 + b),
    step_02_hash,
    verdict(verdict_not_an_item_header),
    fixture.fraud_prover_key_hash,
  )
}

/// A resumed batch whose last item is bad convicts from that batch.
test canonical_decodability_step_01_walk_bad_item_last_in_a_resumed_batch() {
  let preimage = bytearray.concat(
    array_head(2 * b),
    bytearray.concat(one_byte_run(2 * b - 1), #"00"),
  )
  resumed_step(
    preimage,
    walking(b, 3 + b),
    step_02_hash,
    verdict(verdict_not_an_item_header),
    fixture.fraud_prover_key_hash,
  )
}

/// Trailing bytes after the last declared item, seen by the resumed batch.
test canonical_decodability_step_01_walk_trailing_bytes_after_a_resume() {
  resumed_step(
    bytearray.concat(one_byte_items(b + 1), #"40"),
    walking(1, 3 + b),
    step_02_hash,
    verdict(verdict_trailing_bytes),
    fixture.fraud_prover_key_hash,
  )
}

/// A witness-set field walks too: the state names slot 6, and the resumed step
/// re-derives the witness set before it reads a byte.
test canonical_decodability_step_01_walk_resumes_a_witness_set_field() {
  let preimage = bytearray.concat(array_head(b + 2), one_byte_run(b + 1))
  let block = committed_block(script_witnesses_field_index, preimage)
  run_walk_step_on(
    block,
    script_witnesses_field_index,
    preimage,
    resumed_step_datum(
      walk_state(block, script_witnesses_field_index, 2, 3 + b),
    ),
    step_02_hash,
    verdict_state_data(
      block,
      script_witnesses_field_index,
      verdict_missing_item_header,
    ),
    fixture.fraud_prover_key_hash,
  )
}

// ---------------------------------------------------------------------------
// The largest field, under tier-3 carriage
// ---------------------------------------------------------------------------

/// The thread's first step over the largest item count a field reaches.
test canonical_decodability_step_01_walk_maximum_item_count_first_step() {
  first_step(
    one_byte_items(max_item_count),
    own_hash,
    walking_data(max_item_count - b, 3 + b),
  )
}

/// A middle step of the same walk.
test canonical_decodability_step_01_walk_maximum_item_count_middle_step() {
  let walked = 16 * b
  resumed_step(
    one_byte_items(max_item_count),
    walking(max_item_count - walked, 3 + walked),
    own_hash,
    walking_data(max_item_count - walked - b, 3 + walked + b),
    fixture.fraud_prover_key_hash,
  )
}

/// Its last step, which carries the verdict to step 02.
test canonical_decodability_step_01_walk_maximum_item_count_last_step() {
  let walked = max_item_count / b * b
  resumed_step(
    one_byte_items(max_item_count),
    walking(max_item_count - walked, 3 + walked),
    step_02_hash,
    verdict(verdict_grammatical),
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
/// carriage.
test canonical_decodability_step_01_walk_first_step_at_the_worst_batch() {
  first_step(worst_batch_preimage(), step_02_hash, verdict(verdict_grammatical))
}

/// **Budget control** for the row above: the same block, claim and
/// transaction built, with the validator not run.
test canonical_decodability_step_01_walk_fixture_only_first_step_at_the_worst_batch() {
  let preimage = worst_batch_preimage()
  let block = committed_block(outputs_field_index, preimage)
  fixture_only(
    block,
    preimage,
    fresh_step_datum(),
    step_02_hash,
    verdict(verdict_grammatical)(block),
  )
}

/// **Budget row.** A resumed step at the worst batch, under tier-3 carriage.
test canonical_decodability_step_01_walk_resumed_step_at_the_worst_batch() {
  resumed_step(
    worst_resumed_preimage(),
    walking(b, 4),
    step_02_hash,
    verdict(verdict_grammatical),
    fixture.fraud_prover_key_hash,
  )
}

/// **Budget control** for the row above.
test canonical_decodability_step_01_walk_fixture_only_resumed_step_at_the_worst_batch() {
  let preimage = worst_resumed_preimage()
  let block = committed_block(outputs_field_index, preimage)
  fixture_only(
    block,
    preimage,
    resumed_step_datum(walking(b, 4)(block)),
    step_02_hash,
    verdict(verdict_grammatical)(block),
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
    claim_for(block, outputs_field_index, preimage),
  )
  let tx = walk_tx(
    block,
    outputs_field_index,
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
test canonical_decodability_step_01_walk_worst_batch_shapes() {
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
// Forged successors
// ---------------------------------------------------------------------------

/// A walk with items owed cannot jump to step 02, with any verdict.
test canonical_decodability_step_01_walk_rejects_an_early_step_02() fail {
  first_step(one_byte_items(b + 1), step_02_hash, verdict(verdict_grammatical))
}

/// ... nor carry its cursor to step 02's address.
test canonical_decodability_step_01_walk_rejects_a_cursor_at_step_02() fail {
  first_step(one_byte_items(b + 1), step_02_hash, walking_data(1, 3 + b))
}

/// ... nor to a third script.
test canonical_decodability_step_01_walk_rejects_a_cursor_at_a_foreign_script() fail {
  first_step(
    one_byte_items(b + 1),
    fixture.hub_oracle_script_hash,
    walking_data(1, 3 + b),
  )
}

/// The owed count is derived, not chosen.
test canonical_decodability_step_01_walk_rejects_a_forged_remaining_count() fail {
  first_step(one_byte_items(b + 2), own_hash, walking_data(1, 3 + b))
}

/// The resume offset is derived, not chosen.
test canonical_decodability_step_01_walk_rejects_a_forged_resume_offset() fail {
  first_step(one_byte_items(b + 1), own_hash, walking_data(1, 4 + b))
}

/// The cursor names the slot the bytes were opened at.
test canonical_decodability_step_01_walk_rejects_a_forged_cursor_field() fail {
  first_step(
    one_byte_items(b + 1),
    own_hash,
    fn(block) { walk_state_data(walk_state(block, 1, 1, 3 + b)) },
  )
}

/// The cursor names the transaction the bytes were opened from.
test canonical_decodability_step_01_walk_rejects_a_forged_cursor_transaction() fail {
  first_step(
    one_byte_items(b + 1),
    own_hash,
    fn(_block) {
      walk_state_data(
        WalkState {
          bad_tx_id: fixture.absent_native_tx_id,
          field_index: outputs_field_index,
          remaining: 1,
          offset: 3 + b,
        },
      )
    },
  )
}

/// A finished walk cannot keep its thread at this script.
test canonical_decodability_step_01_walk_rejects_a_finished_walk_kept_here() fail {
  resumed_step(
    one_byte_items(b + 1),
    walking(1, 3 + b),
    own_hash,
    walking_data(0, 4 + b),
    fixture.fraud_prover_key_hash,
  )
}

/// A resumed step cannot forward a verdict its batch did not earn.
test canonical_decodability_step_01_walk_rejects_a_fabricated_resumed_verdict() fail {
  resumed_step(
    one_byte_items(b + 1),
    walking(1, 3 + b),
    step_02_hash,
    verdict(verdict_trailing_bytes),
    fixture.fraud_prover_key_hash,
  )
}

/// A cursor is resumed only over the slot it was written for.
test canonical_decodability_step_01_walk_rejects_a_resume_at_another_field() fail {
  let preimage = one_byte_items(b + 1)
  let block = committed_block(outputs_field_index, preimage)
  run_walk_step_on(
    block,
    outputs_field_index,
    preimage,
    resumed_step_datum(walk_state(block, 3, 1, 3 + b)),
    step_02_hash,
    verdict_state_data(block, outputs_field_index, verdict_grammatical),
    fixture.fraud_prover_key_hash,
  )
}

/// A cursor is resumed only over the transaction it was written for.
test canonical_decodability_step_01_walk_rejects_a_resume_of_another_transaction() fail {
  let preimage = one_byte_items(b + 1)
  let block = committed_block(outputs_field_index, preimage)
  run_walk_step_on(
    block,
    outputs_field_index,
    preimage,
    resumed_step_datum(
      WalkState {
        bad_tx_id: fixture.absent_native_tx_id,
        field_index: outputs_field_index,
        remaining: 1,
        offset: 3 + b,
      },
    ),
    step_02_hash,
    verdict_state_data(block, outputs_field_index, verdict_grammatical),
    fixture.fraud_prover_key_hash,
  )
}

// ---------------------------------------------------------------------------
// Who may take a step
// ---------------------------------------------------------------------------

/// A loop step is the prover's: the same resumed step signed by any other key
/// is refused.
test canonical_decodability_step_01_walk_rejects_a_stranger_on_a_loop_step() fail {
  resumed_step(
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    own_hash,
    walking_data(1, 3 + 2 * b),
    stranger_key_hash,
  )
}

/// Its signed twin passes, so the refusal above is the signature's.
test canonical_decodability_step_01_walk_prover_takes_a_loop_step() {
  resumed_step(
    one_byte_items(2 * b + 1),
    walking(b + 1, 3 + b),
    own_hash,
    walking_data(1, 3 + 2 * b),
    fixture.fraud_prover_key_hash,
  )
}
