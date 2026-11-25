//// Execution cost against field cardinality for the steps that read an
//// item of a variable-length field: `no-input` step-02 and `double-spend`
//// step-04 at one spend input and at the admissible 296
//// (`maximum_cardano_spend_redeemer_count`), and `missing-signature` step-04
//// at one address witness and at the widest field 7 the format admits.
////
//// A step hashes the field preimage once and reaches item `n` by one
//// multiplication and one slice at the field's fixed stride, so the spend-input
//// steps' cost should be flat in the input count. The field-7 walk visits
//// every item, because its rule is an absence claim, so what it must show is
//// fit, not flatness. Each cardinality has a control row that builds the
//// fixture and the L1 transaction but never enters the validator, and a
//// measured row that does the same work and then enters it: the difference is
//// the step, and the difference between two cardinalities' differences is
//// whatever cost still scales with cardinality.
////
//// The readings are held to ceilings in
//// `demo/midgard-core/src/script-budgets.json`, judged by
//// `onchain/aiken/scripts/script-budgets.mjs` at the execution basis
//// (`SCRIPT_BUDGET_BASIS`). The neutralisation tests at the end show that the
//// fixtures are at the cardinality claimed and that the step still refuses a
//// preimage the transaction never committed.
////
//// The module rebuilds its own fixtures rather than share the family suites'
//// helpers, so a change to those helpers cannot move a reading. The file name
//// is narrower than its contents; renaming it would rename every
//// `script-budgets.json` entry for it.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/transaction.{Transaction}
use fraud_proofs/double_spend/step_04 as double_spend_step_04_validator
use fraud_proofs/missing_signature/step_04 as missing_signature_step_04_validator
use fraud_proofs/no_input/step_02 as no_input_step_02_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/double_spend/step_04 as double_spend_step_04
use midgard/fraud_proofs/field_opening_v1.{
  BodyFieldOpening, FieldOpeningV1, WitnessAnchor, WitnessFieldOpening,
  address_witnesses_field_index, opened_field_walk,
}
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage_fixture
use midgard/fraud_proofs/input_existence_v1/types as input_existence_types
use midgard/fraud_proofs/missing_signature/step_04 as missing_signature_step_04
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput,
}
use midgard/fraud_proofs/no_input/step_02 as no_input_step_02
use midgard/fraud_proofs/no_input/step_03 as no_input_step_03
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/native_tx_field_access_v1.{
  Inline, encode_field_preimage, maximum_cardano_spend_redeemer_count,
  spend_input_stride,
}
use midgard/native_tx_machine_walk_v1.{field_walk_checkpoint_hash, walk_fold}

/// The cardinality #551 measured the defect at, and the one this module
/// re-measures at. Named from the door's own constant so the two cannot drift.
const admissible_spend_input_count: Int = maximum_cardano_spend_redeemer_count

/// The other end of the range: the smallest collection an index can be read
/// out of at all.
const minimal_spend_input_count: Int = 1

/// `count` distinct spend inputs. Distinct output indices rather than distinct
/// transaction ids, because §5.3's fixed 3-byte output index is exactly what
/// makes the item width constant — varying the index is what a real
/// high-cardinality transaction does and it keeps every item the same
/// `spend_input_stride` (40) bytes.
fn spend_inputs_of(count: Int) -> List<MidgardTxInput> {
  accumulate_spend_inputs(count, [])
}

fn accumulate_spend_inputs(
  remaining: Int,
  accumulated: List<MidgardTxInput>,
) -> List<MidgardTxInput> {
  if remaining <= 0 {
    accumulated
  } else {
    accumulate_spend_inputs(
      remaining - 1,
      [
        MidgardTxInput {
          tx_id: #"3131313131313131313131313131313131313131313131313131313131313131",
          output_index: remaining - 1,
        },
        ..accumulated
      ],
    )
  }
}

fn cardinality_block(count: Int) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody {
        ..base.body,
        inputs: spend_inputs_of(count),
      },
    },
  )
}

/// Tier-1 carriage of the block's own field-0 preimage.
fn spend_inputs_opening(block: fixture.NativeBlockFixtureV1) -> FieldOpeningV1 {
  BodyFieldOpening {
    native_tx_compact_cbor: block.compact_cbor,
    carriage: Inline {
      preimage: encode_field_preimage(
        block.tx.body.inputs |> list.map(encode_midgard_tx_input),
      ),
    },
  }
}

/// The §5.1 header width for `count` items, so the control row can assert the
/// preimage it built is the size the grammar says — a fixture that quietly
/// stopped growing would make the whole comparison meaningless.
fn field_header_bytes(count: Int) -> Int {
  if count <= 23 {
    1
  } else if count <= 255 {
    2
  } else {
    3
  }
}

fn built_preimage_bytes(opening: FieldOpeningV1) -> Int {
  expect BodyFieldOpening { carriage: Inline { preimage }, .. } = opening
  bytearray.length(preimage)
}

/// The witness-side twin: the same assertion for a `WitnessFieldOpening`, so a
/// field-6 or field-7 control row can prove its fixture is the width §5.1 and
/// §5.3 predict rather than assume it.
fn built_witness_preimage_bytes(opening: FieldOpeningV1) -> Int {
  expect WitnessFieldOpening { carriage: Inline { preimage }, .. } = opening
  bytearray.length(preimage)
}

// ---------------------------------------------------------------------------
// `no-input` step-02
// ---------------------------------------------------------------------------

/// Everything the measured row builds, and nothing it does not: the block
/// fixture, the field-0 carriage, the expected successor state, and the L1
/// transaction. Subtracting this from the measured row leaves the validator.
fn no_input_step_02_control(count: Int) -> Bool {
  let block = cardinality_block(count)
  let claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id: block.tx_id },
    ledger_state.L2Transaction,
    block.header.prev_utxos_root,
    None,
  )
  let opening = spend_inputs_opening(block)
  expect Some(missing_input) = list.at(block.tx.body.inputs, count - 1)
  let expected_output_state: Data = input_existence_types.SelectedInputStateV1 {
    compact: carriage_fixture.compact_view_v1(block.compact),
    source_kind: 0,
    input_index: count - 1,
    claimed_input: None,
    subject: substrate.VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    header: claim.header,
    selected_input: Some(missing_input),
    step_index: claim.event_to_step_membership.value.step_index,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
    redeemers: [
      fixture.phas_withdraw_redeemer_entry(
        claim.event_to_step_membership.phas_root,
        cbor.serialise(claim.event_to_step_membership.key),
        cbor.serialise(claim.event_to_step_membership.value),
      ),
    ],
  }
  and {
    // The fixture really is the size §5.1 says it is at this cardinality.
    built_preimage_bytes(opening) == field_header_bytes(count) + count * spend_input_stride,
    list.length(l1_tx.inputs) > 0,
  }
}

fn no_input_step_02_at(count: Int) -> Bool {
  let block = cardinality_block(count)
  let claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id: block.tx_id },
    ledger_state.L2Transaction,
    block.header.prev_utxos_root,
    None,
  )
  let opening = spend_inputs_opening(block)
  // The *last* input, so a walk-shaped regression would have to traverse the
  // whole collection to answer. Under §10.5's arithmetic access the position is
  // irrelevant, which is the property being measured.
  expect Some(missing_input) = list.at(block.tx.body.inputs, count - 1)
  let expected_output_state: Data = input_existence_types.SelectedInputStateV1 {
    compact: carriage_fixture.compact_view_v1(block.compact),
    source_kind: 0,
    input_index: count - 1,
    claimed_input: None,
    subject: substrate.VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    header: claim.header,
    selected_input: Some(missing_input),
    step_index: claim.event_to_step_membership.value.step_index,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
    redeemers: [
      fixture.phas_withdraw_redeemer_entry(
        claim.event_to_step_membership.phas_root,
        cbor.serialise(claim.event_to_step_membership.key),
        cbor.serialise(claim.event_to_step_membership.value),
      ),
    ],
  }
  no_input_step_02_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          input_existence_types.SourceStateV1 {
            compact: carriage_fixture.compact_view_v1(block.compact),
            subject: substrate.VerdictSubjectV1 {
              version: 1,
              direction: 0,
              source_kind: 0,
              transaction_id: block.tx_id,
              source_key: #"",
              rejection_reason: None,
            },
            header: claim.header,
          },
        ),
      },
    ),
    ct.Continue(
      input_existence_types.SelectArgsV1 {
        input_index: 0,
        output_index: 0,
        inputs_opening: opening,
        selected_input_index: count - 1,
        claimed_inputs_opening: None,
        event_membership: carriage_fixture.direct_carried_membership_v1(
          claim.event_to_step_membership,
          0,
        ),
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

test no_input_step_02_control_at_one_spend_input() {
  no_input_step_02_control(minimal_spend_input_count)
}

test no_input_step_02_at_one_spend_input() {
  no_input_step_02_at(minimal_spend_input_count)
}

test no_input_step_02_control_at_the_admissible_maximum() {
  no_input_step_02_control(admissible_spend_input_count)
}

/// The reading #551 says could not be taken at all. It is taken here, and the
/// step selects the *last* of 296 inputs.
test no_input_step_02_at_the_admissible_maximum() {
  no_input_step_02_at(admissible_spend_input_count)
}

// ---------------------------------------------------------------------------
// `double-spend` step-04
// ---------------------------------------------------------------------------

fn double_spend_step_04_control(count: Int) -> Bool {
  let block = cardinality_block(count)
  let opening = spend_inputs_opening(block)
  expect Some(_double_spent_input) = list.at(block.tx.body.inputs, count - 1)
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = fixture.finalize_step_l1_tx_v1(asset_name)
  and {
    built_preimage_bytes(opening) == field_header_bytes(count) + count * spend_input_stride,
    list.length(l1_tx.inputs) > 0,
  }
}

fn double_spend_step_04_at(count: Int) -> Bool {
  let block = cardinality_block(count)
  let opening = spend_inputs_opening(block)
  expect Some(double_spent_input) = list.at(block.tx.body.inputs, count - 1)
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = fixture.finalize_step_l1_tx_v1(asset_name)
  double_spend_step_04_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          double_spend_step_04.State {
            verified_tx2_id: block.tx_id,
            double_spent_input,
          },
        ),
      },
    ),
    ct.Continue(
      double_spend_step_04.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        tx2_spend_inputs_opening: opening,
        double_spent_input_index: count - 1,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

test double_spend_step_04_control_at_one_spend_input() {
  double_spend_step_04_control(minimal_spend_input_count)
}

test double_spend_step_04_at_one_spend_input() {
  double_spend_step_04_at(minimal_spend_input_count)
}

test double_spend_step_04_control_at_the_admissible_maximum() {
  double_spend_step_04_control(admissible_spend_input_count)
}

/// The other reading #551 says could not be taken at all.
test double_spend_step_04_at_the_admissible_maximum() {
  double_spend_step_04_at(admissible_spend_input_count)
}

// ---------------------------------------------------------------------------
// `missing-signature` step-04, the field-7 walk
// ---------------------------------------------------------------------------
//
// The genuinely unbounded walk. The two pairs above measure *arithmetic* item access over
// a fixed-stride field; this walks field 7, which is also **fixed-stride**, so
// the walk pays no per-item envelope decode to find the next item, but it
// still visits every one because the rule is an absence claim. Its cost is
// O(N) in the address-witness count by construction, so what has to be
// measured is not flatness but *fit*.

/// The widest field 7 the format itself admits: §5.4's aggregate field cap over
/// §5.3's address-witness stride. Derived rather than pinned as a literal,
/// because the point of measuring the *maximum* is that it is the format's
/// maximum and not a number this module chose.
const admissible_address_witness_count: Int =
  ( native_tx_field_access_v1.max_transaction_aggregate_field_bytes - 3 ) / native_tx_field_access_v1.address_witness_stride

const minimal_address_witness_count: Int = 1

/// `count` distinct address witnesses. Distinct because a real
/// high-cardinality witness set is (§6.1 forbids duplicates), and because a
/// collapsible collection would not measure a walk.
fn address_witnesses_of(count: Int) -> List<MidgardAddressWitness> {
  accumulate_address_witnesses(count, [])
}

fn accumulate_address_witnesses(
  remaining: Int,
  accumulated: List<MidgardAddressWitness>,
) -> List<MidgardAddressWitness> {
  if remaining <= 0 {
    accumulated
  } else {
    accumulate_address_witnesses(
      remaining - 1,
      [
        MidgardAddressWitness {
          verification_key: distinct_verification_key(remaining - 1),
          signature: fixture.sample_witness_signature(),
        },
        ..accumulated
      ],
    )
  }
}

/// A 32-byte verification key that varies with `index` in its last two bytes,
/// so every witness in the collection is distinct and every one is the right
/// width for §5.3's 103-byte stride.
fn distinct_verification_key(index: Int) -> ByteArray {
  #"40404040404040404040404040404040404040404040404040404040404040"
    |> bytearray.concat(bytearray.push(#"", index % 256))
}

/// A key `address_witnesses_of` never produces — its leading byte differs — so
/// the absence claim the step adjudicates is true and the walk runs to
/// completion.
fn absent_verification_key() -> ByteArray {
  #"7070707070707070707070707070707070707070707070707070707070707070"
}

fn address_cardinality_block(count: Int) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        addr_tx_wits: address_witnesses_of(count),
      },
    },
  )
}

fn step_04_preimage_items(count: Int) -> List<ByteArray> {
  address_witnesses_of(count) |> list.map(encode_midgard_address_witness)
}

fn missing_signature_step_04_control(count: Int) -> Bool {
  let block = address_cardinality_block(count)
  let items = step_04_preimage_items(count)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    items,
  )
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = fixture.finalize_step_l1_tx_v1(asset_name)
  and {
    built_witness_preimage_bytes(opening) == field_header_bytes(count) + count * native_tx_field_access_v1.address_witness_stride,
    list.length(l1_tx.inputs) > 0,
  }
}

fn missing_signature_step_04_at(count: Int) -> Bool {
  let block = address_cardinality_block(count)
  let items = step_04_preimage_items(count)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    items,
  )
  let asset_name = fixture.ct_token_asset_name(block)
  let state = missing_signature_step_04.State {
    missing_required_signer_vkey: absent_verification_key(),
    verified_tx_id: block.tx_id,
    verified_witness_set_hash: block.compact.witness_set_hash,
    field_walk_checkpoint_hash: #"",
  }
  let datum = Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
  if count > missing_signature_step_04_validator.witness_scan_batch_size {
    let Pair(view, checkpoint) = opened_field_walk(
      opening,
      WitnessAnchor {
        tx_id: block.tx_id,
        witness_set_hash: block.compact.witness_set_hash,
        source_kind: 0,
      },
      address_witnesses_field_index,
      [],
      fixture.test_certificate_policy_id,
    )
    let Pair(_, next_checkpoint) = walk_fold(
      view,
      checkpoint,
      missing_signature_step_04_validator.witness_scan_batch_size,
      False,
      fn(found, _index, _item) { found },
    )
    let expected_output_state: Data = missing_signature_step_04.State {
      ..state,
      field_walk_checkpoint_hash: field_walk_checkpoint_hash(next_checkpoint),
    }
    missing_signature_step_04_validator.main.spend(
      fixture.fraud_proof_policy_id,
      fixture.fraud_proof_token_address(),
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      datum,
      ct.Continue(
        missing_signature_step_04.Scan {
          input_index: 0,
          output_index: 0,
          addr_tx_wits_opening: opening,
          checkpoint_cbor: None,
        },
      ),
      fixture.own_out_ref(),
      fixture.continue_same_step_l1_tx_v1(asset_name, expected_output_state),
    )
  } else {
    missing_signature_step_04_validator.main.spend(
      fixture.fraud_proof_policy_id,
      fixture.fraud_proof_token_address(),
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      datum,
      ct.Continue(
        missing_signature_step_04.Finalize {
          input_index: 0,
          output_index: 0,
          fraud_proof_mint_redeemer_index: 0,
          addr_tx_wits_opening: opening,
          checkpoint_cbor: None,
        },
      ),
      fixture.own_out_ref(),
      fixture.finalize_step_l1_tx_v1(asset_name),
    )
  }
}

test missing_signature_step_04_control_at_one_address_witness() {
  missing_signature_step_04_control(minimal_address_witness_count)
}

test missing_signature_step_04_at_one_address_witness() {
  missing_signature_step_04_at(minimal_address_witness_count)
}

test missing_signature_step_04_control_at_the_admissible_maximum() {
  missing_signature_step_04_control(admissible_address_witness_count)
}

/// The field-7 walk at the format's own widest admissible field 7: 318 address
/// witnesses, every one visited, the absence claim true.
test missing_signature_step_04_at_the_admissible_maximum() {
  missing_signature_step_04_at(admissible_address_witness_count)
}

// ---------------------------------------------------------------------------
// Neutralisation
// ---------------------------------------------------------------------------
//
// The ledger's rows are only worth publishing if what they measure
// discriminates. These two say so: the fixture is genuinely at the cardinality
// claimed, and the step at that cardinality still refuses a preimage the
// transaction never committed. Without them the readings above could be
// readings of a validator that accepts anything, or of a fixture that quietly
// collapsed to one input.

/// The high-cardinality fixture is genuinely 296 inputs, and its field-0
/// preimage is the 11,843 bytes §5.1 and §5.3 predict for them — inside the
/// §8.3 tier-1 redeemer bound, so the reading above is of tier-1 carriage and
/// not of some silently-promoted tier.
test fixture_sits_at_the_admissible_cardinality() {
  let block = cardinality_block(admissible_spend_input_count)
  let preimage_bytes = built_preimage_bytes(spend_inputs_opening(block))
  and {
    list.length(block.tx.body.inputs) == admissible_spend_input_count,
    admissible_spend_input_count == 296,
    preimage_bytes == 3 + 296 * spend_input_stride,
    preimage_bytes == 11843,
    preimage_bytes <= native_tx_field_access_v1.max_tier1_redeemer_preimage_bytes,
  }
}

/// At 296 inputs the door still authenticates: a preimage one input short of
/// the committed collection does not hash to field 0's commitment, so the step
/// refuses. The measured rows are therefore readings of a validator that checks
/// something.
test step_at_the_admissible_maximum_refuses_a_tampered_preimage() fail {
  let count = admissible_spend_input_count
  let block = cardinality_block(count)
  expect Some(double_spent_input) = list.at(block.tx.body.inputs, count - 1)
  let asset_name = fixture.ct_token_asset_name(block)
  double_spend_step_04_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          double_spend_step_04.State {
            verified_tx2_id: block.tx_id,
            double_spent_input,
          },
        ),
      },
    ),
    ct.Continue(
      double_spend_step_04.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        tx2_spend_inputs_opening: BodyFieldOpening {
          native_tx_compact_cbor: block.compact_cbor,
          carriage: Inline {
            preimage: encode_field_preimage(
              spend_inputs_of(count - 1) |> list.map(encode_midgard_tx_input),
            ),
          },
        },
        double_spent_input_index: count - 1,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// **The fact the ledger's delta attribution rests on.** A §2.5 compact
/// structure is nine field *commitments* and a handful of scalars, so its
/// length does not depend on how many items any field holds — the 1-input and
/// 296-input blocks encode to byte strings of the same size, with different
/// content.
///
/// Everything `verify_native_tx_compact_cbor_v1` does is a function of those
/// bytes, so its execution cost is cardinality-independent *by construction*.
/// That is what makes the ledger's `deltaAttribution` checkable rather than a
/// story: a control-subtraction that attributes a cardinality-dependent cost to
/// that function is measuring its own compilation, not the validator.
test compact_structure_length_is_cardinality_independent() {
  let low = cardinality_block(minimal_spend_input_count)
  let high = cardinality_block(admissible_spend_input_count)
  and {
    bytearray.length(low.compact_cbor) == bytearray.length(high.compact_cbor),
    low.compact_cbor != high.compact_cbor,
  }
}

/// The step-04 rows measure a validator that discriminates too: at the
/// admissible address-witness cardinality the walk still finds a key that *is*
/// present, so the absence claim fails and the step refuses. Without this the
/// two step-04 readings could be readings of a walk over an empty or
/// unauthenticated collection.
test step_04_at_the_admissible_maximum_refuses_a_present_signature() fail {
  let count = admissible_address_witness_count
  let block = address_cardinality_block(count)
  expect Some(present) = list.at(
    address_witnesses_of(count),
    missing_signature_step_04_validator.witness_scan_batch_size - 1,
  )
  let asset_name = fixture.ct_token_asset_name(block)
  missing_signature_step_04_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          missing_signature_step_04.State {
            missing_required_signer_vkey: present.verification_key,
            verified_tx_id: block.tx_id,
            verified_witness_set_hash: block.compact.witness_set_hash,
            field_walk_checkpoint_hash: #"",
          },
        ),
      },
    ),
    ct.Continue(
      missing_signature_step_04.Scan {
        input_index: 0,
        output_index: 0,
        addr_tx_wits_opening: fixture.inline_witness_opening(
          block.compact_cbor,
          block.witness_set_compact,
          step_04_preimage_items(count),
        ),
        checkpoint_cbor: None,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_same_step_l1_tx_v1(
      asset_name,
      missing_signature_step_04.State {
        missing_required_signer_vkey: present.verification_key,
        verified_tx_id: block.tx_id,
        verified_witness_set_hash: block.compact.witness_set_hash,
        field_walk_checkpoint_hash: #"",
      },
    ),
  )
}

/// The field-7 fixture is genuinely at the format's widest admissible
/// cardinality, and its preimage is the width §5.1 and §5.3 predict — inside
/// §5.4's aggregate cap by construction, and (unlike field 0's) *outside* the
/// §8.3 tier-1 redeemer bound and over `K`, so on L1 that preimage travels
/// under tier 3 — admissible for a witness-set field since #606 resolved §8.3
/// erratum E2 limit 3 (owner ruling 2026-08-16). The row is nonetheless taken
/// under tier-1 carriage in the harness, at a width tier 1 could not carry, and
/// is published as a walk-cost reading rather than as a reachable
/// configuration; this selector is what §8.3 cites by name so that fact cannot
/// go unnoticed.
test address_witness_fixture_sits_at_the_admissible_cardinality() {
  let block = address_cardinality_block(admissible_address_witness_count)
  let preimage_bytes = built_witness_preimage_bytes(
    fixture.inline_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      step_04_preimage_items(admissible_address_witness_count),
    ),
  )
  and {
    list.length(block.tx.witness_set.addr_tx_wits) == admissible_address_witness_count,
    admissible_address_witness_count == 318,
    preimage_bytes == 3 + 318 * native_tx_field_access_v1.address_witness_stride,
    preimage_bytes == 32757,
    preimage_bytes <= native_tx_field_access_v1.max_transaction_aggregate_field_bytes,
    preimage_bytes > native_tx_field_access_v1.max_tier1_redeemer_preimage_bytes,
  }
}
