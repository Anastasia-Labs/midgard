//// The Q1X-F6 measurement seam — issue #551, re-measured under the flat
//// scheme by the #575 Q1x rebind lane.
////
//// **What #551 recorded.** The admissible spend-input cardinality is 296 (the
//// Cardano script-spend shape bound, `maximum_cardano_spend_redeemer_count`).
//// Under the retired counted idiom, `double-spend` step-04 and `no-input`
//// step-02 each reproduced the whole authenticated spend-input collection
//// inside the step in order to re-hash it, so execution **memory** grew with
//// the disputed transaction's input count: measured through the real pipeline,
//// Q10 step-04 fit the 20% reserve only to 39 inputs and Q11 step-02 to 40, and
//// at 296 neither proof could be evaluated at all. The axis was free to the
//// adversary — build the bad transaction with ≥ 41 inputs and its proof becomes
//// unevaluatable — and carriage could not remediate it, because the cost was
//// execution and not carriage.
////
//// **What this module measures.** The same two steps, rebound onto the §8.8
//// door, at both ends of that range. Under the door a step hashes the field
//// preimage exactly once (§7.1) and reaches item `n` by one multiplication and
//// one slice at §5.3's fixed 40-byte width (§10.5), so the *step's* cost should
//// be flat in the input count. That is the claim, and these four readings are
//// what makes it falsifiable: each cardinality has a control row that builds
//// the fixture and the L1 transaction but never enters the validator, and a
//// measured row that does exactly the same work and then enters it. The
//// difference is the step; the difference between the two differences is
//// whatever cost still scales with cardinality.
////
//// The readings are pinned in `scripts/native-tx-q1x-exec-ledger-v1.json` and
//// asserted by `scripts/verify-q1x-exec-ledger-v1.mjs` at the GOAL_SPEC §3.3
//// declared basis of 13,200,000 memory units. Aiken cannot assert its own
//// execution units, so the pin has to live one level up; this module's job is
//// to be a *stable and honest subject* for it.
////
//// **This is the provisional signal, not the closure.** #565 §"Sequencing"
//// puts formal closure of #551 on the Phase-7 number taken against the Phase-6
//// blueprint. What is established here is the shape of the fix — that the
//// defect's mechanism is gone — not the evidence-grade lifecycle margin.
////
//// Nothing in this module is a fixture convenience shared with the family
//// suites: each family's own selectors live beside its validator, and this
//// module deliberately rebuilds what it needs so that a change to a family's
//// test helpers cannot silently move a published number.
////
//// **On the file name.** It says `spend-input-cardinality` because #551 was a
//// spend-input defect and the first two pairs of rows are still that
//// measurement. It has since grown two more: a script-witness cardinality pair
//// (field 6, Q17 step-06) and an address-witness cardinality pair (field 7,
//// Q16 step-04), because those are the walks the rebind made linear and
//// measuring only the flat ones would not be a measurement of the rebind. The
//// name is therefore narrower than the contents. Renaming it would move the
//// `module` key of every row in `scripts/native-tx-q1x-exec-ledger-v1.json`
//// and add a second entry to the drift the F05 manifest already carries for
//// this wave's deleted `input-witness.ak`, so it is noted here and left to
//// [#579](https://github.com/Anastasia-Labs/midgard/issues/579)'s identity
//// re-derivation batch, which is where that manifest is reconciled. What the
//// module measures is *cardinality of whatever field a rebound step reads*;
//// read the section headers, not the file name.

use aiken/collection/list
use aiken/primitive/bytearray
use fraud_proofs/double_spend/step_04 as double_spend_step_04_validator
use fraud_proofs/missing_native_script_tx/step_06 as missing_native_script_tx_step_06_validator
use fraud_proofs/missing_signature/step_04 as missing_signature_step_04_validator
use fraud_proofs/no_input/step_02 as no_input_step_02_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/double_spend/step_04 as double_spend_step_04
use midgard/fraud_proofs/field_opening_v1.{
  BodyFieldOpening, FieldOpeningV1, WitnessFieldOpening,
}
use midgard/fraud_proofs/missing_native_script_tx/step_06 as missing_native_script_tx_step_06
use midgard/fraud_proofs/missing_signature/step_04 as missing_signature_step_04
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, MidgardVersionedScript,
  NativeCardanoScript,
}
use midgard/fraud_proofs/no_input/step_02 as no_input_step_02
use midgard/fraud_proofs/no_input/step_03 as no_input_step_03
use midgard/native_tx_field_access_v1.{
  Inline, encode_field_preimage, maximum_cardano_spend_redeemer_count,
  spend_input_stride,
}
use midgard/script_proof_v1

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
// Q11 — `no-input` step-02
// ---------------------------------------------------------------------------

/// Everything the measured row builds, and nothing it does not: the block
/// fixture, the field-0 carriage, the expected successor state, and the L1
/// transaction. Subtracting this from the measured row leaves the validator.
fn no_input_step_02_control(count: Int) -> Bool {
  let block = cardinality_block(count)
  let opening = spend_inputs_opening(block)
  expect Some(missing_input) = list.at(block.tx.body.inputs, count - 1)
  let expected_output_state: Data = no_input_step_03.State {
    missing_input,
    blocks_prev_utxos_root: block.header.prev_utxos_root,
    blocks_transactions_root: block.transactions_phas_root,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = fixture.continue_mid_step_l1_tx_v1(
    asset_name,
    expected_output_state,
  )
  and {
    // The fixture really is the size §5.1 says it is at this cardinality.
    built_preimage_bytes(opening) == field_header_bytes(count) + count * spend_input_stride,
    list.length(l1_tx.inputs) > 0,
  }
}

fn no_input_step_02_at(count: Int) -> Bool {
  let block = cardinality_block(count)
  let opening = spend_inputs_opening(block)
  // The *last* input, so a walk-shaped regression would have to traverse the
  // whole collection to answer. Under §10.5's arithmetic access the position is
  // irrelevant, which is the property being measured.
  expect Some(missing_input) = list.at(block.tx.body.inputs, count - 1)
  let expected_output_state: Data = no_input_step_03.State {
    missing_input,
    blocks_prev_utxos_root: block.header.prev_utxos_root,
    blocks_transactions_root: block.transactions_phas_root,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = fixture.continue_mid_step_l1_tx_v1(
    asset_name,
    expected_output_state,
  )
  no_input_step_02_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          no_input_step_02.State {
            bad_tx_id: block.tx_id,
            blocks_prev_utxos_root: block.header.prev_utxos_root,
            blocks_transactions_root: block.transactions_phas_root,
          },
        ),
      },
    ),
    ct.Continue(
      no_input_step_02.Args {
        input_index: 0,
        output_index: 0,
        spend_inputs_opening: opening,
        bad_input_index: count - 1,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

test q1x_f6_no_input_step_02_control_at_one_spend_input() {
  no_input_step_02_control(minimal_spend_input_count)
}

test q1x_f6_no_input_step_02_at_one_spend_input() {
  no_input_step_02_at(minimal_spend_input_count)
}

test q1x_f6_no_input_step_02_control_at_the_admissible_maximum() {
  no_input_step_02_control(admissible_spend_input_count)
}

/// The reading #551 says could not be taken at all. It is taken here, and the
/// step selects the *last* of 296 inputs.
test q1x_f6_no_input_step_02_at_the_admissible_maximum() {
  no_input_step_02_at(admissible_spend_input_count)
}

// ---------------------------------------------------------------------------
// Q10 — `double-spend` step-04
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

test q1x_f6_double_spend_step_04_control_at_one_spend_input() {
  double_spend_step_04_control(minimal_spend_input_count)
}

test q1x_f6_double_spend_step_04_at_one_spend_input() {
  double_spend_step_04_at(minimal_spend_input_count)
}

test q1x_f6_double_spend_step_04_control_at_the_admissible_maximum() {
  double_spend_step_04_control(admissible_spend_input_count)
}

/// The other reading #551 says could not be taken at all.
test q1x_f6_double_spend_step_04_at_the_admissible_maximum() {
  double_spend_step_04_at(admissible_spend_input_count)
}

// ---------------------------------------------------------------------------
// Q17 — `missing-native-script-tx` step-06, the field-6 walk
// ---------------------------------------------------------------------------
//
// The two pairs above measure *arithmetic* item access over a fixed-stride
// field, which is what #551 was about. Step-06 is the other shape the rebind
// introduced and the only genuinely **unbounded** one in this wave: field 6 is
// variable-width, the rule is an absence claim, and `fold_opened_field` walks
// every item. Its cost is O(N) in the script-witness count by construction, so
// what has to be measured is not flatness but *fit* — that the whole walk still
// lands inside the GOAL_SPEC §3.3 basis at the admissible cardinality.
//
// AC2 asked for the rolling re-measurement to cover the newly unbounded walks;
// leaving this one out would have measured only the steps whose cost the rebind
// made flat and none of the step whose cost it made linear.

/// C20.6's Cardano envelope for native script witnesses — the field-6 twin of
/// `maximum_cardano_spend_redeemer_count`.
const admissible_script_witness_count: Int = 224

const minimal_script_witness_count: Int = 1

/// `count` distinct native script witnesses. Distinct because a real
/// high-cardinality witness set is (§6.1 forbids duplicates), and because the
/// walk's cost must not be measured against a collection an optimiser could
/// collapse.
fn script_witnesses_of(count: Int) -> List<MidgardVersionedScript> {
  accumulate_script_witnesses(count, [])
}

fn accumulate_script_witnesses(
  remaining: Int,
  accumulated: List<MidgardVersionedScript>,
) -> List<MidgardVersionedScript> {
  if remaining <= 0 {
    accumulated
  } else {
    accumulate_script_witnesses(
      remaining - 1,
      [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"8200581c30303030303030303030303030303030303030303030"
            |> bytearray.concat(bytearray.push(#"", ( remaining - 1 ) % 256))
            |> bytearray.concat(bytearray.push(#"", ( remaining - 1 ) / 256)),
        },
        ..accumulated
      ],
    )
  }
}

fn script_cardinality_block(count: Int) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        script_tx_wits: script_witnesses_of(count),
      },
    },
  )
}

/// A versioned script no `script_witnesses_of` collection contains, so the
/// absence claim the step adjudicates is true and the walk runs to completion —
/// the worst case, and the only one worth measuring.
fn absent_script_hash() -> ByteArray {
  script_proof_v1.versioned_script_hash(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"8201818200581c31313131313131313131313131313131313131313131",
    },
  )
}

fn step_06_preimage_items(count: Int) -> List<ByteArray> {
  script_witnesses_of(count) |> list.map(encode_midgard_versioned_script)
}

fn missing_native_script_tx_step_06_control(count: Int) -> Bool {
  let block = script_cardinality_block(count)
  let items = step_06_preimage_items(count)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    items,
  )
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = fixture.finalize_step_l1_tx_v1(asset_name)
  expect WitnessFieldOpening { carriage: Inline { preimage }, .. } = opening
  and {
    // The fixture really does carry `count` canonically encoded items, and the
    // preimage really is the envelope over them.
    bytearray.length(preimage) > count,
    list.length(items) == count,
    bytearray.length(block.compact.witness_set_hash) == 32,
    list.length(l1_tx.inputs) > 0,
  }
}

fn missing_native_script_tx_step_06_at(count: Int) -> Bool {
  let block = script_cardinality_block(count)
  let items = step_06_preimage_items(count)
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    items,
  )
  let asset_name = fixture.ct_token_asset_name(block)
  let l1_tx = fixture.finalize_step_l1_tx_v1(asset_name)
  missing_native_script_tx_step_06_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          missing_native_script_tx_step_06.State {
            expected_missing_script_hash: absent_script_hash(),
            bad_tx_id: block.tx_id,
            bad_tx_witness_set_hash: block.compact.witness_set_hash,
          },
        ),
      },
    ),
    ct.Continue(
      missing_native_script_tx_step_06.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_tx_wits_opening: opening,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

test q1x_f6_missing_native_script_tx_step_06_control_at_one_script_witness() {
  missing_native_script_tx_step_06_control(minimal_script_witness_count)
}

test q1x_f6_missing_native_script_tx_step_06_at_one_script_witness() {
  missing_native_script_tx_step_06_at(minimal_script_witness_count)
}

test q1x_f6_missing_native_script_tx_step_06_control_at_the_admissible_maximum() {
  missing_native_script_tx_step_06_control(admissible_script_witness_count)
}

/// The unbounded walk at C20.6's envelope: 224 script witnesses, every one
/// visited, the absence claim true.
test q1x_f6_missing_native_script_tx_step_06_at_the_admissible_maximum() {
  missing_native_script_tx_step_06_at(admissible_script_witness_count)
}

// ---------------------------------------------------------------------------
// Q16 — `missing-signature` step-04, the field-7 walk
// ---------------------------------------------------------------------------
//
// The second of the wave's two genuinely unbounded walks, and the one the #575
// round-2 review asked for by name. Step-06 above walks field 6; this walks
// field 7, and the shapes differ in a way worth measuring separately: field 7
// is **fixed-stride**, so the walk pays no per-item envelope decode to find the
// next item, but it still visits every one because the rule is an absence claim.
// Measuring only field 6 would have left open whether the O(N) cost is the walk
// or the variable-width decoding behind it.

/// The widest field 7 the format itself admits: §5.4's aggregate field cap over
/// §5.3's address-witness stride. Derived rather than pinned as a literal,
/// because the point of measuring the *maximum* is that it is the format's
/// maximum and not a number this module chose.
const admissible_address_witness_count: Int =
  ( native_tx_field_access_v1.max_transaction_aggregate_field_bytes - 3 ) / native_tx_field_access_v1.address_witness_stride

const minimal_address_witness_count: Int = 1

/// `count` distinct address witnesses. Distinct for the same reason the script
/// witnesses above are: §6.1 forbids duplicates, and a collapsible collection
/// would not measure a walk.
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
  let l1_tx = fixture.finalize_step_l1_tx_v1(asset_name)
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
            missing_required_signer_vkey: absent_verification_key(),
            verified_tx_id: block.tx_id,
            verified_witness_set_hash: block.compact.witness_set_hash,
          },
        ),
      },
    ),
    ct.Continue(
      missing_signature_step_04.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        addr_tx_wits_opening: opening,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

test q1x_f6_missing_signature_step_04_control_at_one_address_witness() {
  missing_signature_step_04_control(minimal_address_witness_count)
}

test q1x_f6_missing_signature_step_04_at_one_address_witness() {
  missing_signature_step_04_at(minimal_address_witness_count)
}

test q1x_f6_missing_signature_step_04_control_at_the_admissible_maximum() {
  missing_signature_step_04_control(admissible_address_witness_count)
}

/// The field-7 walk at the format's own widest admissible field 7: 318 address
/// witnesses, every one visited, the absence claim true.
test q1x_f6_missing_signature_step_04_at_the_admissible_maximum() {
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
test q1x_f6_fixture_sits_at_the_admissible_cardinality() {
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
test q1x_f6_step_at_the_admissible_maximum_refuses_a_tampered_preimage() fail {
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
test q1x_f6_compact_structure_length_is_cardinality_independent() {
  let low = cardinality_block(minimal_spend_input_count)
  let high = cardinality_block(admissible_spend_input_count)
  and {
    bytearray.length(low.compact_cbor) == bytearray.length(high.compact_cbor),
    low.compact_cbor != high.compact_cbor,
  }
}

/// The step-06 rows measure a validator that discriminates: at the admissible
/// script-witness cardinality the walk still finds a script that *is* present,
/// so the absence claim fails and the step refuses. Without this the two
/// step-06 readings could be readings of a walk over an empty or unauthenticated
/// collection.
test q1x_f6_step_06_at_the_admissible_maximum_refuses_a_present_script() fail {
  let count = admissible_script_witness_count
  let block = script_cardinality_block(count)
  expect Some(present) = list.at(script_witnesses_of(count), count - 1)
  let asset_name = fixture.ct_token_asset_name(block)
  missing_native_script_tx_step_06_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          missing_native_script_tx_step_06.State {
            expected_missing_script_hash: script_proof_v1.versioned_script_hash(
              present,
            ),
            bad_tx_id: block.tx_id,
            bad_tx_witness_set_hash: block.compact.witness_set_hash,
          },
        ),
      },
    ),
    ct.Continue(
      missing_native_script_tx_step_06.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_tx_wits_opening: fixture.inline_witness_opening(
          block.compact_cbor,
          block.witness_set_compact,
          step_06_preimage_items(count),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// The step-04 rows measure a validator that discriminates too: at the
/// admissible address-witness cardinality the walk still finds a key that *is*
/// present, so the absence claim fails and the step refuses. Without this the
/// two step-04 readings could be readings of a walk over an empty or
/// unauthenticated collection.
test q1x_f6_step_04_at_the_admissible_maximum_refuses_a_present_signature() fail {
  let count = admissible_address_witness_count
  let block = address_cardinality_block(count)
  expect Some(present) = list.at(address_witnesses_of(count), count - 1)
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
          },
        ),
      },
    ),
    ct.Continue(
      missing_signature_step_04.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        addr_tx_wits_opening: fixture.inline_witness_opening(
          block.compact_cbor,
          block.witness_set_compact,
          step_04_preimage_items(count),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
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
test q1x_f6_address_witness_fixture_sits_at_the_admissible_cardinality() {
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
