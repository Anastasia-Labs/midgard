//// Measured ExUnits frontier for the §5.5.3 transition-trace arms (#628).
////
//// Two questions the `#628` evidence left open, both answered here by
//// measurement rather than by interpolation:
////
////   1. Where does the deposit arm cross the §3.3 basis? The pinned reading
////      for `accepts_valid_deposit_transition_fault` sits just under the
////      memory basis with an empty `l2_datum`. This module sweeps the
////      `l2_datum` width, which is the one deposit field a user controls, and
////      brackets the crossing between adjacent *measured* rungs.
////
////   2. Is the L2 arm's ~15.2M memory reading genuinely over basis, or is it
////      an artefact of charging fixture construction to the row? Every row
////      below has a `..._fixture_only` twin that builds the identical
////      fixtures -- MPF roots, compact CBOR, tx id, descriptor ledger values,
////      pre/post roots, the header hash -- and then stops short of the
////      validator entrypoint. The difference between the two pinned readings
////      is the marginal validator cost; neither reading on its own is one.
////
//// An Aiken `test` reading is a whole-test upper bound. That is why the
//// baselines exist and why the ledger's derived rows, not these tests, carry
//// the arm-cost claims: `scripts/verify-transition-trace-descriptor-exec-
//// ledger-v1.mjs` recomputes every difference from the measured readings and
//// fails if a hand-written figure drifts from the arithmetic.
////
//// Fixture construction is deliberately shared between a row and its twin
//// through a single `*_parts` builder whose every field is consumed by both
//// flavours, so the twin cannot silently skip work the measured row paid for.
////
//// Guardrail 3 (#617): payloads are built programmatically by `filler`, never
//// pinned as source literals.

use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use env
use midgard/common/utils
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/preimages as native_preimages
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/transition_trace/proof
use midgard/hub_oracle
use midgard/ledger_output_descriptor_v1
use midgard/ledger_state
use midgard/native_tx_field_access_v1
use midgard/transition_trace
use midgard/user_events/deposit

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h28_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const h32_c =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const fraud_category_id = #"00000004"

/// `n` bytes of `0xab`, built by doubling rather than pinned as a literal.
fn filler(n: Int) -> ByteArray {
  if n <= 0 {
    #""
  } else if n == 1 {
    #"ab"
  } else {
    let half = filler(n / 2)
    let doubled = bytearray.concat(half, half)
    if n % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, #"ab")
    }
  }
}

fn out_ref(ix: Int) -> OutputReference {
  OutputReference { transaction_id: h32_a, output_index: ix }
}

fn dummy_address() -> Address {
  address.from_script(h28_a)
}

fn dummy_hub_datum() -> hub_oracle.Datum {
  hub_oracle.Datum {
    registered_operators: h28_a,
    active_operators: h28_a,
    retired_operators: h28_a,
    scheduler: h28_a,
    state_queue: h28_a,
    fraud_proof_catalogue: h28_a,
    fraud_proof: h28_a,
    deposit: h28_a,
    withdrawal: h28_a,
    tx_order: h28_a,
    settlement: h28_a,
    payout: h28_a,
    registered_operators_addr: dummy_address(),
    active_operators_addr: dummy_address(),
    retired_operators_addr: dummy_address(),
    scheduler_addr: dummy_address(),
    state_queue_addr: dummy_address(),
    fraud_proof_catalogue_addr: dummy_address(),
    fraud_proof_addr: dummy_address(),
    deposit_addr: dummy_address(),
    withdrawal_addr: dummy_address(),
    tx_order_addr: dummy_address(),
    settlement_addr: dummy_address(),
    reserve_addr: dummy_address(),
    payout_addr: dummy_address(),
    reserve_observer: h28_a,
  }
}

fn base_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: h32_a,
    utxos_root: h32_b,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28_b,
    operator_vkey: h28_a,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn prove(
  header: ledger_state.HeaderV1,
  fault: proof.TransitionFault,
) -> proof.TransitionFaultProof {
  let header_data: Data = header
  proof.TransitionFaultProof {
    challenged_header_hash: utils.serialise_and_hash_28(header_data),
    header,
    fault,
  }
}

fn asset_name_for(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  fraud_category_id
    |> bytearray.concat(utils.serialise_and_hash_28(header_data))
}

fn step(
  ix: Int,
  event_key: ledger_state.EventKey,
  phase: ledger_state.TransitionPhase,
  pre: ByteArray,
  post: ByteArray,
) -> ledger_state.TransitionStep {
  ledger_state.TransitionStep {
    schema_version: 1,
    step_index: ix,
    event_key,
    phase,
    pre_utxos_root: pre,
    post_utxos_root: post,
  }
}

fn trace_membership(
  transition_step: ledger_state.TransitionStep,
) -> transition_trace.IndexedTraceProof {
  let key_bytes = cbor.serialise(transition_step.step_index)
  let value_bytes = cbor.serialise(transition_step)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.TransitionTraceRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.TransitionTraceRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: transition_step.step_index,
    value: transition_step,
    proof: [],
  }
}

fn event_to_step_membership(
  event_key: ledger_state.EventKey,
  step_index: Int,
  phase: ledger_state.TransitionPhase,
) -> transition_trace.RootMembershipProof<
  ledger_state.EventKey,
  ledger_state.EventToStepValue,
> {
  let value = ledger_state.EventToStepValue { step_index, phase }
  let key_bytes = cbor.serialise(event_key)
  let value_bytes = cbor.serialise(value)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.EventToStepRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.EventToStepRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: event_key,
    value,
    proof: [],
  }
}

fn deposit_source_membership(
  deposit_id: ledger_state.DepositId,
  info: ledger_state.DepositInfo,
) -> transition_trace.RootMembershipProof<
  ledger_state.DepositId,
  ledger_state.DepositInfo,
> {
  let key_bytes = cbor.serialise(deposit_id)
  let value_bytes = cbor.serialise(info)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.DepositsRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.DepositsRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: deposit_id,
    value: info,
    proof: [],
  }
}

fn ledger_outref_key(reference: OutputReference) -> ByteArray {
  native_components.encode_midgard_tx_input(
    native_types.MidgardTxInput {
      tx_id: reference.transaction_id,
      output_index: reference.output_index,
    },
  )
}

fn ledger_value(output_index: Int, output_cbor: ByteArray) -> ByteArray {
  expect Some(value) = ledger_output_descriptor_v1.ledger_value_v1(
    output_index,
    output_cbor,
  )
  value
}

fn ledger_insert_witness(
  key: ByteArray,
  value: ByteArray,
) -> proof.LedgerInsertWitness {
  proof.LedgerInsertWitness {
    key,
    value,
    non_membership_proof: [],
    insert_proof: [],
  }
}

fn ledger_delete_witness(
  key: ByteArray,
  value: ByteArray,
) -> proof.LedgerDeleteWitness {
  proof.LedgerDeleteWitness {
    key,
    value,
    membership_proof: [],
    delete_proof: [],
  }
}

// ---------------------------------------------------------------------------
// Deposit arm
// ---------------------------------------------------------------------------

/// `l2_datum` for a rung: `None` at width `-1`, otherwise a `width`-byte blob.
fn sweep_datum(width: Int) -> Option<Data> {
  if width < 0 {
    None
  } else {
    let payload: Data = filler(width)
    Some(payload)
  }
}

fn sweep_deposit_info(width: Int) -> ledger_state.DepositInfo {
  ledger_state.DepositInfo {
    l2_address: dummy_address(),
    l2_network_id: 0,
    l2_datum: sweep_datum(width),
  }
}

fn deposit_value(event_asset_name: ByteArray) {
  assets.from_lovelace(2_000_000)
    |> assets.add(h28_a, event_asset_name, 1)
    |> assets.add(h28_b, #"aa", 7)
}

/// The exact L2 output bytes a deposit at this rung projects.
///
/// Mirrors `projected_deposit_output_cbor`: the datum the user supplied is
/// serialised into the projected output, so the descriptor derived from it
/// grows with the rung.
fn sweep_deposit_output_cbor(width: Int) -> ByteArray {
  let datum_cbor = when sweep_datum(width) is {
    Some(datum) -> Some(cbor.serialise(datum))
    None -> None
  }
  native_components.encode_midgard_tx_output(
    native_types.MidgardTxOutput {
      address: native_types.MidgardAddress {
        protected: False,
        network_id: 0,
        payment_credential: native_types.ScriptCredential(h28_a),
        stake_credential: None,
      },
      value: native_types.MidgardValue {
        lovelace: 2_000_000,
        assets: [Pair(h28_b |> bytearray.concat(#"aa"), 7)],
      },
      datum_cbor,
      script_ref: None,
    },
  )
}

fn deposit_reference_input(
  deposit_id: ledger_state.DepositId,
  info: ledger_state.DepositInfo,
  event_asset_name: ByteArray,
) -> Input {
  let deposit_datum = deposit.DepositDatum {
    event: ledger_state.DepositEvent { id: deposit_id, info },
    inclusion_time: 15,
    witness: h28_a,
  }
  let deposit_datum_data: Data = deposit_datum
  Input {
    output_reference: out_ref(99),
    output: Output {
      address: dummy_address(),
      value: deposit_value(event_asset_name),
      datum: InlineDatum(deposit_datum_data),
      reference_script: None,
    },
  }
}

type DepositParts {
  header: ledger_state.HeaderV1,
  fault: proof.TransitionFault,
  reference_inputs: List<Input>,
  projected_value: ByteArray,
  expected_post_root: ByteArray,
  opened_post_root: ByteArray,
}

/// Everything `accepts_valid_deposit_transition_fault` builds, at one rung.
fn deposit_parts(width: Int) -> DepositParts {
  let deposit_id = out_ref(0)
  let deposit_info = sweep_deposit_info(width)
  let event_key = ledger_state.DepositEventKey { deposit_id }
  let projected_key = ledger_outref_key(deposit_id)
  let projected_value = ledger_value(
    deposit_id.output_index,
    sweep_deposit_output_cbor(width),
  )
  let expected_post_root = proof.insert_root(
    env.empty_merkle_tree_root,
    projected_key,
    projected_value,
    [],
  )
  let opened_step = step(
    0,
    event_key,
    ledger_state.Deposit,
    env.empty_merkle_tree_root,
    h32_b,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.Deposit,
  )
  let source_membership = deposit_source_membership(deposit_id, deposit_info)
  let event_asset_name = #"01"
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    deposits_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    deposit_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
  DepositParts {
    header,
    fault: proof.InvalidOneStepTransition {
      witness: proof.ValidDepositTransition {
        trace_proof,
        event_to_step,
        source_membership,
        event_ref_input_index: 0,
        event_asset_name,
        projected_utxo: ledger_insert_witness(projected_key, projected_value),
      },
    },
    reference_inputs: [
      deposit_reference_input(deposit_id, deposit_info, event_asset_name),
    ],
    projected_value,
    expected_post_root,
    opened_post_root: opened_step.post_utxos_root,
  }
}

/// Force every fixture a deposit rung built, without entering the validator.
///
/// Each field of `DepositParts` -- and each field of the witness inside
/// `fault` -- is projected here, so the baseline cannot be cheaper than the
/// measured row by skipping construction the measured row paid for.
fn deposit_fixtures_forced(parts: DepositParts) -> Bool {
  expect proof.InvalidOneStepTransition { witness } = parts.fault
  expect
      proof.ValidDepositTransition {
        trace_proof,
        event_to_step,
        source_membership,
        projected_utxo,
        ..
      }
    = witness
  let header_data: Data = parts.header
  and {
    parts.expected_post_root != parts.opened_post_root,
    bytearray.length(parts.projected_value) > 0,
    bytearray.length(projected_utxo.key) > 0,
    bytearray.length(trace_proof.root) == 32,
    bytearray.length(event_to_step.root) == 32,
    bytearray.length(source_membership.root) == 32,
    list.length(parts.reference_inputs) == 1,
    bytearray.length(utils.serialise_and_hash_28(header_data)) == 28,
    bytearray.length(asset_name_for(parts.header)) == 32,
  }
}

fn deposit_arm(width: Int) -> Bool {
  let parts = deposit_parts(width)
  expect parts.expected_post_root != parts.opened_post_root
  proof.validate_transition_fault_proof(
    prove(parts.header, parts.fault),
    asset_name_for(parts.header),
    dummy_hub_datum(),
    parts.reference_inputs,
  )
}

fn deposit_baseline(width: Int) -> Bool {
  deposit_fixtures_forced(deposit_parts(width))
}

test deposit_arm_datum_absent() {
  deposit_arm(-1)
}

test deposit_arm_datum_absent_fixture_only() {
  deposit_baseline(-1)
}

// A `Some(#"")` rung is deliberately absent from the ladder. It cannot be
// measured: an otherwise-canonical output whose inline datum is the empty byte
// string has no §5.3 descriptor at all -- `ledger_value_v1` returns `None` and
// the fixture builder aborts before any arm is entered. That behaviour is
// pinned as an assertion, not as a cost row, in
// `midgard/ledger_output_descriptor_v1_exunits_frontier.test`.

test deposit_arm_datum_1_byte() {
  deposit_arm(1)
}

test deposit_arm_datum_1_byte_fixture_only() {
  deposit_baseline(1)
}

test deposit_arm_datum_8_bytes() {
  deposit_arm(8)
}

test deposit_arm_datum_8_bytes_fixture_only() {
  deposit_baseline(8)
}

test deposit_arm_datum_16_bytes() {
  deposit_arm(16)
}

test deposit_arm_datum_16_bytes_fixture_only() {
  deposit_baseline(16)
}

test deposit_arm_datum_24_bytes() {
  deposit_arm(24)
}

test deposit_arm_datum_24_bytes_fixture_only() {
  deposit_baseline(24)
}

test deposit_arm_datum_32_bytes() {
  deposit_arm(32)
}

test deposit_arm_datum_32_bytes_fixture_only() {
  deposit_baseline(32)
}

test deposit_arm_datum_48_bytes() {
  deposit_arm(48)
}

test deposit_arm_datum_48_bytes_fixture_only() {
  deposit_baseline(48)
}

test deposit_arm_datum_64_bytes() {
  deposit_arm(64)
}

test deposit_arm_datum_64_bytes_fixture_only() {
  deposit_baseline(64)
}

test deposit_arm_datum_128_bytes() {
  deposit_arm(128)
}

test deposit_arm_datum_128_bytes_fixture_only() {
  deposit_baseline(128)
}

test deposit_arm_datum_256_bytes() {
  deposit_arm(256)
}

test deposit_arm_datum_256_bytes_fixture_only() {
  deposit_baseline(256)
}

// ---------------------------------------------------------------------------
// L2 arm
// ---------------------------------------------------------------------------

fn native_input(
  tx_id: ByteArray,
  output_index: Int,
) -> native_types.MidgardTxInput {
  native_types.MidgardTxInput { tx_id, output_index }
}

fn native_output() -> native_types.MidgardTxOutput {
  native_types.MidgardTxOutput {
    address: native_types.MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: native_types.PubKeyCredential(h28_a),
      stake_credential: None,
    },
    value: native_types.MidgardValue { lovelace: 1_000_000, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

fn native_l2_witness_set_compact_cbor() -> ByteArray {
  native_compact.encode_native_tx_witness_set_compact(
    native_types.NativeTxWitnessSetCompact {
      addr_tx_wits_hash: h32_a,
      script_tx_wits_hash: h32_b,
      redeemer_tx_wits_hash: h32_c,
    },
  )
}

fn native_l2_field_preimage_lengths_cbor() -> ByteArray {
  native_compact.encode_native_tx_field_preimage_lengths_v1(
    native_types.NativeTxFieldPreimageLengthsV1 {
      spend_inputs: 0,
      reference_inputs: 0,
      outputs: 0,
      required_observers: 0,
      required_signers: 0,
      mint: 0,
      address_witnesses: 0,
      script_witnesses: 0,
      redeemers: 0,
    },
  )
}

fn native_l2_compact_cbor(
  spend_inputs_preimage: ByteArray,
  outputs_preimage: ByteArray,
) -> ByteArray {
  native_compact.encode_native_tx_compact_v1(
    native_types.NativeTxCompact {
      body: native_types.NativeTxBodyCompact {
        spend_inputs_hash: native_tx_field_access_v1.field_commitment(
          spend_inputs_preimage,
        ),
        reference_inputs_hash: h32_b,
        outputs_hash: native_tx_field_access_v1.field_commitment(
          outputs_preimage,
        ),
        fee: 0,
        validity_interval_start: -1,
        validity_interval_end: -1,
        required_observers_hash: h32_b,
        required_signers_hash: h32_b,
        mint_hash: h32_b,
        script_integrity_hash: h32_b,
        auxiliary_data_hash: h32_b,
        network_id: 0,
      },
      witness_set_hash: blake2b_256(native_l2_witness_set_compact_cbor()),
      validity_code: 0,
    },
  )
}

fn native_l2_tx_id(compact_cbor: ByteArray) -> ByteArray {
  let compact = native_compact.decode_native_tx_compact_v1(compact_cbor)
  native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
}

fn raw_l2_source_membership(
  tx_id: ByteArray,
  compact_cbor: ByteArray,
) -> transition_trace.RootMembershipProof<ByteArray, ByteArray> {
  let source = ledger_state.L2TransactionSourceV1 {
    tx_id,
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor,
      witness_set_compact_cbor: native_l2_witness_set_compact_cbor(),
      field_preimage_lengths_cbor: native_l2_field_preimage_lengths_cbor(),
    },
  }
  let source_cbor = cbor.serialise(source)
  let phas_root = mpf.root(mpf.insert(mpf.empty, tx_id, source_cbor, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.TransactionsV1RootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.TransactionsV1RootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: tx_id,
    value: source_cbor,
    proof: [],
  }
}

type L2Parts {
  header: ledger_state.HeaderV1,
  fault: proof.TransitionFault,
  pre_root: ByteArray,
  expected_post_root: ByteArray,
  opened_post_root: ByteArray,
}

/// Everything `accepts_valid_l2_transaction_no_op_transition_fault` builds.
fn l2_no_op_parts() -> L2Parts {
  let spent_input = native_input(h32_a, 0)
  let spent_key = native_components.encode_midgard_tx_input(spent_input)
  let spent_value = ledger_value(
    spent_input.output_index,
    native_components.encode_midgard_tx_output(native_output()),
  )
  let produced_output = native_output()
  let produced_value = ledger_value(
    0,
    native_components.encode_midgard_tx_output(produced_output),
  )
  let spend_inputs_preimage = native_preimages.encode_input_preimage(
    [spent_input],
  )
  let outputs_preimage = native_preimages.encode_output_preimage(
    [produced_output],
  )
  let compact_cbor = native_l2_compact_cbor(
    spend_inputs_preimage,
    outputs_preimage,
  )
  let tx_id = native_l2_tx_id(compact_cbor)
  let produced_key = native_components.encode_midgard_tx_input(
    native_input(tx_id, 0),
  )
  let source_membership = raw_l2_source_membership(tx_id, compact_cbor)
  let pre_root = mpf.root(mpf.insert(mpf.empty, spent_key, spent_value, []))
  let expected_post_root = proof.insert_root(
    proof.delete_root(pre_root, spent_key, spent_value, []),
    produced_key,
    produced_value,
    [],
  )
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let opened_step = step(
    0,
    event_key,
    ledger_state.L2Transaction,
    pre_root,
    pre_root,
  )
  let trace_proof = trace_membership(opened_step)
  let event_to_step = event_to_step_membership(
    event_key,
    0,
    ledger_state.L2Transaction,
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    transactions_root: source_membership.root,
    transition_trace_root: trace_proof.root,
    event_to_step_root: event_to_step.root,
    l2_transaction_count: 1,
    total_event_count: 1,
    transition_step_count: 1,
  }
  L2Parts {
    header,
    fault: proof.InvalidOneStepTransition {
      witness: proof.L2TransactionTransition {
        trace_proof,
        event_to_step,
        source_membership,
        spend_inputs_preimage,
        outputs_preimage,
        spent_utxos: [ledger_delete_witness(spent_key, spent_value)],
        produced_utxos: [ledger_insert_witness(produced_key, produced_value)],
      },
    },
    pre_root,
    expected_post_root,
    opened_post_root: opened_step.post_utxos_root,
  }
}

fn l2_fixtures_forced(parts: L2Parts) -> Bool {
  expect proof.InvalidOneStepTransition { witness } = parts.fault
  expect
      proof.L2TransactionTransition {
        trace_proof,
        event_to_step,
        source_membership,
        spend_inputs_preimage,
        outputs_preimage,
        spent_utxos,
        produced_utxos,
      }
    = witness
  let header_data: Data = parts.header
  and {
    parts.expected_post_root != parts.opened_post_root,
    bytearray.length(parts.pre_root) == 32,
    bytearray.length(trace_proof.root) == 32,
    bytearray.length(event_to_step.root) == 32,
    bytearray.length(source_membership.root) == 32,
    bytearray.length(spend_inputs_preimage) > 0,
    bytearray.length(outputs_preimage) > 0,
    list.length(spent_utxos) == 1,
    list.length(produced_utxos) == 1,
    bytearray.length(utils.serialise_and_hash_28(header_data)) == 28,
    bytearray.length(asset_name_for(parts.header)) == 32,
  }
}

test l2_no_op_arm() {
  let parts = l2_no_op_parts()
  expect parts.expected_post_root != parts.opened_post_root
  proof.validate_accepted_transaction_fault_proof(
    prove(parts.header, parts.fault),
    asset_name_for(parts.header),
  )
}

test l2_no_op_arm_fixture_only() {
  l2_fixtures_forced(l2_no_op_parts())
}

// ---------------------------------------------------------------------------
// Neutralisation selectors
//
// A cost row over an arm that accepts everything measures nothing. These two
// run in the same invocation as every reading -- the exec ledger declares them
// and the verifier refuses a ledger whose selectors did not run -- and show
// that both measured arms still reject: the deposit arm returns `False` when
// the projected value is not the descriptor it must be, and the L2 arm aborts
// rather than replaying full output bytes into `utxos_root`, which is the #628
// hole itself.
// ---------------------------------------------------------------------------

/// The serialised width of the `l2_datum` a rung carries, in bytes.
///
/// This is the axis the deposit sweep's crossing is published on, so it is
/// pinned here rather than asserted only in the exec ledger: `0` means the
/// field is absent altogether, `1` is a present-but-empty blob, and the rest
/// grow with the user-supplied payload.
fn sweep_datum_cbor_length(width: Int) -> Int {
  when sweep_datum(width) is {
    Some(datum) -> bytearray.length(cbor.serialise(datum))
    None -> 0
  }
}

test deposit_datum_cbor_lengths_are_exact() {
  and {
    sweep_datum_cbor_length(-1) == 0,
    sweep_datum_cbor_length(1) == 2,
    sweep_datum_cbor_length(8) == 9,
    sweep_datum_cbor_length(16) == 17,
    sweep_datum_cbor_length(24) == 26,
    sweep_datum_cbor_length(32) == 34,
    sweep_datum_cbor_length(48) == 50,
    sweep_datum_cbor_length(64) == 66,
    sweep_datum_cbor_length(128) == 134,
    sweep_datum_cbor_length(256) == 266,
  }
}

test deposit_arm_rejects_arbitrary_projected_value() {
  let parts = deposit_parts(-1)
  expect proof.InvalidOneStepTransition { witness } = parts.fault
  expect
      proof.ValidDepositTransition {
        trace_proof,
        event_to_step,
        source_membership,
        event_ref_input_index,
        event_asset_name,
        projected_utxo,
      }
    = witness
  let tampered = proof.InvalidOneStepTransition {
    witness: proof.ValidDepositTransition {
      trace_proof,
      event_to_step,
      source_membership,
      event_ref_input_index,
      event_asset_name,
      projected_utxo: ledger_insert_witness(projected_utxo.key, #"deadbeef"),
    },
  }
  !proof.validate_transition_fault_proof(
    prove(parts.header, tampered),
    asset_name_for(parts.header),
    dummy_hub_datum(),
    parts.reference_inputs,
  )
}

test l2_arm_refuses_full_output_replay() fail {
  let parts = l2_no_op_parts()
  expect proof.InvalidOneStepTransition { witness } = parts.fault
  expect
      proof.L2TransactionTransition {
        trace_proof,
        event_to_step,
        source_membership,
        spend_inputs_preimage,
        outputs_preimage,
        spent_utxos,
        produced_utxos,
      }
    = witness
  expect [produced] = produced_utxos
  let tampered = proof.InvalidOneStepTransition {
    witness: proof.L2TransactionTransition {
      trace_proof,
      event_to_step,
      source_membership,
      spend_inputs_preimage,
      outputs_preimage,
      spent_utxos,
      produced_utxos: [
        ledger_insert_witness(
          produced.key,
          native_components.encode_midgard_tx_output(native_output()),
        ),
      ],
    },
  }
  proof.validate_accepted_transaction_fault_proof(
    prove(parts.header, tampered),
    asset_name_for(parts.header),
  )
}
