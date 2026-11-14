//// Validator-level selectors for the `transactionOutputNonCanonical` physical
//// chain: the exact-item authentication of step 02, the successor discipline
//// and checkpoint seams of the resumable step 03, and the terminal polarity
//// of step 04, each driven through the applied spend handler over the shared
//// native-V1 block fixture.

use aiken/primitive/bytearray
use cardano/transaction.{Input, Transaction}
use fraud_proofs/transaction_output_non_canonical/step_02 as step_02_validator
use fraud_proofs/transaction_output_non_canonical/step_03 as step_03_validator
use fraud_proofs/transaction_output_non_canonical/step_03_datum as step_03_datum_validator
use fraud_proofs/transaction_output_non_canonical/step_04 as step_04_validator
use midgard/canonical_plutus_data_stream_v1 as stream
use midgard/computation_thread as ct
use midgard/field_grammar_certificate_v1.{FieldGrammarCertificateV1}
use midgard/fraud_proofs/field_grammar_fixture_v1 as grammar
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput, MidgardValue}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/transaction_output_non_canonical/rule
use midgard/fraud_proofs/transaction_output_non_canonical/step_02.{
  Args as Step02Args, Datum as Step02Datum, SpendRedeemer as Step02Redeemer,
}
use midgard/fraud_proofs/transaction_output_non_canonical/step_03.{
  Args as Step03Args,
}
use midgard/fraud_proofs/transaction_output_non_canonical/step_03_datum.{
  Args as Step03DatumArgs,
}
use midgard/fraud_proofs/transaction_output_non_canonical/step_04.{
  Args as Step04Args,
}
use midgard/rejection_reason_v1.{OutputNonCanonical}

const malformed_output =
  #"b80200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

fn block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn asset_name() -> ByteArray {
  fixture.ct_token_asset_name(block())
}

/// The committed field-2 item of the fixture transaction, as the block encodes it.
fn committed_output() -> ByteArray {
  encode_midgard_tx_output(fixture.sample_native_output())
}

/// A second canonical output that is not the committed one.
fn other_output() -> ByteArray {
  let base = fixture.sample_native_output()
  let value = base.value
  encode_midgard_tx_output(
    MidgardTxOutput {
      ..base,
      value: MidgardValue { ..value, lovelace: value.lovelace + 1 },
    },
  )
}

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: block().tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn rejected() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: block().tx_id,
    source_key: #"d8799f581c000000000000000000000000000000000000000000000000000000001affffffff",
    rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
  }
}

fn bound(index: Int) -> rule.BoundOutputV1 {
  rule.BoundOutputV1 { subject: accepted(), output_index: index }
}

// ## Step 02: exact field-2 item authentication

fn run_step_02(
  index: Int,
  items: List<ByteArray>,
  next_state: rule.OutputScanStateV1,
  successor: ByteArray,
) -> Bool {
  let next_state_data: Data = next_state
  step_02_validator.main.spend(
    successor,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    grammar.test_grammar_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound(index)),
      },
    ),
    ct.Continue(
      Step02Args {
        input_index: 0,
        output_index: 0,
        opening: fixture.inline_body_opening(block().compact_cbor, items),
        grammar_ref_input_index: None,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name(), next_state_data),
  )
}

test step_02_binds_the_committed_item_at_the_bound_coordinate() {
  run_step_02(
    0,
    [committed_output()],
    rule.initial_scan_v1(bound(0), committed_output()),
    fixture.next_step_script_hash,
  )
}

test step_02_out_of_range_coordinate_refuses() fail {
  run_step_02(
    1,
    [committed_output()],
    rule.initial_scan_v1(bound(1), committed_output()),
    fixture.next_step_script_hash,
  )
}

test step_02_substituted_field_bytes_refuse() fail {
  run_step_02(
    0,
    [other_output()],
    rule.initial_scan_v1(bound(0), other_output()),
    fixture.next_step_script_hash,
  )
}

test step_02_forged_initial_checkpoint_refuses() fail {
  let genuine = rule.initial_scan_v1(bound(0), committed_output())
  run_step_02(
    0,
    [committed_output()],
    rule.OutputScanStateV1 { ..genuine, item_length: genuine.item_length + 1 },
    fixture.next_step_script_hash,
  )
}

test step_02_wrong_successor_refuses() fail {
  run_step_02(
    0,
    [committed_output()],
    rule.initial_scan_v1(bound(0), committed_output()),
    fixture.step_script_hash,
  )
}

// ## Step 02 through the field grammar certificate
//
// 761 outputs of 43 enveloped bytes fill field 2 to 32,726 bytes, under tier
// 3. The bound item sits at 751, fifteen items past its waypoint, so the read
// pays the most header reads a certified read can.

const certified_count = 761

const certified_index = 751

fn certified_target() -> ByteArray {
  fixture.repeated_bytes_v1(#"b2", 41)
}

fn certified_preimage() -> ByteArray {
  grammar.field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 41),
    certified_count,
    certified_index,
    certified_target(),
  )
}

type Step02Case {
  datum: Option<Step02Datum>,
  redeemer: Step02Redeemer,
  tx: Transaction,
}

/// Step 02 at output `index` of the certified field, with the certificate
/// `certificate(tx_id, preimage)` builds and the scan opened on `item`.
fn certified_step_02(
  index: Int,
  certificate: fn(ByteArray, ByteArray) -> FieldGrammarCertificateV1,
  item: ByteArray,
) -> Step02Case {
  let preimage = certified_preimage()
  tier3_step_02(
    preimage,
    index,
    item,
    fn(tx_id) {
      Pair(
        Some(grammar.grammar_ref_input_index_v1(preimage)),
        grammar.certified_grammar_reference_inputs_v1(
          tx_id,
          2,
          preimage,
          certificate(tx_id, preimage),
        ),
      )
    },
  )
}

/// Step 02 at output `index` of `preimage` under tier 3, the scan opened on
/// `item`, with the grammar certificate slot and reference inputs `route`
/// gives the committing transaction's id.
fn tier3_step_02(
  preimage: ByteArray,
  index: Int,
  item: ByteArray,
  route: fn(ByteArray) -> Pair<Option<Int>, List<Input>>,
) -> Step02Case {
  let committed = fixture.committed_field_preimage_block_fixture_v1(
    block(),
    2,
    preimage,
  )
  let bound = rule.BoundOutputV1 {
    subject: substrate.VerdictSubjectV1 {
      ..accepted(),
      transaction_id: committed.tx_id,
    },
    output_index: index,
  }
  let next_state: Data = rule.initial_scan_v1(bound, item)
  let Pair(grammar_ref_input_index, reference_inputs) = route(committed.tx_id)
  Step02Case {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    redeemer: ct.Continue(
      Step02Args {
        input_index: 0,
        output_index: 0,
        opening: fixture.certified_body_opening(
          committed.compact_cbor,
          preimage,
        ),
        grammar_ref_input_index,
      },
    ),
    tx: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name(), next_state),
      reference_inputs,
    },
  }
}

fn honest_certificate(
  tx_id: ByteArray,
  preimage: ByteArray,
) -> FieldGrammarCertificateV1 {
  grammar.complete_field_grammar_certificate_v1(
    tx_id,
    2,
    preimage,
    certified_count,
  )
}

fn run_certified_step_02(case: Step02Case) -> Bool {
  step_02_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    grammar.test_grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

test step_02_certified_route_fixture_only_at_the_tier3_maximum() {
  let case = certified_step_02(
    certified_index,
    honest_certificate,
    certified_target(),
  )
  case.datum != None
}

test step_02_certified_route_at_the_tier3_maximum_cost() {
  run_certified_step_02(
    certified_step_02(certified_index, honest_certificate, certified_target()),
  )
}

test step_02_certified_route_reads_the_last_output() {
  run_certified_step_02(
    certified_step_02(
      certified_count - 1,
      honest_certificate,
      fixture.repeated_bytes_v1(#"a1", 41),
    ),
  )
}

test step_02_certified_route_binds_the_item_it_read() fail {
  run_certified_step_02(
    certified_step_02(
      certified_index,
      honest_certificate,
      fixture.repeated_bytes_v1(#"a1", 41),
    ),
  )
}

test step_02_certified_route_index_past_the_count_refuses() fail {
  run_certified_step_02(
    certified_step_02(certified_count, honest_certificate, certified_target()),
  )
}

test step_02_certified_route_refuses_another_fields_certificate() fail {
  run_certified_step_02(
    certified_step_02(
      certified_index,
      fn(tx_id, preimage) {
        grammar.complete_field_grammar_certificate_v1(
          tx_id,
          5,
          preimage,
          certified_count,
        )
      },
      certified_target(),
    ),
  )
}

test step_02_certified_route_refuses_another_transactions_certificate() fail {
  run_certified_step_02(
    certified_step_02(
      certified_index,
      fn(_tx_id, preimage) {
        grammar.complete_field_grammar_certificate_v1(
          block().tx_id,
          2,
          preimage,
          certified_count,
        )
      },
      certified_target(),
    ),
  )
}

test step_02_certified_route_refuses_another_preimages_certificate() fail {
  run_certified_step_02(
    certified_step_02(
      certified_index,
      fn(tx_id, _preimage) {
        grammar.complete_field_grammar_certificate_v1(
          tx_id,
          2,
          grammar.field_preimage_with_item_v1(
            fixture.repeated_bytes_v1(#"a1", 41),
            certified_count,
            0,
            certified_target(),
          ),
          certified_count,
        )
      },
      certified_target(),
    ),
  )
}

test step_02_certified_route_refuses_an_in_progress_certificate() fail {
  run_certified_step_02(
    certified_step_02(
      certified_index,
      fn(tx_id, preimage) {
        grammar.in_progress_field_grammar_certificate_v1(
          tx_id,
          2,
          preimage,
          certified_count,
        )
      },
      certified_target(),
    ),
  )
}

// ### The walked route at its limit
//
// A submitter walks a field of at most `grammar.walked_item_limit` items. The
// walk is dearest under tier 3, near the field maximum, to the last item.

fn walked_limit_target() -> ByteArray {
  fixture.repeated_bytes_v1(#"b2", 500)
}

fn walked_limit_step_02() -> Step02Case {
  let preimage = grammar.walked_limit_preimage_v1(walked_limit_target())
  tier3_step_02(
    preimage,
    grammar.walked_item_limit - 1,
    walked_limit_target(),
    fn(tx_id) {
      Pair(None, fixture.certified_reference_inputs_v1(tx_id, 2, preimage))
    },
  )
}

test step_02_walked_route_fixture_only_at_the_walked_limit() {
  walked_limit_step_02().datum != None
}

test step_02_walked_route_at_the_walked_limit_cost() {
  run_certified_step_02(walked_limit_step_02())
}

// ## Step 03: resumable scan successor and checkpoint seams

fn initial_forced_scan() -> rule.OutputScanStateV1 {
  rule.initial_scan_v1(rule.bind_output_v1(rejected(), 0), committed_output())
}

/// The last still-scanning checkpoint before the terminal transition.
fn last_scanning(state: rule.OutputScanStateV1) -> rule.OutputScanStateV1 {
  let next = rule.advance_scan_v1(state, committed_output())
  if next.outcome == rule.outcome_scanning {
    last_scanning(next)
  } else {
    state
  }
}

/// A script hash no successor in these tests pays to.
const unused_script_hash =
  #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

fn run_step_03(
  state: rule.OutputScanStateV1,
  window: ByteArray,
  next_state: rule.OutputScanStateV1,
  same_step: Bool,
) -> Bool {
  let next_state_data: Data = next_state
  run_step_03_to(
    unused_script_hash,
    fixture.next_step_script_hash,
    state,
    window,
    next_state_data,
    same_step,
  )
}

fn run_step_03_to(
  datum_step: ByteArray,
  terminal_step: ByteArray,
  state: rule.OutputScanStateV1,
  window: ByteArray,
  next_state_data: Data,
  same_step: Bool,
) -> Bool {
  step_03_validator.main.spend(
    datum_step,
    terminal_step,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(Step03Args { input_index: 0, output_index: 0, window }),
    fixture.own_out_ref(),
    if same_step {
      fixture.continue_same_step_l1_tx_v1(asset_name(), next_state_data)
    } else {
      fixture.continue_mid_step_l1_tx_v1(asset_name(), next_state_data)
    },
  )
}

test step_03_scanning_transition_returns_to_its_own_script() {
  let state = initial_forced_scan()
  let next = rule.advance_scan_v1(state, committed_output())
  expect next.outcome == rule.outcome_scanning
  run_step_03(state, committed_output(), next, True)
}

test step_03_scanning_transition_to_step_04_refuses() fail {
  let state = initial_forced_scan()
  let next = rule.advance_scan_v1(state, committed_output())
  expect next.outcome == rule.outcome_scanning
  run_step_03(state, committed_output(), next, False)
}

test step_03_terminal_transition_hands_over_to_step_04() {
  let state = last_scanning(initial_forced_scan())
  let next = rule.advance_scan_v1(state, committed_output())
  expect next.outcome == rule.outcome_canonical
  run_step_03(state, committed_output(), next, False)
}

test step_03_terminal_transition_to_its_own_script_refuses() fail {
  let state = last_scanning(initial_forced_scan())
  let next = rule.advance_scan_v1(state, committed_output())
  expect next.outcome == rule.outcome_canonical
  run_step_03(state, committed_output(), next, True)
}

test step_03_substituted_window_refuses() fail {
  let state = initial_forced_scan()
  let next = rule.advance_scan_v1(state, committed_output())
  run_step_03(state, malformed_output, next, True)
}

test step_03_short_window_refuses() fail {
  let state = initial_forced_scan()
  let next = rule.advance_scan_v1(state, committed_output())
  run_step_03(state, bytearray.take(committed_output(), 20), next, True)
}

test step_03_forged_checkpoint_refuses() fail {
  let state = initial_forced_scan()
  let next = rule.advance_scan_v1(state, committed_output())
  expect next.outcome == rule.outcome_scanning
  run_step_03(
    state,
    committed_output(),
    rule.OutputScanStateV1 { ..next, outcome: rule.outcome_canonical },
    False,
  )
}

test step_03_replayed_checkpoint_refuses() fail {
  let state = initial_forced_scan()
  run_step_03(state, committed_output(), state, True)
}

// ## Step 03: routing to the datum step

/// An output carrying `datum` inline.
fn datum_output(datum: ByteArray) -> ByteArray {
  encode_midgard_tx_output(
    MidgardTxOutput {
      ..fixture.sample_native_output(),
      datum_cbor: Some(datum),
    },
  )
}

fn scan_of(output: ByteArray) -> rule.OutputScanStateV1 {
  rule.initial_scan_v1(rule.bind_output_v1(rejected(), 0), output)
}

/// The last envelope checkpoint before its terminal transition.
fn last_scanning_of(output: ByteArray) -> rule.OutputScanStateV1 {
  last_before(scan_of(output), output)
}

fn last_before(
  state: rule.OutputScanStateV1,
  output: ByteArray,
) -> rule.OutputScanStateV1 {
  let next = rule.advance_scan_v1(state, output)
  if next.outcome == rule.outcome_scanning {
    last_before(next, output)
  } else {
    state
  }
}

fn envelope_terminal(output: ByteArray) -> rule.OutputScanStateV1 {
  rule.advance_scan_v1(last_scanning_of(output), output)
}

fn opened_datum(output: ByteArray) -> rule.DatumScanStateV1 {
  expect Some(datum) = rule.open_datum_v1(envelope_terminal(output))
  datum
}

test step_03_canonical_envelope_with_datum_opens_the_datum_step() {
  let output = datum_output(#"d8799f0102ff")
  let next: Data = opened_datum(output)
  run_step_03_to(
    fixture.next_step_script_hash,
    unused_script_hash,
    last_scanning_of(output),
    output,
    next,
    False,
  )
}

test step_03_canonical_envelope_with_datum_skipping_its_datum_refuses() fail {
  let output = datum_output(#"9fff")
  let next: Data = envelope_terminal(output)
  run_step_03_to(
    unused_script_hash,
    fixture.next_step_script_hash,
    last_scanning_of(output),
    output,
    next,
    False,
  )
}

test step_03_datum_step_opened_mid_datum_refuses() fail {
  let output = datum_output(#"9f0102ff")
  let opened = opened_datum(output)
  let next: Data = rule.DatumScanStateV1 { ..opened, cursor: opened.cursor + 1 }
  run_step_03_to(
    fixture.next_step_script_hash,
    unused_script_hash,
    last_scanning_of(output),
    output,
    next,
    False,
  )
}

// ## Step 03 datum: the bounded datum read

fn run_datum_step(
  state: rule.DatumScanStateV1,
  window: ByteArray,
  revealed: List<stream.StreamFrameV1>,
  budget: Int,
  next_state: Data,
  same_step: Bool,
) -> Bool {
  step_03_datum_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Step03DatumArgs {
        input_index: 0,
        output_index: 0,
        window,
        revealed,
        tail_root: stream.empty_stack_root,
        node_budget: budget,
      },
    ),
    fixture.own_out_ref(),
    if same_step {
      fixture.continue_same_step_l1_tx_v1(asset_name(), next_state)
    } else {
      fixture.continue_mid_step_l1_tx_v1(asset_name(), next_state)
    },
  )
}

fn one_node_in(datum: rule.DatumScanStateV1) -> rule.DatumScanStateV1 {
  rule.DatumScanStateV1 {
    ..datum,
    cursor: datum.cursor + 1,
    stack_root: stream.stack_root_v1(
      [stream.ListFrame { started: False }],
      stream.empty_stack_root,
    ),
  }
}

test step_03_datum_paused_read_returns_to_its_own_script() {
  let output = datum_output(#"9f0102ff")
  let opened = opened_datum(output)
  let next: Data = one_node_in(opened)
  run_datum_step(opened, output, [], 1, next, True)
}

test step_03_datum_paused_read_handed_to_step_04_refuses() fail {
  let output = datum_output(#"9f0102ff")
  let opened = opened_datum(output)
  let next: Data = opened.scan
  run_datum_step(opened, output, [], 1, next, False)
}

test step_03_datum_resumes_from_its_stored_stack() {
  let output = datum_output(#"9f0102ff")
  let paused = one_node_in(opened_datum(output))
  let next: Data = paused.scan
  run_datum_step(
    paused,
    output,
    [stream.ListFrame { started: False }],
    10,
    next,
    False,
  )
}

test step_03_datum_canonical_read_hands_the_canonical_terminal_to_step_04() {
  let output = datum_output(#"9f0102ff")
  let opened = opened_datum(output)
  let next: Data = opened.scan
  run_datum_step(opened, output, [], 10, next, False)
}

test step_03_datum_finished_read_on_its_own_script_refuses() fail {
  let output = datum_output(#"9f0102ff")
  let opened = opened_datum(output)
  let next: Data = opened.scan
  run_datum_step(opened, output, [], 10, next, True)
}

test step_03_datum_non_canonical_read_hands_the_non_canonical_terminal_to_step_04() {
  let output = datum_output(#"9f01ff00")
  let opened = opened_datum(output)
  let next: Data = rule.OutputScanStateV1 {
    ..opened.scan,
    outcome: rule.outcome_non_canonical,
  }
  run_datum_step(opened, output, [], 10, next, False)
}

test step_03_datum_non_canonical_read_claimed_canonical_refuses() fail {
  let output = datum_output(#"9f01ff00")
  let opened = opened_datum(output)
  let next: Data = opened.scan
  run_datum_step(opened, output, [], 10, next, False)
}

test step_03_datum_canonical_read_claimed_non_canonical_refuses() fail {
  let output = datum_output(#"9f0102ff")
  let opened = opened_datum(output)
  let next: Data = rule.OutputScanStateV1 {
    ..opened.scan,
    outcome: rule.outcome_non_canonical,
  }
  run_datum_step(opened, output, [], 10, next, False)
}

test step_03_datum_substituted_window_refuses() fail {
  let output = datum_output(#"9f0102ff")
  let opened = opened_datum(output)
  let next: Data = opened.scan
  run_datum_step(opened, datum_output(#"9f0103ff"), [], 10, next, False)
}

test step_03_datum_forged_stack_refuses() fail {
  let output = datum_output(#"9f0102ff")
  let paused = one_node_in(opened_datum(output))
  let next: Data = paused.scan
  run_datum_step(
    paused,
    output,
    [stream.ListFrame { started: True }],
    10,
    next,
    False,
  )
}

// ## Step 04: terminal polarity

fn run_step_04(state: rule.OutputScanStateV1) -> Bool {
  step_04_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Step04Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name()),
  )
}

fn terminal_of(state: rule.OutputScanStateV1) -> rule.OutputScanStateV1 {
  rule.advance_scan_v1(last_scanning(state), committed_output())
}

test step_04_mints_on_a_forced_canonical_terminal() {
  run_step_04(terminal_of(initial_forced_scan()))
}

test step_04_refuses_a_scanning_checkpoint() fail {
  run_step_04(initial_forced_scan())
}

test step_04_refuses_the_honest_accepted_terminal() fail {
  run_step_04(terminal_of(rule.initial_scan_v1(bound(0), committed_output())))
}
