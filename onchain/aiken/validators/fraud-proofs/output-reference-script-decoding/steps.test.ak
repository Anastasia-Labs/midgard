//// Validator-level selectors for the `outputReferenceScriptDecoding` steps
//// that read the bound output out of field 2: step 02, which opens the
//// descriptor scan on it, and step 04, which binds its reference script. Each
//// is driven through the applied spend handler by both routes: walking a small
//// field under tier 1, and through the field grammar certificate over a field
//// of 761 outputs under tier 3.

use aiken/primitive/bytearray
use cardano/transaction.{Input, Transaction}
use fraud_proofs/output_reference_script_decoding/step_02 as step_02_validator
use fraud_proofs/output_reference_script_decoding/step_04 as step_04_validator
use midgard/computation_thread as ct
use midgard/field_grammar_certificate_v1.{FieldGrammarCertificateV1}
use midgard/fraud_proofs/field_grammar_fixture_v1 as grammar
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as scripts
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/output_reference_script_decoding/rule
use midgard/fraud_proofs/output_reference_script_decoding/step_02.{
  Args as Step02Args, Datum as Step02Datum, SpendRedeemer as Step02Redeemer,
}
use midgard/fraud_proofs/output_reference_script_decoding/step_04.{
  Args as Step04Args, Datum as Step04Datum, SpendRedeemer as Step04Redeemer,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

/// No case here reads a decode record, so no reference input carries this.
const script_decode_policy_id =
  #"7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b"

fn block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn asset_name() -> ByteArray {
  fixture.ct_token_asset_name(block())
}

/// The committed field-2 item of the small fixture transaction.
fn committed_output() -> ByteArray {
  encode_midgard_tx_output(fixture.sample_native_output())
}

/// A canonical output carrying a native signature reference script.
fn scripted_output() -> ByteArray {
  bytearray.concat(
    #"a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a003",
    scripts.signature_script_item_v1(),
  )
}

fn bound(tx_id: ByteArray, index: Int) -> rule.BoundOutputV1 {
  rule.BoundOutputV1 {
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    output_index: index,
    accused_class: rule.result_pending,
  }
}

fn scanned(
  state: rule.OutputDescriptorStateV1,
  output: ByteArray,
  fuel: Int,
) -> rule.OutputDescriptorStateV1 {
  if state.outcome != rule.output_scanning {
    state
  } else {
    expect fuel > 0
    scanned(rule.advance_output_scan_v1(state, output), output, fuel - 1)
  }
}

/// The step 03 terminal over `output` at `index`: the state step 04 reads.
fn descriptor(
  tx_id: ByteArray,
  index: Int,
  output: ByteArray,
) -> rule.OutputDescriptorStateV1 {
  let state = scanned(
    rule.initial_output_scan_v1(bound(tx_id, index), output),
    output,
    64,
  )
  expect state.outcome == rule.output_canonical
  state
}

// ## The two routes

const certified_count = 761

/// Fifteen items past its waypoint: the most header reads a certified read
/// pays.
const certified_index = 751

/// How a case opens field 2: the transaction the thread anchored, the
/// opening, the grammar certificate's slot and the L1 reference inputs.
type Route {
  tx_id: ByteArray,
  opening: FieldOpeningV1,
  grammar_ref_input_index: Option<Int>,
  reference_inputs: List<Input>,
}

/// Tier 1 over the small fixture transaction's one output, walked.
fn inline_route() -> Route {
  Route {
    tx_id: block().tx_id,
    opening: fixture.inline_body_opening(
      block().compact_cbor,
      [committed_output()],
    ),
    grammar_ref_input_index: None,
    reference_inputs: [],
  }
}

/// 761 outputs under tier 3, the scripted one at `certified_index`, read
/// through the certificate `certificate(tx_id, preimage)` builds.
fn certified_route(
  certificate: fn(ByteArray, ByteArray) -> FieldGrammarCertificateV1,
) -> Route {
  let preimage = grammar.field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a1", 41),
    certified_count,
    certified_index,
    scripted_output(),
  )
  let committed = fixture.committed_field_preimage_block_fixture_v1(
    block(),
    2,
    preimage,
  )
  Route {
    tx_id: committed.tx_id,
    opening: fixture.certified_body_opening(committed.compact_cbor, preimage),
    grammar_ref_input_index: Some(grammar.grammar_ref_input_index_v1(preimage)),
    reference_inputs: grammar.certified_grammar_reference_inputs_v1(
      committed.tx_id,
      2,
      preimage,
      certificate(committed.tx_id, preimage),
    ),
  }
}

/// The dearest walk a submitter takes: `grammar.walked_item_limit` outputs
/// under tier 3, the scripted one last, walked.
fn walked_limit_route() -> Route {
  let preimage = grammar.walked_limit_preimage_v1(scripted_output())
  let committed = fixture.committed_field_preimage_block_fixture_v1(
    block(),
    2,
    preimage,
  )
  Route {
    tx_id: committed.tx_id,
    opening: fixture.certified_body_opening(committed.compact_cbor, preimage),
    grammar_ref_input_index: None,
    reference_inputs: fixture.certified_reference_inputs_v1(
      committed.tx_id,
      2,
      preimage,
    ),
  }
}

const walked_limit_index = grammar.walked_item_limit - 1

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

fn another_fields_certificate(
  tx_id: ByteArray,
  preimage: ByteArray,
) -> FieldGrammarCertificateV1 {
  grammar.complete_field_grammar_certificate_v1(
    tx_id,
    5,
    preimage,
    certified_count,
  )
}

fn another_transactions_certificate(
  _tx_id: ByteArray,
  preimage: ByteArray,
) -> FieldGrammarCertificateV1 {
  grammar.complete_field_grammar_certificate_v1(
    block().tx_id,
    2,
    preimage,
    certified_count,
  )
}

fn another_preimages_certificate(
  tx_id: ByteArray,
  _preimage: ByteArray,
) -> FieldGrammarCertificateV1 {
  grammar.complete_field_grammar_certificate_v1(
    tx_id,
    2,
    grammar.field_preimage_with_item_v1(
      fixture.repeated_bytes_v1(#"a1", 41),
      certified_count,
      0,
      scripted_output(),
    ),
    certified_count,
  )
}

fn in_progress_certificate(
  tx_id: ByteArray,
  preimage: ByteArray,
) -> FieldGrammarCertificateV1 {
  grammar.in_progress_field_grammar_certificate_v1(
    tx_id,
    2,
    preimage,
    certified_count,
  )
}

// ## Step 02: the descriptor scan opens on the bound output

type Step02Case {
  datum: Option<Step02Datum>,
  redeemer: Step02Redeemer,
  tx: Transaction,
}

/// Step 02 at output `index` by `route`, with the scan opened on `item`.
fn step_02_case(route: Route, index: Int, item: ByteArray) -> Step02Case {
  let bound = bound(route.tx_id, index)
  let next_state: Data = rule.initial_output_scan_v1(bound, item)
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
        opening: route.opening,
        grammar_ref_input_index: route.grammar_ref_input_index,
      },
    ),
    tx: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name(), next_state),
      reference_inputs: route.reference_inputs,
    },
  }
}

fn run_step_02(case: Step02Case) -> Bool {
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

test step_02_walked_route_opens_the_scan_on_the_committed_output() {
  run_step_02(step_02_case(inline_route(), 0, committed_output()))
}

test step_02_walked_route_refuses_an_index_past_the_count() fail {
  run_step_02(step_02_case(inline_route(), 1, committed_output()))
}

test step_02_certified_route_fixture_only_at_the_tier3_maximum() {
  let case = step_02_case(
    certified_route(honest_certificate),
    certified_index,
    scripted_output(),
  )
  case.datum != None
}

test step_02_certified_route_at_the_tier3_maximum_cost() {
  run_step_02(
    step_02_case(
      certified_route(honest_certificate),
      certified_index,
      scripted_output(),
    ),
  )
}

test step_02_certified_route_binds_the_item_it_read() fail {
  run_step_02(
    step_02_case(
      certified_route(honest_certificate),
      certified_index,
      fixture.repeated_bytes_v1(#"a1", 41),
    ),
  )
}

test step_02_certified_route_refuses_an_index_past_the_count() fail {
  run_step_02(
    step_02_case(
      certified_route(honest_certificate),
      certified_count,
      scripted_output(),
    ),
  )
}

test step_02_certified_route_refuses_another_fields_certificate() fail {
  run_step_02(
    step_02_case(
      certified_route(another_fields_certificate),
      certified_index,
      scripted_output(),
    ),
  )
}

test step_02_certified_route_refuses_another_transactions_certificate() fail {
  run_step_02(
    step_02_case(
      certified_route(another_transactions_certificate),
      certified_index,
      scripted_output(),
    ),
  )
}

test step_02_certified_route_refuses_another_preimages_certificate() fail {
  run_step_02(
    step_02_case(
      certified_route(another_preimages_certificate),
      certified_index,
      scripted_output(),
    ),
  )
}

test step_02_certified_route_refuses_an_in_progress_certificate() fail {
  run_step_02(
    step_02_case(
      certified_route(in_progress_certificate),
      certified_index,
      scripted_output(),
    ),
  )
}

test step_02_walked_route_fixture_only_at_the_walked_limit() {
  let case = step_02_case(
    walked_limit_route(),
    walked_limit_index,
    scripted_output(),
  )
  case.datum != None
}

test step_02_walked_route_at_the_walked_limit_cost() {
  run_step_02(
    step_02_case(walked_limit_route(), walked_limit_index, scripted_output()),
  )
}

// ## Step 04: the reference script binds against the bound output

type Step04Case {
  datum: Option<Step04Datum>,
  redeemer: Step04Redeemer,
  tx: Transaction,
}

/// Step 04 at output `index` by `route`, its thread holding the canonical
/// descriptor terminal over `scanned_output` and the field yielding `item`.
fn step_04_case(
  route: Route,
  index: Int,
  scanned_output: ByteArray,
  item: ByteArray,
) -> Step04Case {
  let state = descriptor(route.tx_id, index, scanned_output)
  let next_state: Data = rule.bind_reference_script_v1(
    state,
    item,
    fixture.next_step_script_hash,
    None,
  )
  Step04Case {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    redeemer: ct.Continue(
      Step04Args {
        input_index: 0,
        output_index: 0,
        opening: route.opening,
        record_reference_input_index: -1,
        grammar_ref_input_index: route.grammar_ref_input_index,
      },
    ),
    tx: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name(), next_state),
      reference_inputs: route.reference_inputs,
    },
  }
}

fn run_step_04(case: Step04Case) -> Bool {
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    script_decode_policy_id,
    grammar.test_grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

test step_04_walked_route_binds_the_committed_output() {
  run_step_04(
    step_04_case(inline_route(), 0, committed_output(), committed_output()),
  )
}

test step_04_walked_route_refuses_an_index_past_the_count() fail {
  run_step_04(
    step_04_case(inline_route(), 1, committed_output(), committed_output()),
  )
}

test step_04_certified_route_fixture_only_at_the_tier3_maximum() {
  let case = step_04_case(
    certified_route(honest_certificate),
    certified_index,
    scripted_output(),
    scripted_output(),
  )
  case.datum != None
}

test step_04_certified_route_at_the_tier3_maximum_cost() {
  run_step_04(
    step_04_case(
      certified_route(honest_certificate),
      certified_index,
      scripted_output(),
      scripted_output(),
    ),
  )
}

test step_04_certified_route_binds_the_item_it_read() fail {
  run_step_04(
    step_04_case(
      certified_route(honest_certificate),
      certified_index,
      committed_output(),
      committed_output(),
    ),
  )
}

test step_04_certified_route_refuses_an_index_past_the_count() fail {
  run_step_04(
    step_04_case(
      certified_route(honest_certificate),
      certified_count,
      scripted_output(),
      scripted_output(),
    ),
  )
}

test step_04_certified_route_refuses_another_fields_certificate() fail {
  run_step_04(
    step_04_case(
      certified_route(another_fields_certificate),
      certified_index,
      scripted_output(),
      scripted_output(),
    ),
  )
}

test step_04_certified_route_refuses_an_in_progress_certificate() fail {
  run_step_04(
    step_04_case(
      certified_route(in_progress_certificate),
      certified_index,
      scripted_output(),
      scripted_output(),
    ),
  )
}

test step_04_walked_route_fixture_only_at_the_walked_limit() {
  let case = step_04_case(
    walked_limit_route(),
    walked_limit_index,
    scripted_output(),
    scripted_output(),
  )
  case.datum != None
}

test step_04_walked_route_at_the_walked_limit_cost() {
  run_step_04(
    step_04_case(
      walked_limit_route(),
      walked_limit_index,
      scripted_output(),
      scripted_output(),
    ),
  )
}
