//// Validator-level cases for a `WitnessNativeScriptFalse` rejection whose
//// script position field 6 does not have. Step 02 answers it with the
//// absent-script state, against the opened view's count or the certified
//// grammar walk's, and step 03 closes that state as a contradiction of the
//// rejection only.

use cardano/transaction.{Input, Transaction}
use fraud_proofs/native_script_invalid/step_02 as step_02_validator
use fraud_proofs/native_script_invalid/step_03 as step_03_validator
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/field_grammar_fixture_v1 as grammar
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as scripts
use midgard/fraud_proofs/native_script_invalid/rule
use midgard/fraud_proofs/native_script_invalid/step_02
use midgard/fraud_proofs/native_script_invalid/step_03
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1 as field_access
use midgard/native_tx_machine_walk_v1 as walk
use midgard/rejection_reason_v1.{InputNotFound, WitnessNativeScriptFalse}

/// The inline route's field 6 holds two script witnesses: positions 0 and 1.
const script_count = 2

/// 3,637 enveloped witnesses fill field 6 to 32,765 bytes under tier 3.
const certified_count = 3637

/// How a case opens field 6: the block committing it, the opening and the L1
/// reference inputs carrying it.
type Route {
  block: fixture.NativeBlockFixtureV1,
  opening: opening.FieldOpeningV1,
  reference_inputs: List<Input>,
}

fn route_over(
  preimage: ByteArray,
  carriage: fn(ByteArray) -> field_access.FieldCarriageV1,
  reference_inputs: fn(ByteArray, ByteArray) -> List<Input>,
) -> Route {
  let block = fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    opening.script_witnesses_field_index,
    preimage,
  )
  Route {
    block,
    opening: forced_fixture.from_native_opening(
      opening.WitnessFieldOpening {
        native_tx_compact_cbor: block.compact_cbor,
        witness_set: block.witness_set_compact,
        carriage: carriage(preimage),
      },
    ),
    reference_inputs: reference_inputs(block.tx_id, preimage),
  }
}

/// Tier 1 over a field 6 of `script_count` signature scripts.
fn inline_route() -> Route {
  route_over(
    field_access.encode_field_preimage(
      [scripts.signature_script_item_v1(), scripts.signature_script_item_v1()],
    ),
    fn(preimage) { field_access.Inline { preimage } },
    fn(_, _) { [] },
  )
}

/// Tier 3 over `certified_count` witnesses, the last a signature script.
fn certified_route() -> Route {
  route_over(
    grammar.field_preimage_with_item_v1(
      fixture.repeated_bytes_v1(#"a2", 8),
      certified_count,
      certified_count - 1,
      scripts.signature_script_item_v1(),
    ),
    fixture.certified_carriage_v1,
    fn(tx_id, preimage) {
      fixture.certified_reference_inputs_v1(
        tx_id,
        opening.script_witnesses_field_index,
        preimage,
      )
    },
  )
}

fn anchor(route: Route) {
  opening.WitnessAnchor {
    tx_id: route.block.tx_id,
    witness_set_hash: route.block.compact.witness_set_hash,
    source_kind: substrate.source_kind_forced,
  }
}

fn rejected(route: Route, claimed: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: route.block.tx_id,
    source_key: #"01",
    rejection_reason: Some(WitnessNativeScriptFalse { script_index: claimed }),
  }
}

fn accepted(route: Route) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_forced,
    transaction_id: route.block.tx_id,
    source_key: #"01",
    rejection_reason: None,
  }
}

fn absent_state(
  route: Route,
  subject: substrate.VerdictSubjectV1,
) -> step_03.State {
  step_03.State {
    subject,
    bad_tx_id: route.block.tx_id,
    bad_tx_witness_set_hash: route.block.compact.witness_set_hash,
    script_item_hash: rule.absent_script_item_hash,
    validity_interval_start: 0,
    validity_interval_end: 100,
  }
}

/// Field 6's grammar, certified whole by the family's own walk.
fn completed_grammar(route: Route) {
  let Pair(view, prior) = opening.opened_field_grammar_certification(
    route.opening,
    anchor(route),
    opening.script_witnesses_field_index,
    route.reference_inputs,
    fixture.test_certificate_policy_id,
  )
  let complete = walk.certify_field_grammar(view, prior, certified_count)
  expect walk.field_grammar_is_complete(complete)
  complete
}

type Step02Case {
  datum: Option<step_02.Datum>,
  redeemer: step_02.SpendRedeemer,
  tx: Transaction,
}

/// Step 02 closing `subject`'s claimed script `script_index` as absent. A
/// `Some` grammar checkpoint takes the certified route.
fn step_02_case(
  route: Route,
  subject: substrate.VerdictSubjectV1,
  script_index: Int,
  grammar: Option<Pair<ByteArray, ByteArray>>,
  grammar_checkpoint_bytes: ByteArray,
) -> Step02Case {
  let Pair(grammar_checkpoint_hash, certified_bytes) = when grammar is {
    Some(certified) -> certified
    None -> Pair(#"", grammar_checkpoint_bytes)
  }
  let state = step_02.State {
    subject,
    bad_tx_id: route.block.tx_id,
    bad_tx_witness_set_hash: route.block.compact.witness_set_hash,
    validity_interval_start: 0,
    validity_interval_end: 100,
    grammar_checkpoint_hash,
    grammar_complete: grammar != None,
    script_checkpoint_hash: #"",
  }
  let tx = fixture.continue_mid_step_l1_tx_v1(
    fixture.ct_token_asset_name(route.block),
    absent_state(route, subject),
  )
  Step02Case {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    redeemer: ct.Continue(
      step_02.SelectAbsentScript {
        input_index: 0,
        output_index: 0,
        script_index,
        script_tx_wits_opening: route.opening,
        grammar_checkpoint_bytes: certified_bytes,
      },
    ),
    tx: Transaction { ..tx, reference_inputs: route.reference_inputs },
  }
}

fn run_step_02(case: Step02Case) -> Bool {
  step_02_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

/// Before grammar certification: the opened view's count bounds the field.
fn run_uncertified(claimed: Int, script_index: Int) -> Bool {
  let route = inline_route()
  run_step_02(
    step_02_case(route, rejected(route, claimed), script_index, None, #""),
  )
}

/// After grammar certification: the certified walk's count bounds the field.
fn certified_case(route: Route, claimed: Int) -> Step02Case {
  let grammar = completed_grammar(route)
  step_02_case(
    route,
    rejected(route, claimed),
    claimed,
    Some(
      Pair(
        walk.field_grammar_checkpoint_hash(grammar),
        walk.encode_field_grammar_checkpoint(grammar),
      ),
    ),
    #"",
  )
}

test step_02_absent_script_at_the_count_writes_the_absent_state() {
  run_uncertified(script_count, script_count)
}

test step_02_absent_script_past_the_count_writes_the_absent_state() {
  run_uncertified(9_223_372_036_854_775_808, 9_223_372_036_854_775_808)
}

test step_02_negative_script_position_writes_the_absent_state() {
  run_uncertified(-1, -1)
}

test step_02_absent_script_refuses_the_last_script_position() fail {
  run_uncertified(script_count - 1, script_count - 1)
}

test step_02_absent_script_refuses_the_first_script_position() fail {
  run_uncertified(0, 0)
}

test step_02_absent_script_binds_the_reason_position() fail {
  run_uncertified(script_count, script_count + 1)
}

test step_02_absent_script_refuses_another_reason() fail {
  let route = inline_route()
  let subject = substrate.VerdictSubjectV1 {
    ..rejected(route, script_count),
    rejection_reason: Some(InputNotFound { source_kind: 0, input_index: 2 }),
  }
  run_step_02(step_02_case(route, subject, script_count, None, #""))
}

test step_02_absent_script_refuses_a_negative_acceptance_position() fail {
  let route = inline_route()
  run_step_02(step_02_case(route, accepted(route), -1, None, #""))
}

test step_02_absent_script_refuses_grammar_bytes_before_certification() fail {
  let route = inline_route()
  run_step_02(
    step_02_case(
      route,
      rejected(route, script_count),
      script_count,
      None,
      #"00",
    ),
  )
}

test step_02_certified_absent_script_at_the_count_writes_the_absent_state() {
  run_step_02(certified_case(inline_route(), script_count))
}

test step_02_certified_absent_script_refuses_the_last_script_position() fail {
  run_step_02(certified_case(inline_route(), script_count - 1))
}

test step_02_absent_certified_route_closes_a_script_at_the_tier3_count() {
  run_step_02(certified_case(certified_route(), certified_count))
}

test step_02_absent_certified_route_refuses_the_last_tier3_script() fail {
  run_step_02(certified_case(certified_route(), certified_count - 1))
}

test step_02_absent_certified_route_fixture_only_at_the_tier3_maximum() {
  let case = certified_case(certified_route(), certified_count)
  case.datum != None
}

test step_02_absent_certified_route_at_the_tier3_maximum_cost() {
  run_step_02(certified_case(certified_route(), certified_count))
}

fn run_step_03(route: Route, state: step_03.State) -> Bool {
  step_03_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      step_03.AbsentFinalize {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(route.block)),
  )
}

test step_03_absent_script_convicts_the_rejection() {
  let route = inline_route()
  run_step_03(route, absent_state(route, rejected(route, script_count)))
}

test step_03_absent_script_refuses_an_acceptance() fail {
  let route = inline_route()
  run_step_03(route, absent_state(route, accepted(route)))
}

test step_03_absent_finalize_refuses_a_selected_script() fail {
  let route = inline_route()
  run_step_03(
    route,
    step_03.State {
      ..absent_state(route, rejected(route, script_count)),
      script_item_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    },
  )
}
