//// Validator-level selectors for `redeemerCanonicity` step 02, which reads
//// the bound redeemer out of field 8 and opens its data scan. It is driven
//// through the applied spend handler by both routes: walking a small field
//// under tier 1, and through the field grammar certificate over a field of
//// 1,724 redeemers under tier 3.

use cardano/transaction.{Input, Transaction}
use fraud_proofs/redeemer_canonicity/step_02 as step_02_validator
use midgard/computation_thread as ct
use midgard/field_grammar_certificate_v1.{FieldGrammarCertificateV1}
use midgard/fraud_proofs/field_grammar_fixture_v1 as grammar
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, WitnessFieldOpening}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/redeemer_canonicity/rule
use midgard/fraud_proofs/redeemer_canonicity/step_02.{
  Args as Step02Args, Datum as Step02Datum, SpendRedeemer as Step02Redeemer,
}
use midgard/native_tx_field_access_v1 as field_access

/// The successor a finished data scan pays to; no case here finishes one.
const step_03_hash = #"7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c"

fn base_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn asset_name() -> ByteArray {
  fixture.ct_token_asset_name(base_block())
}

/// The accused redeemer: a well-formed envelope over one byte of data, so
/// the step hands its data to the data step.
fn accused_redeemer() -> ByteArray {
  #"8400004100820102"
}

fn bound(
  block: fixture.NativeBlockFixtureV1,
  index: Int,
) -> rule.BoundRedeemerV1 {
  rule.BoundRedeemerV1 {
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    witness_set_hash: block.compact.witness_set_hash,
    redeemer_index: index,
  }
}

// ## The two routes

/// 1,724 enveloped redeemers fill field 8 to 32,749 bytes.
const certified_count = 1724

/// Fifteen items past its waypoint: the most header reads a certified read
/// pays.
const certified_index = 1711

/// How a case opens field 8: the block committing it, the opening, the
/// grammar certificate's slot and the L1 reference inputs.
type Route {
  block: fixture.NativeBlockFixtureV1,
  opening: FieldOpeningV1,
  grammar_ref_input_index: Option<Int>,
  reference_inputs: List<Input>,
}

/// Tier 1 over a field 8 holding the accused redeemer alone, walked.
fn inline_route() -> Route {
  let preimage = field_access.encode_field_preimage([accused_redeemer()])
  let block = fixture.committed_field_preimage_block_fixture_v1(
    base_block(),
    8,
    preimage,
  )
  Route {
    block,
    opening: WitnessFieldOpening {
      native_tx_compact_cbor: block.compact_cbor,
      witness_set: block.witness_set_compact,
      carriage: field_access.Inline { preimage },
    },
    grammar_ref_input_index: None,
    reference_inputs: [],
  }
}

/// 1,724 redeemers under tier 3, the accused one at `certified_index`, read
/// through the certificate `certificate(tx_id, preimage)` builds.
fn certified_route(
  certificate: fn(ByteArray, ByteArray) -> FieldGrammarCertificateV1,
) -> Route {
  let preimage = grammar.field_preimage_with_item_v1(
    fixture.repeated_bytes_v1(#"a3", 18),
    certified_count,
    certified_index,
    accused_redeemer(),
  )
  let block = fixture.committed_field_preimage_block_fixture_v1(
    base_block(),
    8,
    preimage,
  )
  Route {
    block,
    opening: fixture.certified_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      preimage,
    ),
    grammar_ref_input_index: Some(grammar.grammar_ref_input_index_v1(preimage)),
    reference_inputs: grammar.certified_grammar_reference_inputs_v1(
      block.tx_id,
      8,
      preimage,
      certificate(block.tx_id, preimage),
    ),
  }
}

/// The dearest walk a submitter takes: `grammar.walked_item_limit` items
/// under tier 3, the accused one last, walked.
fn walked_limit_route() -> Route {
  let preimage = grammar.walked_limit_preimage_v1(accused_redeemer())
  let block = fixture.committed_field_preimage_block_fixture_v1(
    base_block(),
    8,
    preimage,
  )
  Route {
    block,
    opening: fixture.certified_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      preimage,
    ),
    grammar_ref_input_index: None,
    reference_inputs: fixture.certified_reference_inputs_v1(
      block.tx_id,
      8,
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
    8,
    preimage,
    certified_count,
  )
}

// ## Step 02

type Step02Case {
  datum: Option<Step02Datum>,
  redeemer: Step02Redeemer,
  tx: Transaction,
}

/// Step 02 at redeemer `index` by `route`, its data scan opened on `item`.
fn step_02_case(route: Route, index: Int, item: ByteArray) -> Step02Case {
  let bound = bound(route.block, index)
  expect rule.ContinueData { scan } = rule.open_item_v1(bound, item)
  let next_state: Data = scan
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
    step_03_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    grammar.test_grammar_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

test step_02_walked_route_opens_the_committed_redeemer() {
  run_step_02(step_02_case(inline_route(), 0, accused_redeemer()))
}

test step_02_walked_route_refuses_an_index_past_the_count() fail {
  run_step_02(step_02_case(inline_route(), 1, accused_redeemer()))
}

test step_02_certified_route_fixture_only_at_the_tier3_maximum() {
  let case = step_02_case(
    certified_route(honest_certificate),
    certified_index,
    accused_redeemer(),
  )
  case.datum != None
}

test step_02_certified_route_at_the_tier3_maximum_cost() {
  run_step_02(
    step_02_case(
      certified_route(honest_certificate),
      certified_index,
      accused_redeemer(),
    ),
  )
}

test step_02_certified_route_binds_the_item_it_read() fail {
  run_step_02(
    step_02_case(
      certified_route(honest_certificate),
      certified_index,
      #"8400004101820102",
    ),
  )
}

test step_02_certified_route_refuses_an_index_past_the_count() fail {
  run_step_02(
    step_02_case(
      certified_route(honest_certificate),
      certified_count,
      accused_redeemer(),
    ),
  )
}

test step_02_certified_route_refuses_another_fields_certificate() fail {
  run_step_02(
    step_02_case(
      certified_route(
        fn(tx_id, preimage) {
          grammar.complete_field_grammar_certificate_v1(
            tx_id,
            6,
            preimage,
            certified_count,
          )
        },
      ),
      certified_index,
      accused_redeemer(),
    ),
  )
}

test step_02_certified_route_refuses_another_preimages_certificate() fail {
  run_step_02(
    step_02_case(
      certified_route(
        fn(tx_id, _preimage) {
          grammar.complete_field_grammar_certificate_v1(
            tx_id,
            8,
            grammar.field_preimage_with_item_v1(
              fixture.repeated_bytes_v1(#"a3", 18),
              certified_count,
              0,
              accused_redeemer(),
            ),
            certified_count,
          )
        },
      ),
      certified_index,
      accused_redeemer(),
    ),
  )
}

test step_02_certified_route_refuses_an_in_progress_certificate() fail {
  run_step_02(
    step_02_case(
      certified_route(
        fn(tx_id, preimage) {
          grammar.in_progress_field_grammar_certificate_v1(
            tx_id,
            8,
            preimage,
            certified_count,
          )
        },
      ),
      certified_index,
      accused_redeemer(),
    ),
  )
}

test step_02_walked_route_fixture_only_at_the_walked_limit() {
  let case = step_02_case(
    walked_limit_route(),
    walked_limit_index,
    accused_redeemer(),
  )
  case.datum != None
}

test step_02_walked_route_at_the_walked_limit_cost() {
  run_step_02(
    step_02_case(walked_limit_route(), walked_limit_index, accused_redeemer()),
  )
}
