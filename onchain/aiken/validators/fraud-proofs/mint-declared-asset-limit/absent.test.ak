//// Validator-level cases for a `MintDeclaredAssetLimit` rejection whose policy
//// coordinate the mint field does not have. Step 02 closes it with the
//// non-crossing decision, against the directly opened field's count or the
//// certified grammar walk's, and step 04 turns that decision into a
//// contradiction of the rejection only.

use aiken/collection/list
use aiken/primitive/bytearray
use cardano/transaction.{Input, Transaction}
use fraud_proofs/mint_declared_asset_limit/step_02 as step_02_validator
use fraud_proofs/mint_declared_asset_limit/step_04 as step_04_validator
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/mint_declared_asset_limit/rule
use midgard/fraud_proofs/mint_declared_asset_limit/state.{
  AuthenticationStateV1, Bound, Grammar,
}
use midgard/fraud_proofs/mint_declared_asset_limit/step_02.{
  AuthenticateAbsent, Datum as Step02Datum, SpendRedeemer as Step02Redeemer,
}
use midgard/fraud_proofs/mint_declared_asset_limit/step_04.{Args as Step04Args}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1 as field_access
use midgard/native_tx_machine_walk_v1 as walk
use midgard/rejection_reason_v1.{MintDeclaredAssetLimit}

/// The inline route's mint field holds two policies: positions 0 and 1.
const inline_count = 2

/// 900 single-asset policies of 34 bytes each fill the mint field
/// under tier 3.
const certified_count = 900

/// A one-asset policy item: `[policy, {h'': 1}]`.
fn policy_item(seed: Int) -> ByteArray {
  #"82581c"
    |> bytearray.concat(fixture.repeated_bytes_v1(#"00", 26))
    |> bytearray.concat(bytearray.from_int_big_endian(seed, 2))
    |> bytearray.concat(#"a14001")
}

fn policy_items(count: Int) -> List<ByteArray> {
  list.map(list.range(0, count - 1), policy_item)
}

/// How a case opens the mint field: the block committing it, the opening and
/// the L1 reference inputs carrying it.
type Route {
  block: fixture.NativeBlockFixtureV1,
  opening: opening.FieldOpeningV1,
  reference_inputs: List<Input>,
}

fn route_over(
  preimage: ByteArray,
  carriage: fn(ByteArray, ByteArray) -> opening.FieldOpeningV1,
  reference_inputs: fn(ByteArray, ByteArray) -> List<Input>,
) -> Route {
  let block = fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    rule.mint_field_index,
    preimage,
  )
  Route {
    block,
    opening: forced_fixture.from_native_opening(
      carriage(block.compact_cbor, preimage),
    ),
    reference_inputs: reference_inputs(block.tx_id, preimage),
  }
}

/// Tier 1 over a mint field of `inline_count` policies.
fn inline_route() -> Route {
  route_over(
    field_access.encode_field_preimage(policy_items(inline_count)),
    fn(compact_cbor, preimage) {
      opening.BodyFieldOpening {
        native_tx_compact_cbor: compact_cbor,
        carriage: field_access.Inline { preimage },
      }
    },
    fn(_, _) { [] },
  )
}

/// Tier 3 over a mint field of `certified_count` policies.
fn certified_route() -> Route {
  route_over(
    field_access.encode_field_preimage(policy_items(certified_count)),
    fixture.certified_body_opening,
    fn(tx_id, preimage) {
      fixture.certified_reference_inputs_v1(
        tx_id,
        rule.mint_field_index,
        preimage,
      )
    },
  )
}

fn rejected(route: Route, claimed: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: route.block.tx_id,
    source_key: #"01",
    rejection_reason: Some(MintDeclaredAssetLimit { policy_index: claimed }),
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

/// The mint field's grammar, certified whole.
fn completed_grammar(route: Route) {
  let Pair(view, prior) = opening.opened_field_grammar_certification(
    route.opening,
    opening.BodyAnchor {
      tx_id: route.block.tx_id,
      source_kind: substrate.source_kind_forced,
    },
    rule.mint_field_index,
    route.reference_inputs,
    fixture.test_certificate_policy_id,
  )
  let complete = walk.certify_field_grammar(view, prior, certified_count)
  expect walk.field_grammar_is_complete(complete)
  complete
}

type Step02Case {
  datum: Option<Step02Datum>,
  redeemer: Step02Redeemer,
  tx: Transaction,
}

fn step_02_case(
  route: Route,
  state: AuthenticationStateV1,
  checkpoint_bytes: ByteArray,
) -> Step02Case {
  let bound = when state is {
    Bound { bound } -> bound
    Grammar { bound, .. } -> bound
  }
  let decision: Data = rule.absent_policy_decision_v1(bound)
  Step02Case {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    redeemer: ct.Continue(
      AuthenticateAbsent {
        input_index: 0,
        output_index: 0,
        opening: route.opening,
        checkpoint_bytes,
      },
    ),
    tx: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(
        fixture.ct_token_asset_name(route.block),
        decision,
      ),
      reference_inputs: route.reference_inputs,
    },
  }
}

/// Step 02's step-03 successor in these cases: none hands over to it.
const fold_successor_hash =
  #"7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c"

fn run_step_02(case: Step02Case, step_04_hash: ByteArray) -> Bool {
  step_02_validator.main.spend(
    fold_successor_hash,
    step_04_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    case.datum,
    case.redeemer,
    fixture.own_out_ref(),
    case.tx,
  )
}

fn direct_case(
  route: Route,
  subject: substrate.VerdictSubjectV1,
  index: Int,
  checkpoint_bytes: ByteArray,
) -> Step02Case {
  step_02_case(
    route,
    Bound { bound: rule.BoundPolicyV1 { subject, policy_index: index } },
    checkpoint_bytes,
  )
}

fn run_direct(claimed: Int) -> Bool {
  let route = inline_route()
  run_step_02(
    direct_case(route, rejected(route, claimed), claimed, #""),
    fixture.next_step_script_hash,
  )
}

fn certified_case(route: Route, claimed: Int) -> Step02Case {
  let grammar = completed_grammar(route)
  step_02_case(
    route,
    Grammar {
      bound: rule.BoundPolicyV1 {
        subject: rejected(route, claimed),
        policy_index: claimed,
      },
      checkpoint_hash: walk.field_grammar_checkpoint_hash(grammar),
    },
    walk.encode_field_grammar_checkpoint(grammar),
  )
}

test step_02_absent_policy_at_the_count_decides_no_crossing() {
  run_direct(inline_count)
}

test step_02_absent_policy_past_the_count_decides_no_crossing() {
  run_direct(9_223_372_036_854_775_808)
}

test step_02_negative_policy_decides_no_crossing() {
  run_direct(-1)
}

test step_02_absent_policy_refuses_the_last_policy() fail {
  run_direct(inline_count - 1)
}

test step_02_absent_policy_refuses_the_fold_successor() fail {
  let route = inline_route()
  run_step_02(
    direct_case(route, rejected(route, inline_count), inline_count, #""),
    fold_successor_hash,
  )
}

test step_02_absent_policy_refuses_grammar_bytes_without_a_grammar() fail {
  let route = inline_route()
  run_step_02(
    direct_case(route, rejected(route, inline_count), inline_count, #"00"),
    fixture.next_step_script_hash,
  )
}

test step_02_absent_certified_route_decides_no_crossing_at_the_count() {
  run_step_02(
    certified_case(certified_route(), certified_count),
    fixture.next_step_script_hash,
  )
}

test step_02_absent_certified_route_refuses_the_last_policy() fail {
  run_step_02(
    certified_case(certified_route(), certified_count - 1),
    fixture.next_step_script_hash,
  )
}

test step_02_absent_certified_route_fixture_only_at_the_tier3_maximum() {
  let case = certified_case(certified_route(), certified_count)
  case.datum != None
}

test step_02_absent_certified_route_at_the_tier3_maximum_cost() {
  run_step_02(
    certified_case(certified_route(), certified_count),
    fixture.next_step_script_hash,
  )
}

fn run_step_04(route: Route, decision: rule.DecisionStateV1) -> Bool {
  step_04_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(decision),
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
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(route.block)),
  )
}

test step_04_absent_policy_convicts_the_rejection() {
  let route = inline_route()
  run_step_04(
    route,
    rule.absent_policy_decision_v1(
      rule.bind_policy_v1(rejected(route, inline_count), inline_count),
    ),
  )
}

test step_04_absent_policy_refuses_an_acceptance() fail {
  let route = inline_route()
  run_step_04(
    route,
    rule.absent_policy_decision_v1(
      rule.bind_policy_v1(accepted(route), inline_count),
    ),
  )
}
