use aiken/collection/list
use cardano/transaction.{Transaction}
use fraud_proofs/missing_redeemer/claim_adjudicate as adjudicate_entry
use fraud_proofs/missing_redeemer/claimed_redeemer_grammar as grammar_entry
use fraud_proofs/missing_redeemer/claimed_redeemer_scan as scan_entry
use fraud_proofs/missing_redeemer/family_entry
use fraud_proofs/missing_redeemer/step_05 as terminal_entry
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/missing_redeemer/adjudicate
use midgard/fraud_proofs/missing_redeemer/claim.{
  ClaimContextV1, ClaimedRedeemerScanV1, FinalStateV1,
}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{ClaimPurposeStateV1}
use midgard/fraud_proofs/missing_redeemer/claim_start as start
use midgard/fraud_proofs/missing_redeemer/entry
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/missing_redeemer/step_05
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

const other = #"55555555555555555555555555555555555555555555555555555555"

fn missing(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.RedeemerMissing { purpose_kind: kind, purpose_index: index })
}

fn source(language: Int) -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 2,
    source_key: #"02",
    language_tag: language,
    script_hash: hash,
    total_length: 8,
    item_commitment: root,
  }
}

fn block(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> fixture.NativeBlockFixtureV1 {
  let tx = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: [hash],
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: witnesses,
      },
    },
  )
}

fn bound(
  fixture_block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: fixture_block.tx_id,
        source_key: #"",
        rejection_reason: reason,
      },
      compact_cbor: fixture_block.compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: source_index.admit_v1(
      source_index.initial_v1(),
      source(3),
      source_index.First { proof: [] },
    ),
  }
}

fn witness_opening(
  fixture_block: fixture.NativeBlockFixtureV1,
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> FieldOpeningV1 {
  fixture.inline_witness_opening(
    fixture_block.compact_cbor,
    fixture_block.witness_set_compact,
    list.map(witnesses, encode_midgard_redeemer_witness),
  )
}

fn reward(index: Int) -> native_types.MidgardRedeemerWitness {
  native_types.MidgardRedeemerWitness {
    purpose: native_types.RewardRedeemer,
    index,
    redeemer_cbor: #"00",
    execution_units: native_types.MidgardExecutionUnits { memory: 1, steps: 2 },
  }
}

fn step_datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

fn step_tx(state: Data, signatures: List<ByteArray>, same: Bool) -> Transaction {
  let tx = if same {
    fixture.continue_same_step_l1_tx_v1(root, state)
  } else {
    fixture.continue_mid_step_l1_tx_v1(root, state)
  }
  Transaction { ..tx, extra_signatories: signatures }
}

// ## family-entry

fn entry_run(
  reason: Option<reasons.RejectionReasonV1>,
  claim_route: Bool,
  output: fn(BoundIndexV1) -> Data,
  signatures: List<ByteArray>,
) -> Bool {
  let state = bound(block([]), reason)
  let action = if claim_route {
    entry.Claim { input_index: 0, output_index: 0 }
  } else {
    entry.Observe { input_index: 0, output_index: 0 }
  }
  family_entry.main.spend(
    if claim_route {
      other
    } else {
      fixture.next_step_script_hash
    },
    if claim_route {
      fixture.next_step_script_hash
    } else {
      other
    },
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(action),
    fixture.own_out_ref(),
    step_tx(output(state), signatures, False),
  )
}

fn unchanged(state: BoundIndexV1) -> Data {
  let output: Data = state
  output
}

fn claimed_only(state: BoundIndexV1) -> Data {
  let output: Data = ClaimContextV1 { bound: state, observed: None }
  output
}

test entry_observe_hands_the_exact_binding_to_purpose_selection() {
  entry_run(None, False, unchanged, [fixture.fraud_prover_key_hash])
}

test entry_observe_cannot_alter_the_binding() {
  !entry_run(
    None,
    False,
    fn(state) {
      let changed: Data = BoundIndexV1 {
        ..state,
        source_index: source_index.initial_v1(),
      }
      changed
    },
    [fixture.fraud_prover_key_hash],
  )
}

test entry_claim_starts_a_claimed_only_context() {
  entry_run(missing(2, 0), True, claimed_only, [fixture.fraud_prover_key_hash])
}

test entry_claim_cannot_carry_an_invented_observation() {
  !entry_run(
    missing(2, 0),
    True,
    fn(state) {
      let changed: Data = ClaimContextV1 {
        bound: state,
        observed: Some(
          rule.DecisionStateV1 {
            authenticated: workers.SourceSelectionV1 {
              selection: SelectionV1 {
                bound: state.bound,
                source_index: state.source_index,
                coordinate: PurposeCoordinateV1 { kind: 2, index: 0 },
                purpose: None,
              },
              source: None,
            },
            redeemer_missing: True,
          },
        ),
      }
      changed
    },
    [fixture.fraud_prover_key_hash],
  )
}

test entry_claim_refuses_an_accepted_leaf() fail {
  entry_run(None, True, claimed_only, [fixture.fraud_prover_key_hash])
}

test entry_claim_refuses_another_rejection_reason() fail {
  entry_run(
    Some(reasons.ScriptSourceMissing { purpose_kind: 2, purpose_index: 0 }),
    True,
    claimed_only,
    [fixture.fraud_prover_key_hash],
  )
}

test entry_route_choice_requires_the_prover_signature() fail {
  entry_run(None, False, unchanged, [])
}

test entry_route_choice_refuses_another_signature() fail {
  entry_run(missing(2, 0), True, claimed_only, [other])
}

// ## claim-adjudicate

fn claimed_context(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> ClaimContextV1 {
  ClaimContextV1 {
    bound: bound(block(witnesses), missing(2, 0)),
    observed: None,
  }
}

fn reopened(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> ClaimPurposeStateV1 {
  let context = claimed_context(witnesses)
  start.observe_v1(
    context,
    fixture.inline_body_opening(context.bound.bound.compact_cbor, [hash]),
    [],
    fixture.test_certificate_policy_id,
  )
}

fn adjudicate_run(
  state: ClaimPurposeStateV1,
  action: adjudicate.ActionV1,
  output: Data,
) -> Bool {
  adjudicate_entry.main.spend(
    fixture.next_step_script_hash,
    other,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(state),
    ct.Continue(action),
    fixture.own_out_ref(),
    step_tx(output, [fixture.fraud_prover_key_hash], False),
  )
}

test adjudicate_resolve_is_exact() {
  let state = ClaimPurposeStateV1 {
    ..reopened([]),
    purpose: claim_purpose.ImpossiblePurpose {
      coordinate: PurposeCoordinateV1 { kind: 2, index: 0 },
    },
  }
  adjudicate_run(
    state,
    adjudicate.Resolve { input_index: 0, output_index: 0 },
    adjudicate.resolve_v1(state),
  )
}

test adjudicate_resolve_cannot_alter_the_fact() {
  let state = ClaimPurposeStateV1 {
    ..reopened([]),
    purpose: claim_purpose.ImpossiblePurpose {
      coordinate: PurposeCoordinateV1 { kind: 2, index: 0 },
    },
  }
  let expected = adjudicate.resolve_v1(state)
  !adjudicate_run(
    state,
    adjudicate.Resolve { input_index: 0, output_index: 0 },
    FinalStateV1 { ..expected, claim: claim.NotRequired },
  )
}

test adjudicate_lookup_scan_hands_the_scan_to_the_claimed_grammar() {
  let state = reopened([])
  let opening = witness_opening(block([]), [])
  let action = adjudicate.LookupScan {
    input_index: 0,
    output_index: 0,
    source: Some(source(3)),
    proof: [],
    opening,
  }
  let expected: Data = adjudicate.lookup_scan_v1(
    state,
    Some(source(3)),
    [],
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  and {
    adjudicate_entry.main.spend(
      other,
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      step_datum(state),
      ct.Continue(action),
      fixture.own_out_ref(),
      step_tx(expected, [fixture.fraud_prover_key_hash], False),
    ),
    !adjudicate_run(state, action, expected),
  }
}

test adjudicate_lookup_cannot_finish_a_redeemer_bearing_source() fail {
  let state = reopened([])
  adjudicate_run(
    state,
    adjudicate.Lookup {
      input_index: 0,
      output_index: 0,
      source: Some(source(3)),
      proof: [],
    },
    claim.NotRequired,
  )
}

// ## claimed field-8 grammar and scan

fn scan_initial(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> (ClaimedRedeemerScanV1, ByteArray, FieldOpeningV1) {
  let state = reopened(witnesses)
  let opening = witness_opening(block(witnesses), witnesses)
  let initial = adjudicate.lookup_scan_v1(
    state,
    Some(source(3)),
    [],
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = rule.initial_scan_v1(
    initial.scan.authenticated,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  (initial, checkpoint, opening)
}

fn progress(opening: FieldOpeningV1, bytes: ByteArray, budget: Int) {
  workers.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: bytes,
    budget,
  }
}

fn grammar_run(
  witnesses: List<native_types.MidgardRedeemerWitness>,
  budget: Int,
  same: Bool,
) -> Bool {
  let (initial, checkpoint, opening) = scan_initial(witnesses)
  let args = progress(opening, checkpoint, budget)
  let expected: Data = adjudicate.claimed_certify_v1(
    initial,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  grammar_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(initial),
    ct.Continue(args),
    fixture.own_out_ref(),
    step_tx(expected, [fixture.fraud_prover_key_hash], same),
  )
}

test claimed_grammar_completion_hands_off_to_the_scan() {
  and {
    grammar_run([], 16, False),
    !grammar_run([], 16, True),
  }
}

test claimed_grammar_partial_batch_stays_in_place() {
  let witnesses = [reward(1), reward(2), reward(3)]
  and {
    grammar_run(witnesses, 1, True),
    !grammar_run(witnesses, 1, False),
  }
}

fn certified(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> ClaimedRedeemerScanV1 {
  let (initial, checkpoint, opening) = scan_initial(witnesses)
  adjudicate.claimed_certify_v1(
    initial,
    progress(opening, checkpoint, 16),
    [],
    fixture.test_certificate_policy_id,
  )
}

fn scan_finish(state: ClaimedRedeemerScanV1, output: Data) -> Bool {
  scan_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(state),
    ct.Continue(adjudicate.Finish { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    step_tx(output, [fixture.fraud_prover_key_hash], False),
  )
}

test claimed_scan_finish_hands_the_decision_to_the_terminal() {
  let state = certified([])
  scan_finish(state, adjudicate.claimed_decision_v1(state))
}

test claimed_scan_finish_cannot_flip_the_decision() {
  let state = certified([])
  let expected = adjudicate.claimed_decision_v1(state)
  expect claim.RedeemerAbsent { decision } = expected.claim
  !scan_finish(
    state,
    FinalStateV1 {
      ..expected,
      claim: claim.RedeemerPresent {
        decision: rule.DecisionStateV1 { ..decision, redeemer_missing: False },
      },
    },
  )
}

test claimed_scan_walk_finds_the_present_redeemer() {
  let witnesses = [reward(1), reward(0)]
  let (initial, checkpoint, opening) = scan_initial(witnesses)
  let ready = adjudicate.claimed_certify_v1(
    initial,
    progress(opening, checkpoint, 16),
    [],
    fixture.test_certificate_policy_id,
  )
  let (_, semantic, _) = cursor.certify_with_completion_at_v1(
    initial.scan.scan.cursor,
    8,
    opening,
    checkpoint,
    16,
    [],
    fixture.test_certificate_policy_id,
  )
  let args = progress(opening, semantic, 16)
  let walked = adjudicate.claimed_advance_v1(
    ready,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  and {
    scan_entry.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      step_datum(ready),
      ct.Continue(adjudicate.Scan { args }),
      fixture.own_out_ref(),
      step_tx(walked, [fixture.fraud_prover_key_hash], True),
    ),
    scan_finish(walked, adjudicate.claimed_decision_v1(walked)),
    when adjudicate.claimed_decision_v1(walked).claim is {
      claim.RedeemerPresent { .. } -> True
      _ -> False
    },
  }
}

test claimed_scan_cannot_finish_before_exhaustion() fail {
  let witnesses = [reward(1), reward(0)]
  let state = certified(witnesses)
  scan_finish(state, adjudicate.claimed_decision_v1(state))
}

// ## Terminal

fn terminal(state: FinalStateV1) -> Bool {
  terminal_entry.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(
      step_05.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(root),
  )
}

test terminal_convicts_a_refuted_rejection() {
  let state = certified([reward(0)])
  // A present redeemer: grammar alone does not exhaust a nonempty field, so
  // walk it to completion first.
  let (initial, checkpoint, opening) = scan_initial([reward(0)])
  let (_, semantic, _) = cursor.certify_with_completion_at_v1(
    initial.scan.scan.cursor,
    8,
    opening,
    checkpoint,
    16,
    [],
    fixture.test_certificate_policy_id,
  )
  let walked = adjudicate.claimed_advance_v1(
    state,
    progress(opening, semantic, 16),
    [],
    fixture.test_certificate_policy_id,
  )
  terminal(adjudicate.claimed_decision_v1(walked))
}

test terminal_convicts_an_impossible_purpose() {
  let context = claimed_context([])
  terminal(
    FinalStateV1 {
      context,
      claim: claim.ImpossiblePurpose {
        coordinate: PurposeCoordinateV1 { kind: 2, index: 0 },
      },
    },
  )
}

test terminal_refuses_an_honest_rejection() {
  !terminal(adjudicate.claimed_decision_v1(certified([])))
}

test terminal_convicts_an_accepted_missing_redeemer() {
  let decided = adjudicate.claimed_decision_v1(certified([]))
  expect claim.RedeemerAbsent { decision } = decided.claim
  let accepted = rule.DecisionStateV1 {
    ..decision,
    authenticated: workers.SourceSelectionV1 {
      ..decision.authenticated,
      selection: SelectionV1 {
        ..decision.authenticated.selection,
        bound: BoundV1 {
          ..decision.authenticated.selection.bound,
          subject: VerdictSubjectV1 {
            ..decision.authenticated.selection.bound.subject,
            rejection_reason: None,
          },
        },
      },
    },
  }
  terminal(
    adjudicate.resolve_v1(
      start.complete_v1(claim.observed_context_v1(accepted)),
    ),
  )
}

test terminal_refuses_an_accepted_present_redeemer() {
  let decided = adjudicate.claimed_decision_v1(certified([]))
  expect claim.RedeemerAbsent { decision } = decided.claim
  let accepted = rule.DecisionStateV1 {
    redeemer_missing: False,
    authenticated: workers.SourceSelectionV1 {
      ..decision.authenticated,
      selection: SelectionV1 {
        ..decision.authenticated.selection,
        bound: BoundV1 {
          ..decision.authenticated.selection.bound,
          subject: VerdictSubjectV1 {
            ..decision.authenticated.selection.bound.subject,
            rejection_reason: None,
          },
        },
      },
    },
  }
  !terminal(
    adjudicate.resolve_v1(
      start.complete_v1(claim.observed_context_v1(accepted)),
    ),
  )
}
