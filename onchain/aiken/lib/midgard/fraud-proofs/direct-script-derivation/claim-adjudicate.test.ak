use aiken/cbor
use aiken/collection/list
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, FinalStateV1, LeafPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate as adjudicate
use midgard/fraud_proofs/direct_script_derivation/claim_purpose.{
  ClaimPurposeStateV1, ExistingPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/claim_start as start
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{
  DecisionStateV1,
} as rule
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1, phase_of_reason_v1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

// ## Terminal truth table over synthetic final states

fn coordinate(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
}

fn missing(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.RedeemerMissing { purpose_kind: kind, purpose_index: index })
}

fn source_missing(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.ScriptSourceMissing { purpose_kind: kind, purpose_index: index })
}

fn synthetic_bound(reason: Option<reasons.RejectionReasonV1>) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: root,
        source_key: #"",
        rejection_reason: reason,
      },
      compact_cbor: #"",
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: source_index.StateV1 { root, count: 7 },
  }
}

fn synthetic_decision(at: PurposeCoordinateV1, absent: Bool) -> DecisionStateV1 {
  let bound = synthetic_bound(None)
  DecisionStateV1 {
    authenticated: workers.SourceSelectionV1 {
      selection: SelectionV1 {
        bound: bound.bound,
        source_index: bound.source_index,
        coordinate: at,
        purpose: Some(
          PurposeV1 { coordinate: at, script_hash: hash, subject: hash },
        ),
      },
      source: Some(
        SourceV1 {
          field_index: 6,
          item_index: 0,
          source_key: #"00",
          language_tag: 3,
          script_hash: hash,
          total_length: 2,
          item_commitment: root,
        },
      ),
    },
    redeemer_missing: absent,
  }
}

fn final(
  reason: Option<reasons.RejectionReasonV1>,
  observed: Option<DecisionStateV1>,
  fact: claim.ClaimedPredicateFactV1,
) -> FinalStateV1 {
  FinalStateV1 {
    context: ClaimContextV1 {
      bound: synthetic_bound(reason),
      observed,
      target: LeafPurpose,
    },
    claim: fact,
  }
}

fn absent_fact() -> claim.ClaimedPredicateFactV1 {
  claim.RedeemerAbsent { decision: synthetic_decision(coordinate(2, 4), True) }
}

fn present_fact() -> claim.ClaimedPredicateFactV1 {
  claim.RedeemerPresent {
    decision: synthetic_decision(coordinate(2, 4), False),
  }
}

fn selection_fact() -> workers.SourceSelectionV1 {
  synthetic_decision(coordinate(2, 4), True).authenticated
}

fn seen(kind: Int, index: Int) -> Option<DecisionStateV1> {
  Some(synthetic_decision(coordinate(kind, index), True))
}

// Accepted leaves: only the observed route's local decision matters.
test accepted_leaf_convicts_on_an_observed_missing_redeemer() {
  adjudicate.convicts_v1(final(None, seen(1, 0), claim.NotRequired))
}

test accepted_leaf_is_not_convicted_by_an_observed_present_redeemer() {
  !adjudicate.convicts_v1(
    final(
      None,
      Some(synthetic_decision(coordinate(1, 0), False)),
      claim.NotRequired,
    ),
  )
}

test accepted_leaf_needs_the_observed_route() fail {
  adjudicate.convicts_v1(final(None, None, claim.NotRequired))
}

test accepted_leaf_cannot_carry_a_claimed_fact() fail {
  adjudicate.convicts_v1(final(None, seen(1, 0), absent_fact()))
}

// Rejected RedeemerMissing leaves on the claimed-only route: the leaf's own
// predicate decides.
test claimed_impossible_purpose_convicts() {
  adjudicate.convicts_v1(
    final(
      missing(2, 4),
      None,
      claim.ImpossiblePurpose { coordinate: coordinate(2, 4) },
    ),
  )
}

test claimed_source_absent_convicts() {
  adjudicate.convicts_v1(
    final(
      missing(2, 4),
      None,
      claim.SourceAbsent { selection: selection_fact() },
    ),
  )
}

test claimed_native_source_convicts() {
  adjudicate.convicts_v1(
    final(
      missing(2, 4),
      None,
      claim.NativeSource { selection: selection_fact() },
    ),
  )
}

test claimed_present_redeemer_convicts() {
  adjudicate.convicts_v1(final(missing(2, 4), None, present_fact()))
}

test claimed_absent_redeemer_does_not_convict() {
  !adjudicate.convicts_v1(final(missing(2, 4), None, absent_fact()))
}

test claimed_route_refuses_a_not_required_fact() fail {
  adjudicate.convicts_v1(final(missing(2, 4), None, claim.NotRequired))
}

test claimed_route_refuses_a_source_present_fact() fail {
  adjudicate.convicts_v1(
    final(
      missing(2, 4),
      None,
      claim.SourcePresent { selection: selection_fact() },
    ),
  )
}

// Rejected leaves with an observed missing redeemer: the machine's order decides.
test observed_missing_redeemer_after_the_leaf_admits_the_leaf() {
  !adjudicate.convicts_v1(final(missing(2, 4), seen(2, 5), absent_fact()))
}

test observed_missing_redeemer_at_the_leaf_admits_the_leaf() {
  !adjudicate.convicts_v1(final(missing(2, 4), seen(2, 4), absent_fact()))
}

test observed_missing_redeemer_before_the_leaf_convicts() {
  and {
    adjudicate.convicts_v1(final(missing(2, 4), seen(2, 3), absent_fact())),
    adjudicate.convicts_v1(final(missing(2, 4), seen(1, 9), absent_fact())),
    adjudicate.convicts_v1(final(missing(2, 4), seen(0, 0), absent_fact())),
  }
}

test observed_missing_redeemer_with_a_refuted_leaf_convicts() {
  adjudicate.convicts_v1(final(missing(2, 4), seen(2, 9), present_fact()))
}

test observed_present_redeemer_never_convicts_a_rejected_leaf() {
  !adjudicate.convicts_v1(
    final(
      missing(2, 4),
      Some(synthetic_decision(coordinate(0, 0), False)),
      present_fact(),
    ),
  )
}

test earlier_phase_reasons_are_admitted() {
  and {
    !adjudicate.convicts_v1(
      final(Some(reasons.EmptyInputs), seen(0, 0), claim.NotRequired),
    ),
    !adjudicate.convicts_v1(
      final(
        Some(reasons.ValidityIntervalExcludesBlockSlot),
        seen(0, 0),
        claim.NotRequired,
      ),
    ),
  }
}

test later_phase_reasons_convict() {
  and {
    adjudicate.convicts_v1(
      final(
        Some(reasons.ScriptIntegrityHashMismatch),
        seen(0, 0),
        claim.NotRequired,
      ),
    ),
    adjudicate.convicts_v1(
      final(Some(reasons.ValueNotPreserved), seen(0, 0), claim.NotRequired),
    ),
  }
}

test network_mismatch_order_follows_the_network_context() {
  let late = final(
    Some(reasons.NetworkIdMismatch),
    seen(0, 0),
    claim.NotRequired,
  )
  let early = FinalStateV1 {
    ..late,
    context: ClaimContextV1 {
      ..late.context,
      bound: BoundIndexV1 {
        ..late.context.bound,
        bound: BoundV1 {
          ..late.context.bound.bound,
          network_order_context: NetworkOrderContextV1 {
            body_network_mismatch: True,
          },
        },
      },
    },
  }
  and {
    !adjudicate.convicts_v1(late),
    !adjudicate.convicts_v1(early),
  }
}

test unused_witness_and_redeemer_follow_every_purpose() {
  and {
    adjudicate.convicts_v1(
      final(
        Some(reasons.UnusedScriptWitness { script_index: 0 }),
        seen(3, 9),
        claim.NotRequired,
      ),
    ),
    adjudicate.convicts_v1(
      final(
        Some(reasons.UnusedRedeemer { redeemer_index: 0 }),
        seen(3, 9),
        claim.NotRequired,
      ),
    ),
  }
}

test source_missing_order_puts_the_source_check_first() {
  let fact = claim.SourceAbsent { selection: selection_fact() }
  and {
    // Same coordinate: the source check precedes the redeemer check.
    !adjudicate.convicts_v1(final(source_missing(2, 4), seen(2, 4), fact)),
    !adjudicate.convicts_v1(final(source_missing(2, 3), seen(2, 4), fact)),
    adjudicate.convicts_v1(final(source_missing(2, 5), seen(2, 4), fact)),
    adjudicate.convicts_v1(
      final(
        source_missing(2, 4),
        seen(2, 9),
        claim.SourcePresent { selection: selection_fact() },
      ),
    ),
    adjudicate.convicts_v1(
      final(
        source_missing(2, 4),
        seen(2, 9),
        claim.ImpossiblePurpose { coordinate: coordinate(2, 4) },
      ),
    ),
  }
}

fn script_item(kind: Int) -> Option<reasons.RejectionReasonV1> {
  Some(
    reasons.PlutusScriptUndecodable { item_kind: kind, item_index: 0, class: 1 },
  )
}

// An undecodable Plutus item is decoded where its native siblings are: a
// witness script before the purpose scans, an output reference script in the
// same phase but as a whole-transaction check ahead of them.
test undecodable_output_reference_script_precedes_every_purpose() {
  and {
    !adjudicate.convicts_v1(
      final(script_item(1), seen(0, 0), claim.NotRequired),
    ),
    !adjudicate.convicts_v1(
      final(script_item(1), seen(3, 9), claim.NotRequired),
    ),
  }
}

test undecodable_witness_script_precedes_every_purpose() {
  and {
    !adjudicate.convicts_v1(
      final(script_item(0), seen(0, 0), claim.NotRequired),
    ),
    !adjudicate.convicts_v1(
      final(script_item(0), seen(3, 9), claim.NotRequired),
    ),
  }
}

// A witness script item is decoded with the native witness scripts, so it
// takes exactly their phase.
test witness_script_item_shares_the_native_witness_script_phase() {
  let context = NetworkOrderContextV1 { body_network_mismatch: False }
  phase_of_reason_v1(
    reasons.PlutusScriptUndecodable { item_kind: 0, item_index: 0, class: 1 },
    context,
  ) == phase_of_reason_v1(
    reasons.WitnessNativeScriptMalformed { script_index: 0 },
    context,
  )
}

// Either verdict passes the body, so only an abort satisfies these rows.
test undecodable_script_item_of_unknown_kind_refuses() fail {
  let verdict = adjudicate.convicts_v1(
    final(script_item(2), seen(0, 0), claim.NotRequired),
  )
  verdict || !verdict
}

test undecodable_output_reference_script_cannot_carry_a_purpose_fact() fail {
  let verdict = adjudicate.convicts_v1(
    final(script_item(1), seen(0, 0), absent_fact()),
  )
  verdict || !verdict
}

test non_purpose_reason_cannot_carry_a_purpose_fact() fail {
  adjudicate.convicts_v1(
    final(
      Some(reasons.RedeemerMalformed { redeemer_index: 0 }),
      seen(0, 0),
      absent_fact(),
    ),
  )
}

// ## Resolve

fn purpose_state(
  reason: Option<reasons.RejectionReasonV1>,
  observed: Option<DecisionStateV1>,
  fact: claim_purpose.ClaimedPurposeFactV1,
) -> ClaimPurposeStateV1 {
  ClaimPurposeStateV1 {
    context: ClaimContextV1 {
      bound: synthetic_bound(reason),
      observed,
      target: LeafPurpose,
    },
    purpose: fact,
  }
}

fn existing(kind: Int, index: Int) -> claim_purpose.ClaimedPurposeFactV1 {
  ExistingPurpose {
    purpose: PurposeV1 {
      coordinate: coordinate(kind, index),
      script_hash: hash,
      subject: hash,
    },
  }
}

test resolve_reuses_the_observed_decision_for_its_own_purpose() {
  let decision = synthetic_decision(coordinate(2, 4), True)
  let present = synthetic_decision(coordinate(2, 4), False)
  and {
    adjudicate.resolve_v1(
      purpose_state(missing(2, 4), Some(decision), existing(2, 4)),
    ).claim == claim.RedeemerAbsent { decision },
    adjudicate.resolve_v1(
      purpose_state(missing(2, 4), Some(present), existing(2, 4)),
    ).claim == claim.RedeemerPresent { decision: present },
    adjudicate.resolve_v1(
      purpose_state(source_missing(2, 4), Some(decision), existing(2, 4)),
    ).claim == claim.SourcePresent { selection: decision.authenticated },
  }
}

test resolve_passes_receipts_that_need_no_lookup() {
  and {
    adjudicate.resolve_v1(
      purpose_state(None, seen(2, 4), claim_purpose.NotRequired),
    ).claim == claim.NotRequired,
    adjudicate.resolve_v1(
      purpose_state(
        missing(2, 4),
        None,
        claim_purpose.ImpossiblePurpose { coordinate: coordinate(2, 4) },
      ),
    ).claim == claim.ImpossiblePurpose { coordinate: coordinate(2, 4) },
  }
}

test resolve_cannot_reuse_another_purpose_decision() fail {
  cbor.serialise(
    adjudicate.resolve_v1(
      purpose_state(missing(2, 4), seen(2, 5), existing(2, 4)),
    ),
  ) != #""
}

test resolve_cannot_invent_a_decision_on_the_claimed_route() fail {
  cbor.serialise(
    adjudicate.resolve_v1(purpose_state(missing(2, 4), None, existing(2, 4))),
  ) != #""
}

// ## Lookup over an authenticated source map

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

fn mapped(
  reason: Option<reasons.RejectionReasonV1>,
  entry: Option<SourceV1>,
) -> ClaimPurposeStateV1 {
  let index = when entry is {
    None -> source_index.initial_v1()
    Some(candidate) ->
      source_index.admit_v1(
        source_index.initial_v1(),
        candidate,
        source_index.First { proof: [] },
      )
  }
  let bound = synthetic_bound(reason)
  ClaimPurposeStateV1 {
    context: ClaimContextV1 {
      bound: BoundIndexV1 { ..bound, source_index: index },
      observed: None,
      target: LeafPurpose,
    },
    purpose: existing(2, 4),
  }
}

test lookup_proves_an_absent_source() {
  when adjudicate.lookup_v1(mapped(missing(2, 4), None), None, []).claim is {
    claim.SourceAbsent { selection } -> selection.source == None
    _ -> False
  }
}

test lookup_proves_a_native_source() {
  when
    adjudicate.lookup_v1(
      mapped(missing(2, 4), Some(source(0))),
      Some(source(0)),
      [],
    ).claim
  is {
    claim.NativeSource { selection } -> selection.source == Some(source(0))
    _ -> False
  }
}

test lookup_refuses_a_redeemer_bearing_source() fail {
  cbor.serialise(
    adjudicate.lookup_v1(
      mapped(missing(2, 4), Some(source(3))),
      Some(source(3)),
      [],
    ),
  ) != #""
}

test lookup_cannot_hide_a_present_source() fail {
  cbor.serialise(
    adjudicate.lookup_v1(mapped(missing(2, 4), Some(source(0))), None, []),
  ) != #""
}

test lookup_decides_source_presence_for_a_source_leaf() {
  when
    adjudicate.lookup_v1(
      mapped(source_missing(2, 4), Some(source(3))),
      Some(source(3)),
      [],
    ).claim
  is {
    claim.SourcePresent { .. } -> True
    _ -> False
  }
}

// ## Verticals over authenticated transaction fields
//
// One observer purpose (kind 2, index 0) whose script hash is `hash`.

fn block(
  observers: List<ByteArray>,
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> fixture.NativeBlockFixtureV1 {
  let tx = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: observers,
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: witnesses,
      },
    },
  )
}

fn witness(
  purpose: native_types.MidgardRedeemerPurpose,
  index: Int,
) -> native_types.MidgardRedeemerWitness {
  native_types.MidgardRedeemerWitness {
    purpose,
    index,
    redeemer_cbor: #"00",
    execution_units: native_types.MidgardExecutionUnits { memory: 1, steps: 2 },
  }
}

fn real_bound(
  fixture_block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
  entry: Option<SourceV1>,
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
    source_index: when entry is {
      None -> source_index.initial_v1()
      Some(candidate) ->
        source_index.admit_v1(
          source_index.initial_v1(),
          candidate,
          source_index.First { proof: [] },
        )
    },
  }
}

fn redeemer_opening(
  fixture_block: fixture.NativeBlockFixtureV1,
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> FieldOpeningV1 {
  fixture.inline_witness_opening(
    fixture_block.compact_cbor,
    fixture_block.witness_set_compact,
    list.map(witnesses, encode_midgard_redeemer_witness),
  )
}

/// Grammar then pointer walk over field 8, to the claimed decision.
fn claimed_scan(
  initial: claim.ClaimedRedeemerScanV1,
  checkpoint: ByteArray,
  opening: FieldOpeningV1,
) -> FinalStateV1 {
  let args = fn(bytes) {
    workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: bytes,
      budget: 16,
    }
  }
  let certified = adjudicate.claimed_certify_v1(
    initial,
    args(checkpoint),
    [],
    fixture.test_certificate_policy_id,
  )
  let ready = if certified.scan.scan.complete {
    certified
  } else {
    let (_, semantic, _) = cursor.certify_with_completion_at_v1(
      initial.scan.scan.cursor,
      8,
      opening,
      checkpoint,
      16,
      [],
      fixture.test_certificate_policy_id,
    )
    adjudicate.claimed_advance_v1(
      certified,
      args(semantic),
      [],
      fixture.test_certificate_policy_id,
    )
  }
  adjudicate.claimed_decision_v1(ready)
}

/// The claimed-only route from family entry to the terminal's final state.
fn claimed_route(
  observers: List<ByteArray>,
  witnesses: List<native_types.MidgardRedeemerWitness>,
  entry: Option<SourceV1>,
) -> FinalStateV1 {
  let fixture_block = block(observers, witnesses)
  let context = ClaimContextV1 {
    bound: real_bound(fixture_block, missing(2, 0), entry),
    observed: None,
    target: LeafPurpose,
  }
  let purposes = start.observe_v1(
    context,
    fixture.inline_body_opening(fixture_block.compact_cbor, observers),
    [],
    fixture.test_certificate_policy_id,
  )
  when purposes.purpose is {
    ExistingPurpose { .. } ->
      when entry is {
        Some(candidate) ->
          if rule.is_redeemer_bearing_language_v1(candidate.language_tag) {
            let opening = redeemer_opening(fixture_block, witnesses)
            let initial = adjudicate.lookup_scan_v1(
              purposes,
              entry,
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
            claimed_scan(initial, checkpoint, opening)
          } else {
            adjudicate.lookup_v1(purposes, entry, [])
          }
        None -> adjudicate.lookup_v1(purposes, None, [])
      }
    _ -> adjudicate.resolve_v1(purposes)
  }
}

test claimed_route_honest_empty_redeemers_field_is_refused() {
  // C-4: a redeemer-bearing purpose with an empty redeemers field.
  let state = claimed_route([hash], [], Some(source(3)))
  and {
    when state.claim is {
      claim.RedeemerAbsent { decision } -> decision.redeemer_missing
      _ -> False
    },
    !adjudicate.convicts_v1(state),
  }
}

test claimed_route_midgard_language_is_redeemer_bearing() {
  !adjudicate.convicts_v1(claimed_route([hash], [], Some(source(128))))
}

test claimed_route_honest_with_unrelated_redeemers_is_refused() {
  !adjudicate.convicts_v1(
    claimed_route(
      [hash],
      [
        witness(native_types.SpendRedeemer, 0),
        witness(native_types.RewardRedeemer, 1),
      ],
      Some(source(3)),
    ),
  )
}

test claimed_route_false_leaf_with_a_present_redeemer_convicts() {
  let state = claimed_route(
    [hash],
    [witness(native_types.RewardRedeemer, 0)],
    Some(source(3)),
  )
  and {
    when state.claim is {
      claim.RedeemerPresent { .. } -> True
      _ -> False
    },
    adjudicate.convicts_v1(state),
  }
}

test claimed_route_false_leaf_for_an_impossible_purpose_convicts() {
  let state = claimed_route([], [], Some(source(3)))
  and {
    state.claim == claim.ImpossiblePurpose { coordinate: coordinate(2, 0) },
    adjudicate.convicts_v1(state),
  }
}

test claimed_route_false_leaf_for_a_native_source_convicts() {
  adjudicate.convicts_v1(claimed_route([hash], [], Some(source(0))))
}

test claimed_route_false_leaf_for_an_absent_source_convicts() {
  adjudicate.convicts_v1(claimed_route([hash], [], None))
}

test claimed_scan_refuses_a_native_source() fail {
  let fixture_block = block([hash], [])
  let context = ClaimContextV1 {
    bound: real_bound(fixture_block, missing(2, 0), Some(source(0))),
    observed: None,
    target: LeafPurpose,
  }
  cbor.serialise(
    adjudicate.lookup_scan_v1(
      ClaimPurposeStateV1 { context, purpose: existing(2, 0) },
      Some(source(0)),
      [],
      redeemer_opening(fixture_block, []),
      [],
      fixture.test_certificate_policy_id,
    ),
  ) != #""
}

/// The observed route for an accepted leaf, from source selection to the
/// terminal's final state.
fn observed_route(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> FinalStateV1 {
  let fixture_block = block([hash], witnesses)
  let bound = real_bound(fixture_block, None, Some(source(3)))
  let selection = workers.lookup_v1(
    SelectionV1 {
      bound: bound.bound,
      source_index: bound.source_index,
      coordinate: coordinate(2, 0),
      purpose: Some(
        PurposeV1 {
          coordinate: coordinate(2, 0),
          script_hash: hash,
          subject: hash,
        },
      ),
    },
    Some(source(3)),
    [],
  )
  let opening = redeemer_opening(fixture_block, witnesses)
  let Pair(initial, checkpoint) = rule.initial_scan_v1(
    selection,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let decided = claimed_scan(
    claim.ClaimedRedeemerScanV1 {
      context: ClaimContextV1 { bound, observed: None, target: LeafPurpose },
      scan: initial,
    },
    checkpoint,
    opening,
  )
  let decision = when decided.claim is {
    claim.RedeemerAbsent { decision } -> decision
    claim.RedeemerPresent { decision } -> decision
    _ -> fail
  }
  adjudicate.resolve_v1(start.complete_v1(claim.observed_context_v1(decision)))
}

test observed_route_accepted_missing_redeemer_convicts() {
  and {
    adjudicate.convicts_v1(observed_route([])),
    adjudicate.convicts_v1(
      observed_route([witness(native_types.RewardRedeemer, 1)]),
    ),
  }
}

test observed_route_accepted_present_redeemer_is_refused() {
  !adjudicate.convicts_v1(
    observed_route([witness(native_types.RewardRedeemer, 0)]),
  )
}
