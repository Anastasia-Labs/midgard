use aiken/cbor
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, FinalStateV1, LeafPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate as adjudicate
use midgard/fraud_proofs/direct_script_derivation/claim_start as start
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{
  DecisionStateV1,
}
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
} as first_fault
use midgard/fraud_proofs/missing_script_source/observe
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn coordinate(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
}

fn source_missing(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.ScriptSourceMissing { purpose_kind: kind, purpose_index: index })
}

fn redeemer_missing(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.RedeemerMissing { purpose_kind: kind, purpose_index: index })
}

fn bound(reason: Option<reasons.RejectionReasonV1>) -> BoundIndexV1 {
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

fn plutus_source() -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 0,
    source_key: #"00",
    language_tag: 3,
    script_hash: hash,
    total_length: 2,
    item_commitment: root,
  }
}

/// A lookup result under the leaf's own binding, as the shared source lookup
/// hands it over.
fn selection(
  reason: Option<reasons.RejectionReasonV1>,
  at: PurposeCoordinateV1,
  source: Option<SourceV1>,
) -> SourceSelectionV1 {
  let index = bound(reason)
  SourceSelectionV1 {
    selection: SelectionV1 {
      bound: index.bound,
      source_index: index.source_index,
      coordinate: at,
      purpose: Some(
        PurposeV1 { coordinate: at, script_hash: hash, subject: hash },
      ),
    },
    source,
  }
}

fn absent_at(
  reason: Option<reasons.RejectionReasonV1>,
  kind: Int,
  index: Int,
) -> SourceSelectionV1 {
  selection(reason, coordinate(kind, index), None)
}

/// The observed route through claim start and adjudication when the claim
/// reopens no other coordinate: no leaf coordinate, or the observed one.
fn adjudicated(authenticated: SourceSelectionV1) -> FinalStateV1 {
  adjudicate.resolve_v1(
    start.complete_v1(observe.observed_context_v1(authenticated)),
  )
}

fn final(
  reason: Option<reasons.RejectionReasonV1>,
  observed: Option<SourceSelectionV1>,
  fact: claim.ClaimedPredicateFactV1,
) -> FinalStateV1 {
  FinalStateV1 {
    context: ClaimContextV1 {
      bound: bound(reason),
      observed: when observed is {
        None -> None
        Some(authenticated) ->
          Some(DecisionStateV1 { authenticated, redeemer_missing: False })
      },
      target: LeafPurpose,
    },
    claim: fact,
  }
}

// ## Observed decision

test observed_missing_source_hands_its_decision_to_claim_start() {
  let authenticated = absent_at(None, 2, 0)
  observe.observed_context_v1(authenticated) == ClaimContextV1 {
    bound: bound(None),
    observed: Some(DecisionStateV1 { authenticated, redeemer_missing: False }),
    target: LeafPurpose,
  }
}

test observed_route_refuses_a_present_source() fail {
  cbor.serialise(
    observe.observed_context_v1(
      selection(None, coordinate(2, 0), Some(plutus_source())),
    ),
  ) != #""
}

test observed_decision_proves_a_source_required_fault() {
  adjudicate.observed_fault_v1(
    DecisionStateV1 {
      authenticated: absent_at(None, 1, 3),
      redeemer_missing: False,
    },
  ) == Some(
    first_fault.PurposeFaultPositionV1 {
      coordinate: coordinate(1, 3),
      subcheck: first_fault.SourceRequired,
    },
  )
}

// ## Accepted and purpose-free rejected leaves (A-N, A-F, R-F later reason)

test accepted_leaf_convicts_on_an_observed_missing_source() {
  adjudicate.convicts_v1(adjudicated(absent_at(None, 0, 0)))
}

test later_phase_reason_convicts_on_an_observed_missing_source() {
  and {
    adjudicate.convicts_v1(
      adjudicated(
        absent_at(Some(reasons.UnusedRedeemer { redeemer_index: 0 }), 3, 1),
      ),
    ),
    adjudicate.convicts_v1(
      adjudicated(
        absent_at(Some(reasons.UnusedScriptWitness { script_index: 0 }), 2, 0),
      ),
    ),
  }
}

test earlier_phase_reason_admits_an_observed_missing_source() {
  and {
    !adjudicate.convicts_v1(
      adjudicated(
        absent_at(
          Some(reasons.InputNotFound { source_kind: 0, input_index: 0 }),
          0,
          0,
        ),
      ),
    ),
    !adjudicate.convicts_v1(
      adjudicated(
        absent_at(Some(reasons.RedeemerMalformed { redeemer_index: 0 }), 0, 0),
      ),
    ),
  }
}

// ## ScriptSourceMissing leaves (R-F)

test claimed_present_source_convicts() {
  adjudicate.convicts_v1(
    final(
      source_missing(2, 4),
      None,
      claim.SourcePresent {
        selection: selection(
          source_missing(2, 4),
          coordinate(2, 4),
          Some(plutus_source()),
        ),
      },
    ),
  )
}

test claimed_impossible_purpose_convicts() {
  adjudicate.convicts_v1(
    final(
      source_missing(9, 4),
      None,
      claim.ImpossiblePurpose { coordinate: coordinate(9, 4) },
    ),
  )
}

test claimed_absent_source_does_not_convict() {
  !adjudicate.convicts_v1(
    final(
      source_missing(2, 4),
      None,
      claim.SourceAbsent { selection: absent_at(source_missing(2, 4), 2, 4) },
    ),
  )
}

test claimed_source_leaf_refuses_a_native_source_fact() fail {
  adjudicate.convicts_v1(
    final(
      source_missing(2, 4),
      None,
      claim.NativeSource { selection: absent_at(source_missing(2, 4), 2, 4) },
    ),
  )
}

test observed_missing_source_at_the_leaf_admits_the_leaf() {
  !adjudicate.convicts_v1(adjudicated(absent_at(source_missing(2, 4), 2, 4)))
}

test observed_missing_source_before_the_leaf_convicts() {
  let leaf = absent_at(source_missing(2, 4), 2, 4)
  and {
    adjudicate.convicts_v1(
      final(
        source_missing(2, 4),
        Some(absent_at(source_missing(2, 4), 2, 3)),
        claim.SourceAbsent { selection: leaf },
      ),
    ),
    adjudicate.convicts_v1(
      final(
        source_missing(2, 4),
        Some(absent_at(source_missing(2, 4), 0, 9)),
        claim.SourceAbsent { selection: leaf },
      ),
    ),
  }
}

test observed_missing_source_after_the_leaf_admits_the_leaf() {
  !adjudicate.convicts_v1(
    final(
      source_missing(2, 4),
      Some(absent_at(source_missing(2, 4), 2, 5)),
      claim.SourceAbsent { selection: absent_at(source_missing(2, 4), 2, 4) },
    ),
  )
}

// ## RedeemerMissing leaves meeting an observed missing source

test observed_missing_source_at_a_redeemer_missing_leaf_convicts() {
  adjudicate.convicts_v1(adjudicated(absent_at(redeemer_missing(1, 2), 1, 2)))
}

test observed_missing_source_before_a_redeemer_missing_leaf_convicts() {
  adjudicate.convicts_v1(
    final(
      redeemer_missing(1, 2),
      Some(absent_at(redeemer_missing(1, 2), 1, 1)),
      claim.RedeemerAbsent {
        decision: DecisionStateV1 {
          authenticated: selection(
            redeemer_missing(1, 2),
            coordinate(1, 2),
            Some(plutus_source()),
          ),
          redeemer_missing: True,
        },
      },
    ),
  )
}
