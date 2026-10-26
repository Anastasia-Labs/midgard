use aiken/cbor
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, ClaimedPredicateFactV1, FinalStateV1, ImpossiblePurpose,
  LeafPurpose, NativeSource, NotRequired, RedeemerAbsent, RedeemerOutOfRange,
  RedeemerPresent, SourceAbsent, SourcePresent,
}
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
}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/receive_purpose_language/terminal
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

// ## Synthetic final states

fn at(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
}

fn subject(reason: Option<reasons.RejectionReasonV1>) -> VerdictSubjectV1 {
  when reason is {
    None ->
      VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: root,
        source_key: #"",
        rejection_reason: None,
      }
    Some(_) ->
      VerdictSubjectV1 {
        version: 1,
        direction: 1,
        source_kind: 1,
        transaction_id: root,
        source_key: #"00",
        rejection_reason: reason,
      }
  }
}

fn bound(reason: Option<reasons.RejectionReasonV1>) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: subject(reason),
      compact_cbor: #"",
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: source_index.StateV1 { root, count: 1 },
  }
}

fn source(language: Int) -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 0,
    source_key: #"00",
    language_tag: language,
    script_hash: hash,
    total_length: 2,
    item_commitment: root,
  }
}

fn selection(
  coordinate: PurposeCoordinateV1,
  entry: Option<SourceV1>,
) -> SourceSelectionV1 {
  let b = bound(None)
  SourceSelectionV1 {
    selection: SelectionV1 {
      bound: b.bound,
      source_index: b.source_index,
      coordinate,
      purpose: Some(PurposeV1 { coordinate, script_hash: hash, subject: hash }),
    },
    source: entry,
  }
}

fn present(kind: Int, index: Int) -> ClaimedPredicateFactV1 {
  RedeemerPresent {
    decision: DecisionStateV1 {
      authenticated: selection(at(kind, index), Some(source(3))),
      redeemer_missing: False,
    },
  }
}

fn absent(kind: Int, index: Int) -> ClaimedPredicateFactV1 {
  RedeemerAbsent {
    decision: DecisionStateV1 {
      authenticated: selection(at(kind, index), Some(source(3))),
      redeemer_missing: True,
    },
  }
}

fn other_language(language: Int) -> ClaimedPredicateFactV1 {
  SourcePresent { selection: selection(at(3, 1), Some(source(language))) }
}

fn no_source() -> ClaimedPredicateFactV1 {
  SourceAbsent { selection: selection(at(3, 1), None) }
}

fn final(
  reason: Option<reasons.RejectionReasonV1>,
  fact: ClaimedPredicateFactV1,
) -> FinalStateV1 {
  FinalStateV1 {
    context: ClaimContextV1 {
      bound: bound(reason),
      observed: None,
      target: LeafPurpose,
    },
    claim: fact,
  }
}

fn forbidden(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(
    reasons.ReceivePurposePlutusV3Forbidden {
      purpose_kind: kind,
      purpose_index: index,
    },
  )
}

fn run_failed(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(
    reasons.PlutusExecutionFailed { purpose_kind: kind, purpose_index: index },
  )
}

fn convicts(
  reason: Option<reasons.RejectionReasonV1>,
  fact: ClaimedPredicateFactV1,
) -> Bool {
  terminal.convicts_v1(final(reason, fact))
}

// ## Accepted leaves: A-N convicts on a forbidden execution

test accepted_leaf_convicts_on_a_plutus_v3_receive_with_its_redeemer() {
  convicts(None, present(3, 1))
}

/// A-F shape: a forced admission has no redeemers, so a PlutusV3 receive
/// purpose fails the phase-8 redeemer check first; that fault is not this
/// family's.
test accepted_leaf_is_not_convicted_without_the_redeemer() {
  !convicts(None, absent(3, 1))
}

test accepted_leaf_is_not_convicted_by_another_language() {
  and {
    !convicts(None, other_language(0)),
    !convicts(None, other_language(128)),
  }
}

test accepted_leaf_is_not_convicted_by_a_missing_source() {
  !convicts(None, no_source())
}

test accepted_leaf_is_not_convicted_past_the_last_receive_purpose() {
  !convicts(None, ImpossiblePurpose { coordinate: at(3, 4) })
}

// ## Leaves recording this arm at the fact's coordinate: R-F false claims

test recorded_arm_with_a_present_redeemer_is_established() {
  !convicts(forbidden(3, 1), present(3, 1))
}

test recorded_arm_without_the_redeemer_convicts() {
  convicts(forbidden(3, 1), absent(3, 1))
}

test recorded_arm_on_another_language_convicts() {
  and {
    convicts(forbidden(3, 1), other_language(0)),
    convicts(forbidden(3, 1), other_language(128)),
  }
}

test recorded_arm_on_a_missing_source_convicts() {
  convicts(forbidden(3, 1), no_source())
}

test recorded_arm_past_the_last_receive_purpose_convicts() {
  convicts(forbidden(3, 4), ImpossiblePurpose { coordinate: at(3, 4) })
}

test recorded_arm_at_a_non_receive_coordinate_convicts() {
  and {
    convicts(forbidden(0, 0), ImpossiblePurpose { coordinate: at(0, 0) }),
    convicts(forbidden(3, -1), ImpossiblePurpose { coordinate: at(3, -1) }),
  }
}

// ## Leaves recording another reason: later-reason R-F through phase 11

test recorded_arm_at_a_later_receive_purpose_convicts() {
  convicts(forbidden(3, 2), present(3, 1))
}

test recorded_arm_at_an_earlier_receive_purpose_is_admitted() {
  !convicts(forbidden(3, 0), present(3, 1))
}

test script_run_at_the_same_purpose_follows_the_language_check() {
  convicts(run_failed(3, 1), present(3, 1))
}

test script_run_at_an_earlier_execution_is_admitted() {
  and {
    !convicts(run_failed(0, 0), present(3, 1)),
    !convicts(run_failed(3, 0), present(3, 1)),
  }
}

test script_run_at_a_later_receive_purpose_convicts() {
  convicts(run_failed(3, 2), present(3, 1))
}

test value_phase_reason_convicts() {
  and {
    convicts(Some(reasons.ValueNotPreserved), present(3, 1)),
    convicts(Some(reasons.OutputBelowMinAda { output_index: 0 }), present(3, 1)),
  }
}

test earlier_phase_reasons_are_admitted() {
  and {
    !convicts(Some(reasons.ScriptIntegrityHashMismatch), present(3, 1)),
    !convicts(
      Some(reasons.RedeemerMissing { purpose_kind: 3, purpose_index: 1 }),
      present(3, 1),
    ),
    !convicts(Some(reasons.UnusedRedeemer { redeemer_index: 0 }), present(3, 1)),
  }
}

test another_reason_without_a_forbidden_execution_is_not_convicted() {
  and {
    !convicts(Some(reasons.ValueNotPreserved), absent(3, 1)),
    !convicts(Some(reasons.ValueNotPreserved), other_language(0)),
    !convicts(Some(reasons.ValueNotPreserved), no_source()),
    !convicts(
      Some(reasons.ValueNotPreserved),
      ImpossiblePurpose { coordinate: at(3, 4) },
    ),
  }
}

// ## Facts the receive-language custody never produces

test not_required_fact_refuses() fail {
  cbor.serialise(convicts(None, NotRequired)) != #""
}

test native_source_fact_refuses() fail {
  cbor.serialise(
    convicts(
      None,
      NativeSource { selection: selection(at(3, 1), Some(source(0))) },
    ),
  ) != #""
}

test field_walk_fact_refuses() fail {
  cbor.serialise(convicts(None, RedeemerOutOfRange)) != #""
}
