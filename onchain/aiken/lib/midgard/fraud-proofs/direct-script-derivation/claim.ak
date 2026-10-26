//// Claim custody shared by the direct purpose families. Every claim-side
//// state carries the exact source-finish binding, the purpose coordinate it
//// reopens and, when an observed route ran, its complete decision.
//// Constructor and field order below is wire format.

use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{
  DecisionStateV1, ScanStateV1,
}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}

/// Which purpose coordinate the claim reopens: 0 the one the leaf's own
/// reason names, 1 the pointer of an authenticated field-8 redeemer.
pub type ClaimTargetV1 {
  LeafPurpose
  RedeemerPointer { redeemer_index: Int, coordinate: PurposeCoordinateV1 }
}

/// Constr 0 [bound, observed, target]. `observed` is None only on a
/// claimed-only entry; otherwise it is the observed route's complete decision.
pub type ClaimContextV1 {
  bound: BoundIndexV1,
  observed: Option<DecisionStateV1>,
  target: ClaimTargetV1,
}

/// The authenticated predicate fact for the leaf's claimed reason:
/// 0 NotRequired, 1 ImpossiblePurpose, 2 SourceAbsent, 3 NativeSource,
/// 4 SourcePresent, 5 RedeemerPresent, 6 RedeemerAbsent.
pub type ClaimedPredicateFactV1 {
  NotRequired
  ImpossiblePurpose { coordinate: PurposeCoordinateV1 }
  SourceAbsent { selection: SourceSelectionV1 }
  NativeSource { selection: SourceSelectionV1 }
  SourcePresent { selection: SourceSelectionV1 }
  RedeemerPresent { decision: DecisionStateV1 }
  RedeemerAbsent { decision: DecisionStateV1 }
}

/// Constr 0 [context, claim]. The terminal's only input.
pub type FinalStateV1 {
  context: ClaimContextV1,
  claim: ClaimedPredicateFactV1,
}

/// Constr 0 [context, scan]. The claimed field-8 pointer scan.
pub type ClaimedRedeemerScanV1 {
  context: ClaimContextV1,
  scan: ScanStateV1,
}

/// The source-finish binding a selection was derived from.
pub fn bound_index_v1(selection: SourceSelectionV1) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: selection.selection.bound,
    source_index: selection.selection.source_index,
  }
}

/// The observed route's hand-off: the decision and the exact binding it
/// carries. No caller supplies either part.
pub fn observed_context_v1(decision: DecisionStateV1) -> ClaimContextV1 {
  ClaimContextV1 {
    bound: bound_index_v1(decision.authenticated),
    observed: Some(decision),
    target: LeafPurpose,
  }
}
