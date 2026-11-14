//// Terminal decision for `receivePurposeLanguage` (arm 40
//// `ReceivePurposePlutusV3Forbidden`, phase 11). The execution of receive
//// purpose (3, i) is forbidden exactly when the purpose exists, its source
//// is PlutusV3, and a redeemer reaches it: the producer checks the language
//// only for an execution that passed the phase-8 redeemer check. The datum
//// and redeemer are the shared claim terminal's.

use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimedPredicateFactV1, FinalStateV1, ImpossiblePurpose, RedeemerAbsent,
  RedeemerPresent, SourceAbsent, SourcePresent,
}
use midgard/fraud_proofs/first_fault_v1.{
  CekFault, CekFaultPositionV1, PurposeCoordinateV1, ReceiveLanguage,
}
use midgard/rejection_reason_v1.{ReceivePurposePlutusV3Forbidden}

/// The receive coordinate a fact is about.
pub fn coordinate_v1(fact: ClaimedPredicateFactV1) -> PurposeCoordinateV1 {
  when fact is {
    ImpossiblePurpose { coordinate } -> coordinate
    SourceAbsent { selection } | SourcePresent { selection } ->
      selection.selection.coordinate
    RedeemerPresent { decision } | RedeemerAbsent { decision } ->
      decision.authenticated.selection.coordinate
    _ -> fail
  }
}

/// Whether the fact establishes the forbidden execution. `SourcePresent` is
/// a non-PlutusV3 source here (the lookup refuses PlutusV3 without a scan),
/// and `RedeemerPresent` / `RedeemerAbsent` decide a PlutusV3 source.
pub fn forbidden_v1(fact: ClaimedPredicateFactV1) -> Bool {
  when fact is {
    RedeemerPresent { .. } -> True
    _ -> False
  }
}

/// A leaf recording this arm at the fact's coordinate convicts when the
/// execution is not forbidden. Any other leaf, accepted ones included,
/// convicts on a forbidden execution its recorded reason does not precede.
pub fn convicts_v1(state: FinalStateV1) -> Bool {
  let bound = state.context.bound.bound
  let coordinate = coordinate_v1(state.claim)
  let forbidden = forbidden_v1(state.claim)
  let names_coordinate = when bound.subject.rejection_reason is {
    Some(ReceivePurposePlutusV3Forbidden { purpose_kind, purpose_index }) ->
      PurposeCoordinateV1 { kind: purpose_kind, index: purpose_index } == coordinate
    _ -> False
  }
  if names_coordinate {
    !forbidden
  } else {
    forbidden && !first_fault_v1.leaf_admits_later_fault_v1(
      bound.subject,
      bound.network_order_context,
      CekFault(CekFaultPositionV1 { coordinate, subcheck: ReceiveLanguage }),
    )
  }
}
