/** Off-chain twin of the shared claim terminal's verdict
 * (`direct-script-derivation/claim-adjudicate.ak`: `observed_fault_v1`,
 * `leaf_admits_observed_v1`, `convicts_v1`). A shape the terminal would fail
 * on throws here. */
import type { RejectionReason } from "../rejection-reason.js";
import type {
  DirectClaimedPredicateFact,
  DirectClaimFinalState,
  DirectRedeemerDecisionState,
} from "./direct-claim.js";
import type { DirectScriptBound } from "./direct-script-derivation.js";
import {
  leafAdmitsFault,
  purposeCoordinateOfReason,
  type PurposeFaultPosition,
  purposePositionPrecedesOrEquals,
  purposeSubcheckOfReason,
  rejectionReasonPhase,
} from "./first-fault.js";

const factArm = (fact: DirectClaimedPredicateFact): string =>
  typeof fact === "string" ? fact : Object.keys(fact)[0]!;

/** The purpose fault an observed decision proves at its own coordinate: a
 * missing source, or a redeemer-bearing source with no redeemer. */
export const directClaimObservedFault = (
  decision: DirectRedeemerDecisionState,
): PurposeFaultPosition | null => {
  const coordinate = decision.authenticated.selection.coordinate;
  if (decision.authenticated.source === null)
    return { coordinate, subcheck: "SourceRequired" };
  return decision.redeemer_missing
    ? { coordinate, subcheck: "RedeemerRequired" }
    : null;
};

/** Whether the leaf's own purpose predicate is refuted by its fact. */
export const directClaimedPredicateIsFalse = (
  reason: RejectionReason,
  fact: DirectClaimedPredicateFact,
): boolean => {
  const arm = factArm(fact);
  switch (purposeSubcheckOfReason(reason)) {
    case "RedeemerRequired":
      if (arm === "RedeemerAbsent") return false;
      if (
        arm === "ImpossiblePurpose" ||
        arm === "SourceAbsent" ||
        arm === "NativeSource" ||
        arm === "RedeemerPresent"
      )
        return true;
      break;
    case "SourceRequired":
      if (arm === "SourceAbsent") return false;
      if (arm === "ImpossiblePurpose" || arm === "SourcePresent") return true;
      break;
  }
  throw new Error(`direct claim: ${arm} is not a fact of the leaf's predicate`);
};

/** Whether the leaf's reason is at or before the observed purpose fault. */
export const directClaimLeafAdmitsObserved = (
  bound: DirectScriptBound,
  observed: PurposeFaultPosition,
  fact: DirectClaimedPredicateFact,
): boolean => {
  const reason = bound.subject.rejection_reason;
  if (reason === null) throw new Error("direct claim: the leaf is accepted");
  const coordinate = purposeCoordinateOfReason(reason);
  if (
    coordinate === null ||
    rejectionReasonPhase(reason, bound.network_order_context) !== 8
  ) {
    if (fact !== "NotRequired")
      throw new Error("direct claim: a purpose-free reason carries a fact");
    return leafAdmitsFault(
      bound.subject,
      bound.network_order_context,
      { phase: "purpose", position: observed },
      "NotRequired",
    );
  }
  const subcheck = purposeSubcheckOfReason(reason);
  const absent =
    subcheck === "SourceRequired" ? "SourceAbsent" : "RedeemerAbsent";
  if (factArm(fact) === absent)
    return purposePositionPrecedesOrEquals({ coordinate, subcheck }, observed);
  return !directClaimedPredicateIsFalse(reason, fact);
};

/** Terminal conviction: an accepted leaf convicts on an observed purpose
 * fault; a rejected leaf convicts when its own predicate is refuted, or when
 * the observed purpose fault precedes its reason. */
export const directClaimConvicts = (final: DirectClaimFinalState): boolean => {
  const { context, claim } = final;
  const bound = context.bound.bound;
  const reason = bound.subject.rejection_reason;
  if (reason === null) {
    if (context.observed === null || claim !== "NotRequired")
      throw new Error("direct claim: an accepted leaf needs an observation");
    return directClaimObservedFault(context.observed) !== null;
  }
  if (context.observed === null)
    return directClaimedPredicateIsFalse(reason, claim);
  const position = directClaimObservedFault(context.observed);
  return (
    position !== null && !directClaimLeafAdmitsObserved(bound, position, claim)
  );
};
