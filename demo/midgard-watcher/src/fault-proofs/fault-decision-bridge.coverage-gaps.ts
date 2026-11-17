import type { HeaderDecision } from "@al-ft/midgard-fault-proofs";

import { watcherProofFamilyCoverageGapAlertSubject } from "../runtime/operations-observability.alert-book.js";
import type { BridgeDependencies } from "./fault-decision-bridge.selected-target.js";
import { perHeaderAlert } from "./per-header-alert.js";

/** Whether a decision names a finding no installed family can prove. */
const coverageGap = (decision: HeaderDecision): boolean =>
  decision.decision === "unprovable" &&
  decision.reason !== "predecessor_context_unavailable";

/**
 * The `proof_family_coverage_gap` alert, one subject per header. A header
 * whose latest decision is unprovable because no installed fault-proof
 * family proves its finding (an unregistered finding, or a category the
 * deployment does not install) raises it; classification goes on with the
 * later headers. Being a non-informational alert, it holds readiness, as an
 * unprovable header must. It clears when a later decision of the header is
 * not such a gap, or when the header merges, is removed, or leaves the
 * observed queue (a rollback included); nothing needs an operator to clear
 * it.
 */
export const coverageGapRecorder = (
  dependencies: Pick<BridgeDependencies, "operationsSink" | "nowMs">,
) => {
  const nowMs = dependencies.nowMs ?? (() => BigInt(Date.now()));
  const alert = perHeaderAlert({
    code: "proof_family_coverage_gap",
    subjectOf: watcherProofFamilyCoverageGapAlertSubject,
    operationsSink: dependencies.operationsSink,
    observedAtMs: () => nowMs().toString(),
  });
  return Object.freeze({
    /** Raises or clears the header's alert from its fresh decision. */
    decided: (headerHash: string, decision: HeaderDecision): void => {
      const gap = coverageGap(decision);
      if (gap !== alert.raised(headerHash)) alert.set(headerHash, gap);
    },
    /** Clears every raised header that is released or no longer queued. */
    reconcile: alert.reconcile,
  });
};
