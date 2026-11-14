import type { HeaderDecision } from "@al-ft/midgard-fault-proofs";

import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherReleasedHeaderProof,
} from "../indexers/authenticated-state-queue-observation.js";
import { watcherProofFamilyCoverageGapAlertSubject } from "../runtime/operations-observability.alert-book.js";
import type { BridgeDependencies } from "./fault-decision-bridge.selected-target.js";

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
  const raised = new Set<string>();
  const set = (headerHash: string, active: boolean): void => {
    dependencies.operationsSink?.setAlert({
      code: "proof_family_coverage_gap",
      subjectDigest: watcherProofFamilyCoverageGapAlertSubject(headerHash),
      active,
      observedAtMs: nowMs().toString(),
    });
    if (active) raised.add(headerHash);
    else raised.delete(headerHash);
  };
  return Object.freeze({
    /** Raises or clears the header's alert from its fresh decision. */
    decided: (headerHash: string, decision: HeaderDecision): void => {
      const gap = coverageGap(decision);
      if (gap !== raised.has(headerHash)) set(headerHash, gap);
    },
    /** Clears every raised header that is released or no longer queued. */
    reconcile: (
      observation: WatcherAuthenticatedStateQueueObservation,
      released: ReadonlyMap<string, WatcherReleasedHeaderProof>,
    ): void => {
      const queued = new Set(
        observation.finalizedHeaders.map(({ headerHash }) => headerHash),
      );
      for (const headerHash of [...raised])
        if (!queued.has(headerHash) || released.has(headerHash))
          set(headerHash, false);
    },
  });
};
