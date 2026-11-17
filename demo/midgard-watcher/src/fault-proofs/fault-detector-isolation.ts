import { type CompleteCanonicalReplayIsolation } from "@al-ft/midgard-fault-proofs";

import { watcherFaultDetectorFailedAlertSubject } from "../runtime/operations-observability.alert-book.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.watcher-operations-metrics.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import { isDecisionFailureTransient } from "../runtime/watcher-runtime.decision-failure.js";
import { isClassificationMissHandled } from "./fault-decision-bridge.classification-miss.js";
import { perHeaderAlert } from "./per-header-alert.js";

/**
 * Whether a fault detector's failure fails the whole decision pass: one the
 * pass already retries on its timer (`isDecisionFailureTransient`), or one
 * the classification miss handler defers or settles on its own terms
 * (`isClassificationMissHandled`). Each says a dependency did not answer, a
 * replay is held, or a payload is missing, not that the detector cannot
 * replay the block.
 */
export const failsDecisionPass = (error: unknown): boolean =>
  isDecisionFailureTransient(error) || isClassificationMissHandled(error);

/** The operator's `fault_detector_failed` alert, one subject per header. */
export type WatcherFaultDetectorAlert = Readonly<{
  failed: (headerHash: string) => void;
  /**
   * Clears the alert of every header it was raised for that is released or
   * no longer in `observation`'s queue, a rollback included
   * (`perHeaderAlert`). The fault decision bridge calls it on every pass,
   * beside the `proof_family_coverage_gap` alert's reconcile.
   */
  reconcile: ReturnType<typeof perHeaderAlert>["reconcile"];
}>;

/**
 * Raises the `fault_detector_failed` alert of the header a detector's result
 * was withheld on. It stands until the header is decided or leaves the
 * queue: the header's settled outcome clears it
 * (operations-observability.alert-book.ts), and so does `reconcile` once the
 * header is released or a rollback drops it from the observed queue. No
 * replay of another header clears it. The alert is informational: the header
 * is still decided from the other families, and a header nothing else decides
 * still fails its pass as before. A raise of a header already standing is not
 * recorded again.
 */
export const createWatcherFaultDetectorAlert = (
  operationsSink: Pick<WatcherOperationsSink, "setAlert">,
  nowMs: () => number = Date.now,
): WatcherFaultDetectorAlert => {
  const alert = perHeaderAlert({
    code: "fault_detector_failed",
    subjectOf: watcherFaultDetectorFailedAlertSubject,
    operationsSink,
    observedAtMs: () => nowMs().toString(),
  });
  return Object.freeze({
    failed: (headerHash) => alert.set(headerHash, true),
    reconcile: alert.reconcile,
  });
};

/**
 * The watcher's isolation of one failing fault detector from the others in
 * a decision pass. A withheld result is logged at error level with its
 * family, header and error, and raises `alert` when one is given.
 */
export const watcherFaultDetectorIsolation = (
  alert?: WatcherFaultDetectorAlert,
): CompleteCanonicalReplayIsolation =>
  Object.freeze({
    failsPass: failsDecisionPass,
    withheld: ({ launchScope, headerHash, error }) => {
      process.stderr.write(
        `${JSON.stringify({
          packageName: WATCHER_PACKAGE_NAME,
          level: "error",
          reason: "fault_detector_failed",
          family: launchScope.join(","),
          headerHash,
          detail: error instanceof Error ? error.message : String(error),
        })}\n`,
      );
      alert?.failed(headerHash);
    },
  });
