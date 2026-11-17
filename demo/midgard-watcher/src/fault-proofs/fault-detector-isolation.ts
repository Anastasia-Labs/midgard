import {
  type CompleteCanonicalReplayIsolation,
  isFraudProofL1WaitError,
  isReplayLocalFault,
  isRetainedDaPayloadUnavailableError,
  isValidationTraceReplayHeldError,
} from "@al-ft/midgard-fault-proofs";

import { watcherFaultDetectorFailedAlertSubject } from "../runtime/operations-observability.alert-book.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.watcher-operations-metrics.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import { isDecisionFailureTransient } from "../runtime/watcher-runtime.decision-failure.js";
import { isWatcherRetainedDaTransportUnavailable } from "../storage/retained-da-transport-unavailable.js";

/**
 * Whether a fault detector's failure fails the whole decision pass: one the
 * pass already retries on its timer (`isDecisionFailureTransient`), or one
 * the classification miss handler defers or settles on its own terms
 * (`fault-decision-bridge.classification-miss.ts`). Each says a dependency
 * did not answer, a replay is held, or a payload is missing, not that the
 * detector cannot replay the block.
 */
export const failsDecisionPass = (error: unknown): boolean =>
  isDecisionFailureTransient(error) ||
  isFraudProofL1WaitError(error) ||
  isReplayLocalFault(error) ||
  isValidationTraceReplayHeldError(error) ||
  isWatcherRetainedDaTransportUnavailable(error) ||
  isRetainedDaPayloadUnavailableError(error);

/** The operator's `fault_detector_failed` alert, one subject per family. */
export type WatcherFaultDetectorAlert = Readonly<{
  failed: (family: string) => void;
  replayed: (family: string) => void;
}>;

/**
 * Raises a family's `fault_detector_failed` alert when its result is
 * withheld, and clears it when the family next replays a block. The alert is
 * informational: the header is still decided from the other families, and a
 * header nothing else decides still fails its pass as before. Only a change
 * of state is written, so a family failing on every block adds no records.
 */
export const createWatcherFaultDetectorAlert = (
  operationsSink: Pick<WatcherOperationsSink, "setAlert">,
  nowMs: () => number = Date.now,
): WatcherFaultDetectorAlert => {
  const raised = new Set<string>();
  const set = (family: string, active: boolean): void => {
    operationsSink.setAlert({
      code: "fault_detector_failed",
      subjectDigest: watcherFaultDetectorFailedAlertSubject(family),
      active,
      observedAtMs: nowMs().toString(),
    });
    if (active) raised.add(family);
    else raised.delete(family);
  };
  return Object.freeze({
    failed: (family) => {
      if (!raised.has(family)) set(family, true);
    },
    replayed: (family) => {
      if (raised.has(family)) set(family, false);
    },
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
      const family = launchScope.join(",");
      process.stderr.write(
        `${JSON.stringify({
          packageName: WATCHER_PACKAGE_NAME,
          level: "error",
          reason: "fault_detector_failed",
          family,
          headerHash,
          detail: error instanceof Error ? error.message : String(error),
        })}\n`,
      );
      alert?.failed(family);
    },
    replayed: (launchScope) => alert?.replayed(launchScope.join(",")),
  });
