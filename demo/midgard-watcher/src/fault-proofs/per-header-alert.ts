import type { WatcherReleasedHeaderProof } from "../indexers/authenticated-state-queue-observation.js";
import type {
  WatcherAlertCode,
  WatcherOperationsSink,
} from "../runtime/operations-observability.watcher-operations-metrics.js";

/**
 * An alert kept one subject per state-queue header, with the headers this
 * process raised it for. `reconcile` clears it for every such header that is
 * released (merged or removed) or not in the set of headers it is given, a
 * rollback included, so no header that left the queue keeps it until a
 * restart. Every per-header alert is raised through one, and the fault
 * decision bridge reconciles each with the headers of every observation
 * (`perHeaderAlerts`); an alert whose headers outlive that queue widens the
 * set itself (`bindWatcherRetainedDaOperations`).
 */
export const perHeaderAlert = (input: {
  readonly code: WatcherAlertCode;
  readonly subjectOf: (headerHash: string) => string;
  readonly operationsSink: Pick<WatcherOperationsSink, "setAlert"> | undefined;
  readonly observedAtMs: () => string;
}) => {
  const raised = new Set<string>();
  const set = (headerHash: string, active: boolean): void => {
    input.operationsSink?.setAlert({
      code: input.code,
      subjectDigest: input.subjectOf(headerHash),
      active,
      observedAtMs: input.observedAtMs(),
    });
    if (active) raised.add(headerHash);
    else raised.delete(headerHash);
  };
  return Object.freeze({
    set,
    raised: (headerHash: string): boolean => raised.has(headerHash),
    /** Clears every raised header that is released or not in `queued`. */
    reconcile: (
      queued: ReadonlySet<string>,
      released: ReadonlyMap<string, WatcherReleasedHeaderProof>,
    ): void => {
      for (const headerHash of [...raised])
        if (!queued.has(headerHash) || released.has(headerHash))
          set(headerHash, false);
    },
  });
};

export type PerHeaderAlert = ReturnType<typeof perHeaderAlert>;
