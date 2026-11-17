import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherReleasedHeaderProof,
} from "../indexers/authenticated-state-queue-observation.js";
import type {
  WatcherAlertCode,
  WatcherOperationsSink,
} from "../runtime/operations-observability.watcher-operations-metrics.js";

/**
 * An alert kept one subject per state-queue header, with the headers this
 * process raised it for. `reconcile` clears it for every such header that is
 * released (merged or removed) or no longer in the observed queue, a rollback
 * included, so no header that left the queue keeps it until a restart.
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
