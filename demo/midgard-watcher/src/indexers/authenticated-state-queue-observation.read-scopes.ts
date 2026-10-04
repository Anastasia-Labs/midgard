import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import { createWatcherLocalKupmiosRawSource } from "../l1/local-kupmios-raw-source.js";
import type { WatcherConfig } from "../runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { WatcherStartupStageHeld } from "../runtime/startup-progress.js";

export class WatcherStateQueueReadRetired extends WatcherStartupStageHeld {
  constructor(cause?: unknown) {
    super("state-queue read awaits a current bounded source attempt");
    this.cause = cause;
  }
}

/** Volatile request lifetime only; signed recovery authority is independent. */
export const createWatcherStateQueueReadScopes = (
  input: Readonly<{
    watcherConfig: WatcherConfig;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  }>,
) => {
  let generation = 0;
  let closed = false;
  const active = new Set<ReturnType<typeof createDaAvailabilityReadScope>>();
  const invalidate = () => {
    generation += 1;
    for (const scope of active) scope.close();
    active.clear();
  };
  return Object.freeze({
    invalidate,
    close: () => {
      closed = true;
      invalidate();
    },
    begin: (observationDepth: "inclusion" | "release_finality") => {
      if (closed) throw new Error("state-queue read scopes are closed");
      const expected = generation;
      // Allocation precedes queue waiting. Retries never renew this budget.
      const scope = createDaAvailabilityReadScope({
        attemptTimeoutMs: input.watcherConfig.l1.requestTimeoutMs,
      });
      active.add(scope);
      const close = () => {
        active.delete(scope);
        scope.close();
      };
      const assertCurrent = () => {
        if (closed || expected !== generation)
          throw new WatcherStateQueueReadRetired();
        scope.assertCurrent();
      };
      try {
        const rawSource = createWatcherLocalKupmiosRawSource({
          watcherConfig: input.watcherConfig,
          deploymentIdentity: input.deploymentIdentity,
          observationDepth,
          captureBounds: {
            signal: scope.signal,
            timeoutMs: Math.max(1, Math.ceil(scope.remainingMs())),
          },
        });
        assertCurrent();
        return Object.freeze({ rawSource, scope, assertCurrent, close });
      } catch (error) {
        close();
        throw error;
      }
    },
  });
};

export type WatcherStateQueueReadScopes = ReturnType<
  typeof createWatcherStateQueueReadScopes
>;
