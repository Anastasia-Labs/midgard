import type { WatcherStateQueueReadScopes } from "../indexers/authenticated-state-queue-observation.read-scopes.js";
import {
  type WatcherNativeChainSyncPoint,
  watcherNativeChainSyncStartupTimeoutMs,
} from "../l1/native-chain-sync.js";
import type { WatcherChainCoordinator } from "./chain-coordinator.js";
import type { WatcherOperationsSink } from "./operations-observability.js";
import type { WatcherProcessConfig } from "./process-config.js";
import {
  createWatcherNativeEventHandler,
  type WatcherRestartIntersectionCandidate,
} from "./watcher-runtime.create-watcher-native-event-handler.js";

/** Synchronous options construction preserves native start/await ordering. */
export const watcherRuntimeNativeStartupOptions = (
  input: Readonly<{ config: WatcherProcessConfig }>,
  options: Readonly<{
    intersectionCandidates: readonly WatcherRestartIntersectionCandidate[];
    coordinator: Promise<Pick<WatcherChainCoordinator, "handle">>;
    onCaughtUp(): void;
    operationsSink: WatcherOperationsSink;
    sourceIdentityDigest: string;
    readScopes: WatcherStateQueueReadScopes;
  }>,
) => ({
  binaryPath: input.config.nativeChainSyncBinaryPath,
  watcherConfig: input.config.watcherConfig,
  // The node selects the newest retained point; coordinator rewinds above it.
  intersectionCandidates: Object.freeze(
    options.intersectionCandidates.map(
      ({ blockHash, slot }): WatcherNativeChainSyncPoint =>
        Object.freeze({ kind: "point", blockHash, slot }),
    ),
  ),
  startupTimeoutMs: watcherNativeChainSyncStartupTimeoutMs(
    input.config.watcherConfig,
  ),
  onAuthorityRevoked: options.readScopes.invalidate,
  onEvent: createWatcherNativeEventHandler({
    coordinator: options.coordinator,
    onCaughtUp: options.onCaughtUp,
    operationsSink: options.operationsSink,
    sourceIdentityDigest: options.sourceIdentityDigest,
    onRollbackArrived: options.readScopes.invalidate,
  }),
});

export const observeWatcherNativeReadLifetime = (
  done: Promise<void>,
  readScopes: WatcherStateQueueReadScopes,
  rejectCaughtUp: (error: Error) => void,
): void => {
  void done
    .then(
      () => readScopes.close(),
      (error: unknown) => {
        readScopes.close();
        rejectCaughtUp(
          error instanceof Error ? error : new Error(String(error)),
        );
      },
    )
    .catch(rejectCaughtUp);
};
