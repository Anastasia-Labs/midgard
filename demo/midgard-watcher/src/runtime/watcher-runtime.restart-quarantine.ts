import {
  startWatcherNativeChainSyncWithRetry,
  watcherNativeChainSyncStartupTimeoutMs,
} from "../l1/native-chain-sync.js";
import { type WatcherBlockProgressStore } from "../storage/block-progress-store.js";
import { type WatcherDurableRuntime } from "../storage/durable-runtime.js";
import { recoverWatcherCoordinatorAfterRestart } from "./chain-coordinator.js";
import { type WatcherConfig } from "./config.js";
import { WatcherStartupStageHeld } from "./startup-progress.js";
import {
  readWatcherNativeRecoveryBoundary,
  type WatcherRestartIntersectionCandidate,
  watcherRestartIntersectionCandidates,
} from "./watcher-runtime.create-watcher-native-event-handler.js";

/**
 * One attempt at the authenticated post-finality recovery a restart runs when
 * it finds the finality state quarantined: ask the node where it intersects
 * the recorded resume points and run the durable recovery from there. While
 * that evidence is still incomplete the stage is held, so startup asks again
 * after a capped backoff instead of the process exiting only to be started
 * into the same question. The verdict is the durable recovery's own; a
 * persistence conflict, or a node that intersected outside the recorded
 * history, still fails startup.
 */
export const attemptWatcherRestartQuarantineRecovery = async (input: {
  readonly durable: WatcherDurableRuntime;
  readonly blockProgress: Pick<
    WatcherBlockProgressStore,
    "readHead" | "readCandidates"
  >;
  readonly stateQueueCursor: WatcherRestartIntersectionCandidate;
  readonly binaryPath: string;
  readonly watcherConfig: WatcherConfig;
  readonly start?: typeof startWatcherNativeChainSyncWithRetry;
  readonly recover?: typeof recoverWatcherCoordinatorAfterRestart;
}): Promise<void> => {
  const candidates = watcherRestartIntersectionCandidates({
    progressHead: input.blockProgress.readHead(),
    progressCandidates: input.blockProgress.readCandidates(),
    authorityFinalized: input.durable.readFinality().finalized,
    stateQueueCursor: input.stateQueueCursor,
  });
  const bootstrap = await (input.start ?? startWatcherNativeChainSyncWithRetry)(
    {
      binaryPath: input.binaryPath,
      watcherConfig: input.watcherConfig,
      intersectionCandidates: candidates.map(({ blockHash, slot }) => ({
        kind: "point" as const,
        blockHash,
        slot,
      })),
      startupTimeoutMs: watcherNativeChainSyncStartupTimeoutMs(
        input.watcherConfig,
      ),
      onEvent: async () => undefined,
    },
  );
  try {
    const boundary = readWatcherNativeRecoveryBoundary({
      nativeAuthority: bootstrap.authority,
      admittedIntersections: candidates,
    });
    if (
      await (input.recover ?? recoverWatcherCoordinatorAfterRestart)({
        durable: input.durable,
        restartIntersection: boundary.selectedIntersection,
      })
    )
      throw new WatcherStartupStageHeld(
        "Watcher restart remains quarantined: authenticated recovery evidence is incomplete",
      );
  } finally {
    await bootstrap.close();
  }
};
