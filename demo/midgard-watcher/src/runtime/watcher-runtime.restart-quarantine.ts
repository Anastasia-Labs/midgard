import {
  startWatcherNativeChainSyncWithRetry,
  watcherNativeChainSyncStartupTimeoutMs,
} from "../l1/native-chain-sync.js";
import { type WatcherBlockProgressStore } from "../storage/block-progress-store.js";
import { type WatcherDurableRuntime } from "../storage/durable-runtime.js";
import { WatcherDurableAuthorityConflict } from "../storage/durable-runtime.load-published-authority.js";
import { recoverWatcherCoordinatorAfterRestart } from "./chain-coordinator.js";
import { oldestRetainedCanonicalHint } from "./chain-coordinator.retained-canonical-prefix.js";
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
 * publication conflict is reauthenticated on the next attempt. A node that
 * intersects outside the recorded history still fails startup.
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
  if (input.durable.reconcile !== undefined) {
    try {
      await input.durable.reconcile();
    } catch {
      throw new WatcherStartupStageHeld(
        "Watcher restart recovery held: durable authority cannot yet be reauthenticated",
      );
    }
    if (input.durable.readFinality().phase !== "quarantined") return;
  }
  const candidates = watcherRestartIntersectionCandidates({
    oldestAuthenticatedHint: oldestRetainedCanonicalHint(input.durable),
    progressHead: input.blockProgress.readHead(),
    progressCandidates: input.blockProgress.readCandidates(),
    authorityFinalized: input.durable.readFinality().finalized,
    stateQueueCursor: input.stateQueueCursor,
  });
  let sourceGeneration = 0;
  let firstArrival = true;
  let selected:
    | ReturnType<
        typeof readWatcherNativeRecoveryBoundary
      >["selectedIntersection"]
    | null = null;
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
      onEvent: async (event) => {
        if (
          event.kind === "roll_backward" &&
          !(
            firstArrival &&
            selected !== null &&
            event.point.kind === "point" &&
            event.point.blockHash === selected.blockHash &&
            event.point.slot === selected.slot
          )
        )
          sourceGeneration += 1;
        firstArrival = false;
      },
    },
  );
  try {
    const boundary = readWatcherNativeRecoveryBoundary({
      nativeAuthority: bootstrap.authority,
      admittedIntersections: candidates,
    });
    selected = boundary.selectedIntersection;
    const capturedGeneration = sourceGeneration;
    if (
      await (input.recover ?? recoverWatcherCoordinatorAfterRestart)({
        durable: input.durable,
        restartIntersection: boundary.selectedIntersection,
        assertCurrent: () => {
          try {
            readWatcherNativeRecoveryBoundary({
              nativeAuthority: bootstrap.authority,
              admittedIntersections: candidates,
            });
          } catch (error) {
            throw new WatcherDurableAuthorityConflict(
              "watcher restart native authority expired before recovery CAS",
              { cause: error },
            );
          }
          if (sourceGeneration !== capturedGeneration)
            throw new WatcherDurableAuthorityConflict(
              "watcher restart native generation changed before recovery CAS",
            );
        },
      })
    )
      throw new WatcherStartupStageHeld(
        "Watcher restart remains quarantined: authenticated recovery evidence is incomplete",
      );
  } catch (error) {
    if (error instanceof WatcherDurableAuthorityConflict)
      throw new WatcherStartupStageHeld(
        "Watcher restart recovery held: durable authority publication conflicted",
      );
    throw error;
  } finally {
    await bootstrap.close();
  }
};
