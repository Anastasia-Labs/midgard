import {
  createWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_STARTUP_READINESS_HEADER_HASH,
  type WatcherFaultProofApplication,
  type WatcherFaultProofL1,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import type { WatcherReplayedLedger } from "../fault-proofs/replayed-ledger.js";
import { type WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { watcherValidationTraceReplayStuckAlertSubject } from "./operations-observability.alert-book.js";
import { type WatcherOperationsSink } from "./operations-observability.watcher-operations-metrics.js";
import { type WatcherProcessConfig } from "./process-config.js";
import {
  createWatcherStartupProgress,
  WatcherStartupStageHeld,
} from "./startup-progress.js";
import { assertWatcherFaultProofLaunchScope } from "./watcher-runtime.launch-checks.js";
import { prepareWatcherRuntimeAuthority } from "./watcher-runtime.prepare-authority.js";
/**
 * Holds startup, process up, until the L1 follower has initialized its store
 * and reported a status at the tip: every later stage reads L1 through that
 * store. Catching up from the origin has no deadline, and a reason only an
 * operator clears (no origin, a rollback beyond k) holds here by name.
 */
export const untilFollowerReady = async (
  follower: Pick<WatcherFollowerRuntime, "status" | "readiness">,
  startup: ReturnType<typeof createWatcherStartupProgress>,
): Promise<void> =>
  await startup("l1_follower_ready", async () => {
    const status = follower.status();
    if (status !== null && status.readiness.length === 0) return;
    const reasons = await follower.readiness();
    throw new WatcherStartupStageHeld(
      `the L1 follower is not ready: ${
        reasons
          .map(({ reason, detail }) => `${reason}: ${detail}`)
          .join("; ") || "no status at the tip yet"
      }`,
    );
  });

export const prepareWatcherRuntimeWorkflows = async (
  input: Readonly<{ config: WatcherProcessConfig }>,
  options: Pick<
    Awaited<ReturnType<typeof prepareWatcherRuntimeAuthority>>,
    | "sqlite"
    | "deploymentAuthority"
    | "deploymentIdentity"
    | "fundingProfileOverlay"
  > &
    Readonly<{
      startup: ReturnType<typeof createWatcherStartupProgress>;
      replayedLedger: WatcherReplayedLedger;
      follower: Pick<WatcherFollowerRuntime, "status" | "readiness">;
      l1: WatcherFaultProofL1;
      onAllocated: (application: WatcherFaultProofApplication) => void;
      /** Read when a classifier's script re-execution gets stuck. */
      operationsSink: () => Pick<WatcherOperationsSink, "setAlert">;
    }>,
) => {
  const {
    deploymentAuthority,
    deploymentIdentity,
    sqlite,
    fundingProfileOverlay,
    startup,
    replayedLedger,
    follower,
    l1,
    onAllocated,
    operationsSink,
  } = options;
  await untilFollowerReady(follower, startup);
  return await startup("workflow_readiness", async ({ retryL1Read }) => {
    const faultProofApplication = createWatcherFaultProofApplication({
      l1,
      replayedLedger,
      deploymentAuthority,
      replayTranscriptStore: sqlite.replayTranscripts,
      infrastructure: input.config.faultProofInfrastructure,
      fundingProfileOverlay,
      onValidationTraceReplayStuck: (hold) =>
        operationsSink().setAlert({
          code: "validation_trace_replay_stuck",
          subjectDigest: watcherValidationTraceReplayStuckAlertSubject(
            hold.headerHash,
          ),
          active: true,
          observedAtMs: Date.now().toString(),
        }),
    });
    onAllocated(faultProofApplication);
    assertWatcherFaultProofLaunchScope(
      faultProofApplication.installedCategories,
    );
    const faultProofReadiness: WatcherFaultProofStartupReadiness[] = [];
    for (const category of WATCHER_INSTALLED_WORKFLOW_CATEGORIES) {
      // Readiness only reads L1 and builds nothing: an L1 transient
      // repeats this read, never the allocation above.
      faultProofReadiness.push(
        await retryL1Read(() =>
          faultProofApplication.assertStartupReady({
            mode: "resume",
            category,
            deploymentFingerprint: deploymentIdentity.manifestId,
            headerHash: WATCHER_STARTUP_READINESS_HEADER_HASH,
            runtimeConfigPath: input.config.watcherRuntimeConfigPath,
          }),
        ),
      );
    }
    return { faultProofApplication, faultProofReadiness };
  });
};
