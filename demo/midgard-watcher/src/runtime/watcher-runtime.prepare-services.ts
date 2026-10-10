import { join } from "node:path";

import {
  createWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_STARTUP_READINESS_HEADER_HASH,
  type WatcherFaultProofApplication,
  type WatcherFaultProofL1,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { type WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { type WatcherUserEvents } from "../l1-follower/user-events.js";
import { type WatcherProcessConfig } from "./process-config.js";
import {
  createWatcherStartupProgress,
  WatcherStartupStageHeld,
} from "./startup-progress.js";
import {
  assertWatcherFaultProofLaunchScope,
  prepareJournalDirectory,
} from "./watcher-runtime.launch-checks.js";
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
      userEvents: WatcherUserEvents;
      follower: Pick<WatcherFollowerRuntime, "status" | "readiness">;
      l1: WatcherFaultProofL1;
      onAllocated: (application: WatcherFaultProofApplication) => void;
    }>,
) => {
  const {
    deploymentAuthority,
    deploymentIdentity,
    sqlite,
    fundingProfileOverlay,
    startup,
    userEvents,
    follower,
    l1,
    onAllocated,
  } = options;
  await untilFollowerReady(follower, startup);
  return await startup("workflow_readiness", async ({ retryL1Read }) => {
    const faultProofApplication = createWatcherFaultProofApplication({
      l1,
      deploymentAuthority,
      replayTranscriptStore: sqlite.replayTranscripts,
      userEvents,
      infrastructure: input.config.faultProofInfrastructure,
      fundingProfileOverlay,
    });
    onAllocated(faultProofApplication);
    assertWatcherFaultProofLaunchScope(
      faultProofApplication.installedCategories,
    );
    const faultProofReadiness: WatcherFaultProofStartupReadiness[] = [];
    for (const category of WATCHER_INSTALLED_WORKFLOW_CATEGORIES) {
      const journalDirectory = join(
        input.config.workflowJournalDirectory,
        "readiness",
        category,
      );
      await prepareJournalDirectory(journalDirectory);
      // Readiness only reads L1 and builds nothing: an L1 transient
      // repeats this read, never the allocation above.
      faultProofReadiness.push(
        await retryL1Read(() =>
          faultProofApplication.assertStartupReady({
            mode: "resume",
            category,
            deploymentFingerprint: deploymentIdentity.manifestId,
            headerHash: WATCHER_STARTUP_READINESS_HEADER_HASH,
            journalDirectory,
            runtimeConfigPath: input.config.watcherRuntimeConfigPath,
          }),
        ),
      );
    }
    return { faultProofApplication, faultProofReadiness };
  });
};
