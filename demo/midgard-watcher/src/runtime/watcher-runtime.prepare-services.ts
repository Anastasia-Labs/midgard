import { join } from "node:path";

import {
  createWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_STARTUP_READINESS_HEADER_HASH,
  type WatcherFaultProofApplication,
  type WatcherFaultProofL1,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { type WatcherProcessConfig } from "./process-config.js";
import { createWatcherStartupProgress } from "./startup-progress.js";
import { type WatcherUserEventRuntime } from "./user-event-runtime.js";
import {
  assertWatcherFaultProofLaunchScope,
  prepareJournalDirectory,
} from "./watcher-runtime.launch-checks.js";
import { prepareWatcherRuntimeAuthority } from "./watcher-runtime.prepare-authority.js";
export const prepareWatcherRuntimeWorkflows = async (
  input: Readonly<{ config: WatcherProcessConfig }>,
  options: Pick<
    Awaited<ReturnType<typeof prepareWatcherRuntimeAuthority>>,
    | "sqlite"
    | "deploymentAuthority"
    | "deploymentIdentity"
    | "historicalNativeScriptCheckpointStore"
    | "fundingProfileOverlay"
  > &
    Readonly<{
      startup: ReturnType<typeof createWatcherStartupProgress>;
      eventHistory: WatcherUserEventRuntime;
      l1: WatcherFaultProofL1;
      onAllocated: (application: WatcherFaultProofApplication) => void;
    }>,
) => {
  const {
    deploymentAuthority,
    deploymentIdentity,
    sqlite,
    historicalNativeScriptCheckpointStore,
    fundingProfileOverlay,
    startup,
    eventHistory,
    l1,
    onAllocated,
  } = options;
  return await startup("workflow_readiness", async ({ retryL1Read }) => {
    const faultProofApplication = createWatcherFaultProofApplication({
      l1,
      deploymentAuthority,
      replayTranscriptStore: sqlite.replayTranscripts,
      userEventRuntime: eventHistory,
      infrastructure: input.config.faultProofInfrastructure,
      historicalNativeScriptCheckpointStore,
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
