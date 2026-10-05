import { join } from "node:path";

import {
  createWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  WATCHER_STARTUP_READINESS_HEADER_HASH,
  type WatcherFaultProofApplication,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { createWatcherStateQueueObservationSource } from "../indexers/authenticated-state-queue-observation.js";
import {
  createWatcherStateQueueReadScopes,
  type WatcherStateQueueReadScopes,
} from "../indexers/authenticated-state-queue-observation.read-scopes.js";
import { createWatcherLocalKupmiosRawSource } from "../l1/local-kupmios-raw-source.js";
import { type WatcherProcessConfig } from "./process-config.js";
import { createWatcherStartupProgress } from "./startup-progress.js";
import { createWatcherStateQueueRuntime } from "./state-queue-runtime.js";
import { type WatcherUserEventRuntime } from "./user-event-runtime.js";
import {
  assertWatcherFaultProofLaunchScope,
  prepareJournalDirectory,
} from "./watcher-runtime.create-watcher-native-event-handler.js";
import { prepareWatcherRuntimeAuthority } from "./watcher-runtime.prepare-authority.js";
export const restoreWatcherRuntimeQueue = async (
  input: Readonly<{ config: WatcherProcessConfig }>,
  options: Pick<
    Awaited<ReturnType<typeof prepareWatcherRuntimeAuthority>>,
    "sqlite" | "deploymentIdentity"
  > &
    Readonly<{
      startup: ReturnType<typeof createWatcherStartupProgress>;
      onReadScopesAllocated(scopes: WatcherStateQueueReadScopes): void;
    }>,
) => {
  const { sqlite, deploymentIdentity, startup } = options;
  const readScopes = createWatcherStateQueueReadScopes({
    watcherConfig: input.config.watcherConfig,
    deploymentIdentity,
  });
  options.onReadScopesAllocated(readScopes);
  const rawSource = createWatcherLocalKupmiosRawSource({
    watcherConfig: input.config.watcherConfig,
    deploymentIdentity,
  });
  const inclusionRawSource = createWatcherLocalKupmiosRawSource({
    watcherConfig: input.config.watcherConfig,
    deploymentIdentity,
    observationDepth: "inclusion",
  });
  const stateQueueSource = createWatcherStateQueueObservationSource({
    deploymentIdentity,
    rawSource,
    inclusionRawSource,
    readScopes,
  });
  const stateQueueRuntime = await startup("state_queue_recovery", () =>
    createWatcherStateQueueRuntime({
      store: sqlite.stateQueueObservations,
      source: stateQueueSource,
    }),
  );
  return {
    rawSource,
    inclusionRawSource,
    stateQueueSource,
    stateQueueRuntime,
  };
};

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
    onAllocated,
  } = options;
  return await startup("workflow_readiness", async ({ retryL1Read }) => {
    const faultProofApplication = createWatcherFaultProofApplication({
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
