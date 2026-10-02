import { createHash } from "node:crypto";
import { readFile, realpath } from "node:fs/promises";

import {
  assertWorkflowActuationPermitIdentity,
  authenticatedStateQueueObservationDigest,
  createWorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs";
import { GENESIS_HEADER_HASH, Header } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueObservation,
  type WatcherStateQueueObservationSource,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import { watcherDeferredRetryDelayMs } from "./fault-decision-bridge.classification-miss.js";
import { createBridge } from "./fault-decision-bridge.create-bridge.js";
import {
  ACTION_CONFIRMATION_DEPTH,
  type BridgeApplication,
  type BridgeDependencies,
  type WatcherFaultDecisionBridge,
} from "./fault-decision-bridge.selected-target.js";
import { openWatcherFaultDecisionJournal } from "./fault-decision-journal.js";
import {
  assertWatcherFaultProofApplication,
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  type WatcherFaultProofApplication,
} from "./fault-proof-application.js";
import {
  watcherFaultProofDeadline,
  type WatcherFaultProofSupervisor,
} from "./fault-proof-supervisor.js";

const writeWarning: NonNullable<BridgeDependencies["warn"]> = (warning) =>
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "warn", ...warning })}\n`,
  );

export const createWatcherFaultDecisionBridge = async (input: {
  readonly application: WatcherFaultProofApplication;
  readonly supervisor: WatcherFaultProofSupervisor;
  readonly stateQueueSource: WatcherStateQueueObservationSource;
  readonly journalDirectory: string;
  readonly runtimeConfigPath: string;
  readonly maximumClassificationConcurrency: number;
  readonly operationsSink?: WatcherOperationsSink;
  readonly pendingAvailabilityHeaders?: BridgeDependencies["pendingAvailabilityHeaders"];
  readonly nowMs?: () => bigint;
  /** Defaults to one JSON line on stderr per warning. */
  readonly warn?: BridgeDependencies["warn"];
}): Promise<WatcherFaultDecisionBridge> => {
  assertWatcherFaultProofApplication(input.application);
  const journal = await openWatcherFaultDecisionJournal({
    directory: input.journalDirectory,
    deploymentFingerprint: input.application.deploymentFingerprint,
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  });
  return createBridge({
    application: input.application,
    runtimeConfigPath: input.runtimeConfigPath,
    maximumClassificationConcurrency: input.maximumClassificationConcurrency,
    dependencies: Object.freeze({
      ...(input.pendingAvailabilityHeaders === undefined
        ? {}
        : { pendingAvailabilityHeaders: input.pendingAvailabilityHeaders }),
      assertObservation: assertWatcherStateQueueObservation,
      retainDecisionAuthorities: input.application.retainDecisionAuthorities,
      decisionUsesLocalEventHistory:
        input.application.decisionUsesLocalEventHistory,
      observationDigest: async (candidate) =>
        await authenticatedStateQueueObservationDigest({
          observation: candidate,
          minimumConfirmationDepth: ACTION_CONFIRMATION_DEPTH,
        }),
      classificationContextIdentity: async () => {
        const path = await realpath(input.runtimeConfigPath);
        const contents = await readFile(path);
        return createHash("sha256")
          .update(path)
          .update("\0")
          .update(contents)
          .digest("hex");
      },
      readRecords: journal.readAll,
      append: journal.appendLiveDecision,
      assertActuationPermitIdentity: assertWorkflowActuationPermitIdentity,
      createActuationController: (decision, rollbackGeneration) =>
        createWorkflowActuationPermitController({
          decision,
          rollbackGeneration,
        }),
      deadlineForHeader: watcherFaultProofDeadline,
      deferredRetryDelayMs: watcherDeferredRetryDelayMs,
      mergedHeaders: async (observation) =>
        (await input.stateQueueSource.resolveMergedHeaders?.({
          observation,
        })) ?? new Map(),
      resolvePredecessorHeader: async (header) => {
        const decoded = Data.from(header.headerCborHex, Header);
        if (decoded.prevHeaderHash === GENESIS_HEADER_HASH) return undefined;
        return await input.stateQueueSource.resolveRetainedHeader({
          headerHash: decoded.prevHeaderHash,
        });
      },
      ...(input.operationsSink === undefined
        ? {}
        : { operationsSink: input.operationsSink }),
      ...(input.nowMs === undefined ? {} : { nowMs: input.nowMs }),
      warn: input.warn ?? writeWarning,
      requestProgress: input.supervisor.requestProgress,
      revokeAuthority: input.supervisor.revokeAuthority,
      unfinishedObjectiveCount: () =>
        input.supervisor.status().unfinishedObjectiveCount,
    }),
  });
};

/** Test-only dependency seam; it never mints production application authority. */
export const unsafeCreateWatcherFaultDecisionBridgeForTest = (
  input: Readonly<{
    application: BridgeApplication;
    runtimeConfigPath: string;
    maximumClassificationConcurrency: number;
    dependencies: BridgeDependencies;
  }>,
): WatcherFaultDecisionBridge => createBridge(input);
