import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { authenticatedStateQueueObservationDigest } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import { GENESIS_HEADER_HASH, Header } from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import { assertWatcherStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherQueueHeaderSource } from "../l1-follower/observation.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/package-name.js";
import { createBridge } from "./fault-decision-bridge.create-bridge.js";
import {
  ACTION_CONFIRMATION_DEPTH,
  type BridgeApplication,
  type BridgeDependencies,
  type WatcherFaultDecisionBridge,
} from "./fault-decision-bridge.selected-target.js";
import type { WatcherFaultDecisionJournal } from "./fault-decision-journal.exact-record.js";
import { openWatcherFaultDecisionJournal } from "./fault-decision-journal.js";
import { assertWatcherFaultProofApplication } from "./fault-proof-application.build-common-infrastructure.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "./fault-proof-application.js";
import type { WatcherFaultProofApplication } from "./fault-proof-application.production-dependencies.js";
import {
  watcherFaultProofDeadline,
  type WatcherFaultProofSupervisor,
} from "./fault-proof-supervisor.validate-job.js";
import { openWatcherJournalDatabase } from "./watcher-journal-database.js";
import { watcherUnprovenFaultMerges } from "./watcher-unproven-fault-merges.js";

const writeWarning: NonNullable<BridgeDependencies["warn"]> = (warning) =>
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "warn", ...warning })}\n`,
  );

export const createWatcherFaultDecisionBridge = async (input: {
  readonly application: WatcherFaultProofApplication;
  readonly supervisor: WatcherFaultProofSupervisor;
  readonly stateQueueSource: WatcherQueueHeaderSource;
  readonly journalDirectory: string;
  /** The 32-byte key the fault-proof journals' rows are authenticated with. */
  readonly authenticationKey: Uint8Array;
  readonly maximumClassificationConcurrency: number;
  readonly operationsSink?: WatcherOperationsSink;
  /** The per-header alerts raised outside the bridge, every one of them. */
  readonly perHeaderAlerts: NonNullable<BridgeDependencies["perHeaderAlerts"]>;
  readonly daFetchAlert: NonNullable<BridgeDependencies["daFetchAlert"]>;
  readonly pendingAvailabilityHeaders?: BridgeDependencies["pendingAvailabilityHeaders"];
  readonly nowMs?: () => bigint;
  /** Defaults to one JSON line on stderr per warning. */
  readonly warn?: BridgeDependencies["warn"];
}): Promise<WatcherFaultDecisionBridge> => {
  assertWatcherFaultProofApplication(input.application);
  // The journal opens on a pass's first use, not here: a refused journal
  // fails each pass (readiness reports journal_integrity) and never stops
  // the operations server from binding.
  let journal: WatcherFaultDecisionJournal | undefined;
  const decisions = async (): Promise<WatcherFaultDecisionJournal> =>
    (journal ??= await openWatcherFaultDecisionJournal({
      directory: input.journalDirectory,
      deploymentFingerprint: input.application.deploymentFingerprint,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      authenticationKey: input.authenticationKey,
    }));
  const unprovenFaultMerges = watcherUnprovenFaultMerges(() =>
    openWatcherJournalDatabase({
      journalRoot: input.journalDirectory,
      authenticationKey: input.authenticationKey,
    }),
  );
  return createBridge({
    application: input.application,
    maximumClassificationConcurrency: input.maximumClassificationConcurrency,
    dependencies: Object.freeze({
      ...(input.pendingAvailabilityHeaders === undefined
        ? {}
        : { pendingAvailabilityHeaders: input.pendingAvailabilityHeaders }),
      assertObservation: assertWatcherStateQueueObservation,
      retainDecisionAuthorities: input.application.retainDecisionAuthorities,
      observationDigest: async (candidate) =>
        await authenticatedStateQueueObservationDigest({
          observation: candidate,
          minimumConfirmationDepth: ACTION_CONFIRMATION_DEPTH,
        }),
      readRecords: async () => await (await decisions()).readAll(),
      unprovenFaultMerges: () => unprovenFaultMerges,
      append: async (decision) =>
        await (await decisions()).appendLiveDecision(decision),
      assertActuationPermitIdentity: assertWorkflowActuationPermitIdentity,
      createActuationController: (decision, rollbackGeneration) =>
        createWorkflowActuationPermitController({
          decision,
          rollbackGeneration,
        }),
      deadlineForHeader: watcherFaultProofDeadline,
      mergedHeaders: async (observation) =>
        await input.stateQueueSource.resolveMergedHeaders({ observation }),
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
      perHeaderAlerts: input.perHeaderAlerts,
      daFetchAlert: input.daFetchAlert,
      ...(input.nowMs === undefined ? {} : { nowMs: input.nowMs }),
      warn: input.warn ?? writeWarning,
      supersede: input.supervisor.supersedeDecision,
      driveProvenRemoval: async ({ headerHash, fraudProofAssetName }) =>
        await input.application.driveProvenCorrection({
          correction: { targetHeaderHash: headerHash, fraudProofAssetName },
        }),
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
    maximumClassificationConcurrency: number;
    dependencies: BridgeDependencies;
  }>,
): WatcherFaultDecisionBridge => createBridge(input);
