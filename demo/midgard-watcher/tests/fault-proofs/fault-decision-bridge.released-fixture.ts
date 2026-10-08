import { RetainedDaPayloadUnavailableError } from "@al-ft/midgard-fault-proofs";
import type { DaAvailabilityStateQueueStatus } from "@al-ft/midgard-sdk";

import type { WatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import type {
  WatcherMergedHeaderProof,
  WatcherRemovedHeaderProof,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import {
  createWatcherOperationsObservability,
  type WatcherVerificationDiagnostic,
} from "../../src/runtime/operations-observability.js";
import { DEPLOYMENT } from "./fault-decision-bridge.observation.js";

export const ATTESTED: DaAvailabilityStateQueueStatus = Object.freeze({
  Attested: Object.freeze({ commitment_hash: "44".repeat(32) }),
});

export const MERGE_TX = "4d".repeat(32);

export const REMOVAL_TX = "4f".repeat(32);

export const operations = () =>
  createWatcherOperationsObservability({
    deploymentFingerprint: DEPLOYMENT,
    supervisor: {
      status: () => ({
        phase: "accepting",
        recovered: true,
        queuedJobCount: 0,
        activeJob: null,
        blockedJob: null,
        deadlineHealth: "safe",
        earliestDeadlineJob: null,
        remainingSafeStartMs: "1000",
        journalDecisionMissing: [],
      }),
    } as unknown as WatcherFaultProofSupervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    durableProofQueueStatus: () => ({
      queuedJobCount: 0,
      oldestQueuedAtMs: null,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
  });

export const records = (observability: ReturnType<typeof operations>) =>
  observability.api.diagnostics({ kind: "verification" })
    .records as WatcherVerificationDiagnostic[];

export const proof = (headerHash: string): WatcherMergedHeaderProof =>
  Object.freeze({
    headerHash,
    mergeTransactionHash: MERGE_TX,
    mergeBlockHash: "4e".repeat(32),
    mergeSlot: "1200",
    mergeBlockNo: "120",
    confirmationDepth: "3",
  });

export const removedProof = (headerHash: string): WatcherRemovedHeaderProof =>
  Object.freeze({
    headerHash,
    removalTransactionHash: REMOVAL_TX,
    removalKind: "RemoveUnattestedBlockAfterTimeout",
    removalBlockHash: "50".repeat(32),
    removalSlot: "1300",
    removalBlockNo: "130",
    confirmationDepth: "3",
  });

export const unavailable = (headerHash: string) =>
  new RetainedDaPayloadUnavailableError(
    headerHash,
    `no public retained-DA source served header ${headerHash}`,
  );
