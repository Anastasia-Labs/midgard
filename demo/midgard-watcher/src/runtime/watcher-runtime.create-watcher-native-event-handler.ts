import { mkdir, readFile, realpath } from "node:fs/promises";

import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import { type WatcherAvailabilityRuntime } from "../availability/runtime.js";
import {
  type WatcherFaultProofApplication,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { type WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import { type WatcherFinalityPolicy } from "../l1/finality-engine.js";
import {
  type WatcherNativeChainSyncAuthority,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
} from "../l1/native-chain-sync.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import { type WatcherChainCoordinator } from "./chain-coordinator.js";
import { parseWatcherConfigJson } from "./config.js";
import { type VerifiedWatcherDeploymentAuthority } from "./deployment-authority.js";
import { createWatcherHistoryRecovery } from "./history-recovery.js";
import {
  type WatcherOperationsObservability,
  type WatcherOperationsSink,
} from "./operations-observability.js";
import { type WatcherProcessConfig } from "./process-config.js";

export const WATCHER_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-runtime-v1" as const;

export type WatcherRuntime = Readonly<{
  schemaVersion: typeof WATCHER_RUNTIME_SCHEMA_VERSION;
  deploymentAuthority: VerifiedWatcherDeploymentAuthority;
  policy: WatcherFinalityPolicy;
  coordinator: WatcherChainCoordinator;
  faultProofApplication: WatcherFaultProofApplication;
  faultProofReadiness: readonly WatcherFaultProofStartupReadiness[];
  faultProofSupervisor: WatcherFaultProofSupervisor;
  operations: WatcherOperationsObservability;
  operationsEndpoint: string;
  recoveredFaultProofWorkflowCount: number;
  availability: WatcherAvailabilityRuntime;
  done: Promise<void>;
  caughtUp: Promise<void>;
  status(): Readonly<{
    phase: "live" | "closing" | "closed" | "failed";
    liveness: boolean;
    readiness: boolean;
    caughtUp: boolean;
    historyRecovery: ReturnType<
      ReturnType<typeof createWatcherHistoryRecovery>["status"]
    >;
    proofSupervisor: ReturnType<WatcherFaultProofSupervisor["status"]>;
    availability: ReturnType<WatcherAvailabilityRuntime["status"]>;
  }>;
  close(): Promise<void>;
}>;

/**
 * A partial replay/runner union is not a production classifier: an omitted
 * family could otherwise be misreported as healthy. Launch therefore requires
 * the exact canonical catalogue, in canonical order, before native L1 intake.
 */
export const assertWatcherFaultProofLaunchScope = (
  categories: readonly string[],
): void => {
  if (
    categories.length !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length ||
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category, index) => categories[index] !== category,
    )
  ) {
    throw new Error(
      "watcher production fault-proof application does not cover the exact canonical catalogue",
    );
  }
};

export type WatcherRestartIntersectionCandidate = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
}>;

/**
 * Check that the native stream intersected one of the recorded resume points
 * and that its tip is not behind that point. The replay depth is unbounded:
 * every block between the intersection and the tip is replayed, and only a
 * node whose chain excludes all recorded history is refused.
 */
export const readWatcherNativeRecoveryBoundary = (input: {
  readonly nativeAuthority: WatcherNativeChainSyncAuthority;
  readonly admittedIntersections: readonly WatcherRestartIntersectionCandidate[];
}) => {
  const details = watcherNativeChainSyncAuthorityDetails(input.nativeAuthority);
  if (details === null) {
    throw new Error("native chain-sync authority expired during startup");
  }
  const selected = details.selectedIntersection;
  const admitted =
    selected.kind === "point"
      ? input.admittedIntersections.find(
          (candidate) =>
            candidate.blockHash === selected.blockHash &&
            candidate.slot === selected.slot,
        )
      : undefined;
  if (selected.kind !== "point" || admitted === undefined) {
    throw new Error(
      "native chain-sync selected a point outside the recorded resume history",
    );
  }
  if (
    details.currentTip.kind !== "point" ||
    BigInt(details.currentTip.blockNo) < BigInt(admitted.blockNo)
  ) {
    throw new Error(
      "native chain-sync tip is behind the selected resume point",
    );
  }
  return Object.freeze({
    ...details,
    selectedIntersection: selected,
    selectedBlockNo: admitted.blockNo,
    currentTip: details.currentTip,
  });
};

/**
 * Resume point first, then spaced progress rows, the durable finality
 * authority and the state-queue cursor, so a node that forked below the
 * head still finds a recorded ancestor.
 */
export const watcherRestartIntersectionCandidates = (input: {
  readonly progressHead: WatcherRestartIntersectionCandidate | null;
  readonly progressCandidates: readonly WatcherRestartIntersectionCandidate[];
  readonly authorityFinalized: WatcherRestartIntersectionCandidate | null;
  readonly oldestAuthenticatedHint?: WatcherRestartIntersectionCandidate | null;
  readonly stateQueueCursor: WatcherRestartIntersectionCandidate;
}): readonly WatcherRestartIntersectionCandidate[] => {
  const ordered = [
    input.progressHead ?? input.authorityFinalized ?? input.stateQueueCursor,
    ...input.progressCandidates,
    ...(input.authorityFinalized === null ? [] : [input.authorityFinalized]),
    input.stateQueueCursor,
    ...(input.oldestAuthenticatedHint == null
      ? []
      : [input.oldestAuthenticatedHint]),
  ];
  const seen = new Set<string>();
  const candidates: WatcherRestartIntersectionCandidate[] = [];
  for (const candidate of ordered) {
    const key = `${candidate.blockHash}@${candidate.slot}`;
    if (seen.has(key)) continue;
    seen.add(key);
    candidates.push(
      Object.freeze({
        blockHash: candidate.blockHash,
        blockNo: candidate.blockNo,
        slot: candidate.slot,
      }),
    );
    if (candidates.length === 128) break;
  }
  return Object.freeze(candidates);
};

const sameTipPoint = (event: WatcherNativeChainSyncEvent): boolean => {
  if (event.tip.kind === "origin")
    return event.kind === "roll_backward" && event.point.kind === "origin";
  return event.kind === "roll_forward"
    ? event.blockHash === event.tip.blockHash &&
        event.slot === event.tip.slot &&
        event.blockNo === event.tip.blockNo
    : event.point.kind === "point" &&
        event.point.blockHash === event.tip.blockHash &&
        event.point.slot === event.tip.slot;
};

export const createWatcherNativeEventHandler =
  (input: {
    readonly coordinator: Promise<Pick<WatcherChainCoordinator, "handle">>;
    readonly onCaughtUp: () => void;
    readonly onRollbackArrived?: () => void;
    readonly operationsSink?: WatcherOperationsSink;
    readonly sourceIdentityDigest?: string;
    readonly nowMs?: () => bigint;
  }): ((event: WatcherNativeChainSyncEvent) => Promise<void>) =>
  async (event) => {
    if (event.kind === "roll_backward") input.onRollbackArrived?.();
    const coordinator = await input.coordinator;
    await coordinator.handle(event);
    if (
      input.operationsSink !== undefined &&
      input.sourceIdentityDigest !== undefined
    ) {
      const observedAtMs = (input.nowMs?.() ?? BigInt(Date.now())).toString();
      if (event.kind === "roll_forward") {
        input.operationsSink.recordL1Source({
          sourceIdentityDigest: input.sourceIdentityDigest,
          sourceMode: "local_node",
          status: "consistent",
          blockHash: event.blockHash,
          blockNo: event.blockNo,
          slot: event.slot,
          observedAtMs,
        });
        input.operationsSink.setAlert({
          code: "chain_rollback",
          subjectDigest: input.sourceIdentityDigest,
          active: false,
          observedAtMs,
        });
      } else {
        input.operationsSink.setAlert({
          code: "chain_rollback",
          subjectDigest: input.sourceIdentityDigest,
          active: true,
          observedAtMs,
        });
        if (event.tip.kind === "point") {
          input.operationsSink.recordL1Source({
            sourceIdentityDigest: input.sourceIdentityDigest,
            sourceMode: "local_node",
            status: "consistent",
            blockHash: event.tip.blockHash,
            blockNo: event.tip.blockNo,
            slot: event.tip.slot,
            observedAtMs,
          });
        }
      }
    }
    if (sameTipPoint(event)) input.onCaughtUp();
  };

export const requireWatcherRuntimeConfig = async (
  config: WatcherProcessConfig,
): Promise<void> => {
  if (
    (await realpath(config.watcherRuntimeConfigPath)) !==
    config.watcherRuntimeConfigPath
  ) {
    throw new Error("watcher workflow runtime config traverses a symlink");
  }
  const raw = await readFile(config.watcherRuntimeConfigPath, "utf8");
  const parsed = parseWatcherConfigJson(raw);
  if (
    watcherCanonicalJson(parsed) !== watcherCanonicalJson(config.watcherConfig)
  ) {
    throw new Error(
      "watcher process and workflow runtime configurations differ",
    );
  }
};

export const prepareJournalDirectory = async (path: string): Promise<void> => {
  await mkdir(path, { recursive: true, mode: 0o700 });
  if ((await realpath(path)) !== path) {
    throw new Error("watcher workflow journal directory traverses a symlink");
  }
};
