import {
  daRetentionPruneDecision,
  MIDGARD_RETENTION_WINDOW,
  retentionDeadlineAlert,
  type RetentionPruneDecision,
  type RetentionQueueReference,
} from "@al-ft/midgard-core";

import type { CommitteeConfig } from "../config.js";
import type {
  DaStoredPayloadRecord,
  StateQueueHeaderRecord,
} from "../domain.js";
import type { CommitteeStore } from "../store.js";

/**
 * Retention enforcement for the committee node store (GOAL_SPEC 9.4 / Q54).
 *
 * A retained DA payload is prunable once its header was removed from the state
 * queue OR its block's challengeability horizon (maturity + worst-case
 * proof-time bound) has strictly passed, unless it is the payload of the L1
 * confirmed head or of a header still live in the L1 state queue. Both
 * exemption sets come from the poller's latest authenticated L1 view, never
 * from local header rows, so the retained set is bounded by one head, the live
 * queue, and the payloads whose block ended within the horizon.
 */

export type RetentionCandidate = {
  readonly headerHash: string;
  readonly deploymentFingerprint: string;
  readonly blockEndTimeMs: number;
  readonly headerStatus: StateQueueHeaderRecord["status"] | "unobserved";
  readonly queueReference: RetentionQueueReference;
  /** Diagnostic only: payload or header written under another deployment. */
  readonly fingerprintMismatch: boolean;
  /** Diagnostic only: terminal status without authenticated L1 history. */
  readonly terminalHistoryAuthorityMismatch: boolean;
  readonly decision: RetentionPruneDecision;
};

/** The poller's latest authenticated L1 state-queue view. */
export type RetentionL1View = {
  /** Header hash in the L1 `ConfirmedState` datum. */
  readonly confirmedHeadHash: string;
  /** Hashes of every header node currently in the L1 state queue. */
  readonly liveQueueHeaderHashes: ReadonlySet<string>;
};

export type RetentionScanOptions = RetentionL1View & {
  readonly nowMs: number;
  readonly retentionDays?: number;
  /** Reported as a diagnostic when a payload or header does not match it. */
  readonly deploymentFingerprint?: string;
  /** Release-bound L1 depth used by the terminal-history diagnostic. */
  readonly minimumFinalityDepth?: number;
};

/**
 * End time the horizon is measured from: the payload's own header `endTime`,
 * or, when the store holds no representable header row, the local receipt
 * time of the payload. A header that is unobserved here and not live in the
 * L1 queue has left the queue, so no availability challenge can open against
 * it; receipt time still keeps a payload pushed before its header commits
 * retained for one full horizon, and bounds the rest.
 */
export const retentionBlockEndTimeMs = (
  payload: Pick<DaStoredPayloadRecord, "fetchedAt">,
  header: StateQueueHeaderRecord | undefined,
): number => {
  const endTime: unknown = header?.header.endTime;
  const asNumber =
    typeof endTime === "bigint"
      ? Number(endTime)
      : typeof endTime === "number"
        ? endTime
        : Number.NaN;
  if (Number.isSafeInteger(asNumber) && asNumber >= 0) {
    return asNumber;
  }
  const fetchedAtMs = Date.parse(payload.fetchedAt);
  return Number.isSafeInteger(fetchedAtMs) && fetchedAtMs >= 0
    ? fetchedAtMs
    : 0;
};

export const retentionQueueReference = (
  headerHash: string,
  view: RetentionL1View,
): RetentionQueueReference =>
  headerHash === view.confirmedHeadHash
    ? "confirmed_head"
    : view.liveQueueHeaderHashes.has(headerHash)
      ? "live_in_queue"
      : "none";

const isTerminalStatus = (status: StateQueueHeaderRecord["status"]): boolean =>
  status === "merged" || status === "removed";

const hasAuthenticatedTerminalHistory = (
  header: StateQueueHeaderRecord | undefined,
  minimumFinalityDepth: number | undefined,
): boolean => {
  if (header === undefined || !isTerminalStatus(header.status)) {
    return false;
  }
  const point = header.observedChainPoint;
  return (
    Number.isSafeInteger(minimumFinalityDepth) &&
    minimumFinalityDepth !== undefined &&
    minimumFinalityDepth >= 0 &&
    header.finalized === true &&
    point.finalized === true &&
    point.providerSource === "authenticated_state_queue_transition_v1" &&
    typeof point.slot === "number" &&
    Number.isSafeInteger(point.slot) &&
    point.slot >= 0 &&
    typeof point.blockHash === "string" &&
    /^[0-9a-f]{64}$/u.test(point.blockHash) &&
    typeof point.blockHeight === "number" &&
    Number.isSafeInteger(point.blockHeight) &&
    point.blockHeight >= 0 &&
    typeof point.depth === "number" &&
    Number.isSafeInteger(point.depth) &&
    point.depth >= minimumFinalityDepth &&
    header.computedHeaderHash === header.headerHash &&
    header.validationErrors.length === 0
  );
};

/**
 * Joins retained DA payloads to their state-queue headers and applies the core
 * retention decision to each pair.
 */
export const retentionCandidates = async (
  store: CommitteeStore,
  options: RetentionScanOptions,
): Promise<readonly RetentionCandidate[]> => {
  const payloads = await store.listDaPayloads();
  const headers = await store.listStateQueueHeaders();
  const headerByHash = new Map<string, StateQueueHeaderRecord>(
    headers.map((header) => [header.headerHash, header]),
  );

  return payloads.map((payload: DaStoredPayloadRecord) => {
    const header = headerByHash.get(payload.headerHash);
    const blockEndTimeMs = retentionBlockEndTimeMs(payload, header);
    const headerStatus = header?.status ?? "unobserved";
    const queueReference = retentionQueueReference(payload.headerHash, options);
    const fingerprintMismatch =
      (options.deploymentFingerprint !== undefined &&
        payload.deploymentFingerprint !== options.deploymentFingerprint) ||
      (header !== undefined &&
        (header.deploymentFingerprint !== payload.deploymentFingerprint ||
          (options.deploymentFingerprint !== undefined &&
            header.deploymentFingerprint !== options.deploymentFingerprint)));
    const terminalHistoryAuthorityMismatch =
      header !== undefined &&
      isTerminalStatus(header.status) &&
      !hasAuthenticatedTerminalHistory(header, options.minimumFinalityDepth);
    return {
      headerHash: payload.headerHash,
      deploymentFingerprint: payload.deploymentFingerprint,
      blockEndTimeMs,
      headerStatus,
      queueReference,
      fingerprintMismatch,
      terminalHistoryAuthorityMismatch,
      decision: daRetentionPruneDecision({
        nowMs: options.nowMs,
        blockEndTimeMs,
        headerStatus,
        queueReference,
        retentionDays: options.retentionDays,
      }),
    };
  });
};

export type RetentionPruneResult = {
  readonly scanned: number;
  readonly prunedHeaderHashes: readonly string[];
  readonly retained: number;
};

const pruneRetentionCandidates = async (
  store: CommitteeStore,
  candidates: readonly RetentionCandidate[],
  options: RetentionScanOptions,
): Promise<RetentionPruneResult> => {
  const prunedHeaderHashes: string[] = [];
  for (const candidate of candidates) {
    if (candidate.decision.decision !== "prune") {
      continue;
    }
    // The store re-decides inside its own write boundary, so a header
    // observed between the scan and the delete is decided afresh.
    const deleted = await store.deleteDaPayloadIfPrunable({
      headerHash: candidate.headerHash,
      nowMs: options.nowMs,
      retentionDays: options.retentionDays,
      confirmedHeadHash: options.confirmedHeadHash,
      liveQueueHeaderHashes: options.liveQueueHeaderHashes,
    });
    if (deleted) {
      prunedHeaderHashes.push(candidate.headerHash);
    }
  }
  return {
    scanned: candidates.length,
    prunedHeaderHashes,
    retained: candidates.length - prunedHeaderHashes.length,
  };
};

/** Deletes every retained DA payload the core retention decision prunes. */
export const pruneExpiredDaPayloads = async (
  store: CommitteeStore,
  options: RetentionScanOptions,
): Promise<RetentionPruneResult> => {
  const candidates = await retentionCandidates(store, options);
  return pruneRetentionCandidates(store, candidates, options);
};

export type RetentionDeadlineEntry = {
  readonly headerHash: string;
  readonly reasonCode: RetentionPruneDecision["reasonCode"];
  readonly challengeableUntilMs: number;
  readonly remainingMs: number;
  /** Remaining time minus the threshold; null when no threshold is set. */
  readonly headroomMs: number | null;
  readonly alerting: boolean;
};

export type RetentionDeadlineReport = {
  readonly nowMs: number;
  readonly requiredRetentionMs: number;
  readonly deployedRetentionMs: number;
  readonly marginMs: number;
  /** The operator's threshold, or null when the deadline alert is off. */
  readonly alertThresholdMs: number | null;
  readonly scanned: number;
  readonly retained: number;
  readonly prunable: number;
  readonly alerting: number;
  readonly entries: readonly RetentionDeadlineEntry[];
};

/**
 * Retention deadline options. `alertThresholdMs` is the operator's opt-in
 * deadline alert (`DA_RETENTION_ALERT_THRESHOLD_MS`): with none, no entry
 * alerts. Pruning never removes still-challengeable evidence, and every
 * retained payload ages towards its deadline on its way to pruning, so an
 * alerting entry is information, never a fault, and never affects committee
 * readiness.
 */
export type RetentionDeadlineOptions = RetentionScanOptions & {
  readonly alertThresholdMs?: number;
};

/**
 * The options of one runtime retention cycle: the operator's configuration
 * (including the opt-in alert threshold) and the L1 view accepted this tick.
 */
export const retentionCycleOptions = (
  config: Pick<
    CommitteeConfig,
    "retentionAlertThresholdMs" | "deploymentFingerprint" | "finalityDepth"
  > & {
    readonly daTransport: Pick<CommitteeConfig["daTransport"], "retentionDays">;
  },
  view: RetentionL1View,
  nowMs: number,
): RetentionDeadlineOptions => ({
  nowMs,
  alertThresholdMs: config.retentionAlertThresholdMs,
  retentionDays: config.daTransport.retentionDays,
  deploymentFingerprint: config.deploymentFingerprint,
  minimumFinalityDepth: config.finalityDepth,
  confirmedHeadHash: view.confirmedHeadHash,
  liveQueueHeaderHashes: view.liveQueueHeaderHashes,
});

const retentionDeadlineReportFromCandidates = (
  candidates: readonly RetentionCandidate[],
  options: RetentionDeadlineOptions,
): RetentionDeadlineReport => {
  const { alertThresholdMs } = options;
  if (
    alertThresholdMs !== undefined &&
    (!Number.isSafeInteger(alertThresholdMs) || alertThresholdMs < 0)
  ) {
    throw new Error("alertThresholdMs must be a non-negative safe integer");
  }
  const entries = candidates.map<RetentionDeadlineEntry>((candidate) => {
    const base = {
      headerHash: candidate.headerHash,
      reasonCode: candidate.decision.reasonCode,
      challengeableUntilMs: candidate.decision.challengeableUntilMs,
      remainingMs: candidate.decision.remainingMs,
    };
    if (alertThresholdMs === undefined) {
      return { ...base, headroomMs: null, alerting: false };
    }
    const alert = retentionDeadlineAlert({
      nowMs: options.nowMs,
      blockEndTimeMs: candidate.blockEndTimeMs,
      retentionDays: options.retentionDays,
      alertThresholdMs,
      headerHash: candidate.headerHash,
    });
    return {
      ...base,
      headroomMs: alert.headroomMs,
      alerting:
        candidate.decision.reasonCode === "still_challengeable" &&
        alert.alerting,
    };
  });
  const prunable = candidates.filter(
    (candidate) => candidate.decision.decision === "prune",
  ).length;
  return {
    nowMs: options.nowMs,
    requiredRetentionMs: MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    deployedRetentionMs: MIDGARD_RETENTION_WINDOW.deployedRetentionMs,
    marginMs: MIDGARD_RETENTION_WINDOW.marginMs,
    alertThresholdMs: alertThresholdMs ?? null,
    scanned: candidates.length,
    retained: candidates.length - prunable,
    prunable,
    alerting: entries.filter((entry) => entry.alerting).length,
    entries,
  };
};

/** Executable deadline report over the retained DA payload set. */
export const retentionDeadlineReport = async (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
): Promise<RetentionDeadlineReport> => {
  const candidates = await retentionCandidates(store, options);
  return retentionDeadlineReportFromCandidates(candidates, options);
};

export type RetentionCycleResult = {
  readonly deadlines: RetentionDeadlineReport;
  readonly prune: RetentionPruneResult;
};

/** One non-overlapping production retention cycle: report before deletion. */
export const runRetentionCycle = async (
  store: CommitteeStore,
  options: RetentionDeadlineOptions,
): Promise<RetentionCycleResult> => {
  // Use one joined snapshot for both reporting and deletion. Incoming DA writes
  // may run concurrently with the committee node tick; a second scan could otherwise
  // delete a record that was never present in the preceding report.
  const candidates = await retentionCandidates(store, options);
  const deadlines = retentionDeadlineReportFromCandidates(candidates, options);
  const prune = await pruneRetentionCandidates(store, candidates, options);
  return { deadlines, prune };
};
