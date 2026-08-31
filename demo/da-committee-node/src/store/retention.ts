import {
  daRetentionPruneDecisionV1,
  MIDGARD_RETENTION_WINDOW_V1,
  retentionDeadlineAlertV1,
  type RetentionPruneDecisionV1,
} from "@al-ft/midgard-core";

import type {
  DaStoredPayloadRecordV1,
  StateQueueHeaderRecord,
} from "../domain.js";
import type { WatcherStore } from "../store.js";

/**
 * Retention enforcement for the committee watcher store (GOAL_SPEC 9.4 / Q54).
 *
 * Retained DA payloads are only prunable once BOTH the block's challengeability
 * horizon (maturity + worst-case proof-time bound) has strictly passed AND its
 * L1 state-queue header has reached a terminal outcome. Every unknown - missing
 * header row, missing block end time, unrecognised status, foreign deployment
 * fingerprint - retains.
 */

export type RetentionCandidateV1 = {
  readonly headerHash: string;
  readonly deploymentFingerprint: string;
  readonly blockEndTimeMs: number | null;
  readonly headerStatus: StateQueueHeaderRecord["status"] | undefined;
  readonly headerPresent: boolean;
  readonly fingerprintMismatch: boolean;
  readonly terminalHistoryAuthorityMismatch: boolean;
  readonly availabilityChallengeAuthorityMismatch: boolean;
  readonly activeAvailabilityChallenge: boolean;
  readonly decision: RetentionPruneDecisionV1;
};

export type RetentionScanOptionsV1 = {
  readonly nowMs: number;
  readonly retentionDays?: number;
  /**
   * When set, a payload written under a different deployment fingerprint is
   * always retained (and reported), never pruned.
   */
  readonly deploymentFingerprint?: string;
  /**
   * Release-bound L1 depth. A terminal header is never pruning authority when
   * this is absent/malformed or its authenticated transition was shallower.
   */
  readonly minimumFinalityDepth?: number;
  readonly availabilityChallengeAuthority?: {
    readonly deploymentFingerprint: string;
    readonly capability: "deployed";
    readonly activeHeaderHashes: ReadonlySet<string>;
  };
};

const blockEndTimeMsOf = (
  header: StateQueueHeaderRecord | undefined,
): number | null => {
  if (header === undefined) {
    return null;
  }
  const endTime = header.header.endTime;
  const asNumber =
    typeof endTime === "bigint"
      ? Number(endTime)
      : typeof endTime === "number"
        ? endTime
        : Number.NaN;
  return Number.isSafeInteger(asNumber) && asNumber >= 0 ? asNumber : null;
};

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
export const retentionCandidatesV1 = async (
  store: WatcherStore,
  options: RetentionScanOptionsV1,
): Promise<readonly RetentionCandidateV1[]> => {
  const payloads = await store.listDaPayloads();
  const headers = await store.listStateQueueHeaders();
  const headerByHash = new Map<string, StateQueueHeaderRecord>(
    headers.map((header) => [header.headerHash, header]),
  );

  return payloads.map((payload: DaStoredPayloadRecordV1) => {
    const header = headerByHash.get(payload.headerHash);
    const blockEndTimeMs = blockEndTimeMsOf(header);
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
    const challengeAuthority = options.availabilityChallengeAuthority;
    const availabilityChallengeAuthorityMismatch =
      challengeAuthority === undefined ||
      options.deploymentFingerprint === undefined ||
      challengeAuthority.deploymentFingerprint !==
        options.deploymentFingerprint;
    const activeAvailabilityChallenge =
      !availabilityChallengeAuthorityMismatch &&
      challengeAuthority.capability === "deployed" &&
      challengeAuthority.activeHeaderHashes.has(payload.headerHash);
    const availabilityChallengeState = availabilityChallengeAuthorityMismatch
      ? "unknown"
      : activeAvailabilityChallenge
        ? "active"
        : "inactive";
    const decision = daRetentionPruneDecisionV1(
      { headerHash: payload.headerHash, blockEndTimeMs },
      {
        nowMs: options.nowMs,
        retentionDays: options.retentionDays,
        // A missing header row yields an undefined status, which the core
        // decision treats as unknown and therefore retains.
        headerStatus: terminalHistoryAuthorityMismatch
          ? undefined
          : header?.status,
        availabilityChallengeState,
      },
    );
    return {
      headerHash: payload.headerHash,
      deploymentFingerprint: payload.deploymentFingerprint,
      blockEndTimeMs,
      headerStatus: header?.status,
      headerPresent: header !== undefined,
      fingerprintMismatch,
      terminalHistoryAuthorityMismatch,
      availabilityChallengeAuthorityMismatch,
      activeAvailabilityChallenge,
      decision: fingerprintMismatch
        ? { decision: "retain", reasonCode: "header_status_unknown" }
        : decision,
    };
  });
};

export type RetentionPruneResultV1 = {
  readonly scanned: number;
  readonly prunedHeaderHashes: readonly string[];
  readonly retained: number;
};

const pruneRetentionCandidatesV1 = async (
  store: WatcherStore,
  candidates: readonly RetentionCandidateV1[],
): Promise<RetentionPruneResultV1> => {
  const prunedHeaderHashes: string[] = [];
  for (const candidate of candidates) {
    if (
      candidate.decision.decision !== "prune" ||
      candidate.decision.reasonCode !== "expired_and_terminal" ||
      candidate.fingerprintMismatch ||
      candidate.terminalHistoryAuthorityMismatch ||
      candidate.availabilityChallengeAuthorityMismatch
    ) {
      continue;
    }
    const deleted = await store.deleteDaPayload(candidate.headerHash);
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

/**
 * Deletes only `expired_and_terminal` retained DA payloads.
 *
 * Terminal status must come from authenticated ordered L1 transition history;
 * neither local disappearance nor a latest-root snapshot is sufficient.
 */
export const pruneExpiredDaPayloadsV1 = async (
  store: WatcherStore,
  options: RetentionScanOptionsV1,
): Promise<RetentionPruneResultV1> => {
  const candidates = await retentionCandidatesV1(store, options);
  return pruneRetentionCandidatesV1(store, candidates);
};

export type RetentionDeadlineEntryV1 = {
  readonly headerHash: string;
  readonly reasonCode: RetentionPruneDecisionV1["reasonCode"];
  readonly challengeableUntilMs: number | null;
  readonly remainingMs: number | null;
  readonly headroomMs: number | null;
  readonly alerting: boolean;
};

export type RetentionDeadlineReportV1 = {
  readonly nowMs: number;
  readonly requiredRetentionMs: number;
  readonly deployedRetentionMs: number;
  readonly marginMs: number;
  readonly alertThresholdMs: number;
  readonly scanned: number;
  readonly retained: number;
  readonly prunable: number;
  readonly alerting: number;
  readonly entries: readonly RetentionDeadlineEntryV1[];
};

const retentionDeadlineReportFromCandidatesV1 = (
  candidates: readonly RetentionCandidateV1[],
  options: RetentionScanOptionsV1 & { readonly alertThresholdMs?: number },
): RetentionDeadlineReportV1 => {
  const alertThresholdMs =
    options.alertThresholdMs ?? MIDGARD_RETENTION_WINDOW_V1.marginMs;
  if (!Number.isSafeInteger(alertThresholdMs) || alertThresholdMs < 0) {
    throw new Error("alertThresholdMs must be a non-negative safe integer");
  }
  const entries = candidates.map<RetentionDeadlineEntryV1>((candidate) => {
    if (
      candidate.blockEndTimeMs === null ||
      candidate.fingerprintMismatch ||
      candidate.terminalHistoryAuthorityMismatch ||
      candidate.availabilityChallengeAuthorityMismatch
    ) {
      // Fail closed: no computable deadline, so report it as alerting rather
      // than fabricating headroom.
      return {
        headerHash: candidate.headerHash,
        reasonCode: candidate.decision.reasonCode,
        challengeableUntilMs: null,
        remainingMs: null,
        headroomMs: null,
        alerting: true,
      };
    }
    const alert = retentionDeadlineAlertV1({
      nowMs: options.nowMs,
      blockEndTimeMs: candidate.blockEndTimeMs,
      retentionDays: options.retentionDays,
      alertThresholdMs,
      headerHash: candidate.headerHash,
    });
    return {
      headerHash: candidate.headerHash,
      reasonCode: candidate.decision.reasonCode,
      challengeableUntilMs: alert.challengeableUntilMs,
      remainingMs: alert.remainingMs,
      headroomMs: alert.headroomMs,
      alerting: candidate.decision.decision === "retain" && alert.alerting,
    };
  });
  const prunable = candidates.filter(
    (candidate) => candidate.decision.decision === "prune",
  ).length;
  return {
    nowMs: options.nowMs,
    requiredRetentionMs: MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
    deployedRetentionMs: MIDGARD_RETENTION_WINDOW_V1.deployedRetentionMs,
    marginMs: MIDGARD_RETENTION_WINDOW_V1.marginMs,
    alertThresholdMs,
    scanned: candidates.length,
    retained: candidates.length - prunable,
    prunable,
    alerting: entries.filter((entry) => entry.alerting).length,
    entries,
  };
};

/** Executable deadline report over the retained DA payload set. */
export const retentionDeadlineReportV1 = async (
  store: WatcherStore,
  options: RetentionScanOptionsV1 & { readonly alertThresholdMs?: number },
): Promise<RetentionDeadlineReportV1> => {
  const candidates = await retentionCandidatesV1(store, options);
  return retentionDeadlineReportFromCandidatesV1(candidates, options);
};

export type RetentionCycleResultV1 = {
  readonly deadlines: RetentionDeadlineReportV1;
  readonly prune: RetentionPruneResultV1;
};

/** One non-overlapping production retention cycle: report before deletion. */
export const runRetentionCycleV1 = async (
  store: WatcherStore,
  options: RetentionScanOptionsV1 & { readonly alertThresholdMs?: number },
): Promise<RetentionCycleResultV1> => {
  // Use one joined snapshot for both reporting and deletion. Incoming DA writes
  // may run concurrently with the watcher tick; a second scan could otherwise
  // delete a record that was never present in the preceding report.
  const candidates = await retentionCandidatesV1(store, options);
  const deadlines = retentionDeadlineReportFromCandidatesV1(
    candidates,
    options,
  );
  const prune = await pruneRetentionCandidatesV1(store, candidates);
  return { deadlines, prune };
};
