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
      options.deploymentFingerprint !== undefined &&
      payload.deploymentFingerprint !== options.deploymentFingerprint;
    const decision = daRetentionPruneDecisionV1(
      { headerHash: payload.headerHash, blockEndTimeMs },
      {
        nowMs: options.nowMs,
        retentionDays: options.retentionDays,
        // A missing header row yields an undefined status, which the core
        // decision treats as unknown and therefore retains.
        headerStatus: header?.status,
      },
    );
    return {
      headerHash: payload.headerHash,
      deploymentFingerprint: payload.deploymentFingerprint,
      blockEndTimeMs,
      headerStatus: header?.status,
      headerPresent: header !== undefined,
      fingerprintMismatch,
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

/**
 * Deletes only `expired_and_terminal` retained DA payloads.
 *
 * NOTE (deliberately inert today): the L1 state-queue scanner does not yet emit
 * `merged` or `removed` header statuses, so no record can currently reach the
 * terminal precondition and this pruner deletes nothing in practice. The
 * terminal-status requirement must NOT be loosened to make it active; the
 * scanner must start emitting terminal statuses instead.
 */
export const pruneExpiredDaPayloadsV1 = async (
  store: WatcherStore,
  options: RetentionScanOptionsV1,
): Promise<RetentionPruneResultV1> => {
  const candidates = await retentionCandidatesV1(store, options);
  const prunedHeaderHashes: string[] = [];
  for (const candidate of candidates) {
    if (
      candidate.decision.decision !== "prune" ||
      candidate.decision.reasonCode !== "expired_and_terminal" ||
      candidate.fingerprintMismatch
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

/** Executable deadline report over the retained DA payload set. */
export const retentionDeadlineReportV1 = async (
  store: WatcherStore,
  options: RetentionScanOptionsV1 & { readonly alertThresholdMs?: number },
): Promise<RetentionDeadlineReportV1> => {
  const alertThresholdMs =
    options.alertThresholdMs ?? MIDGARD_RETENTION_WINDOW_V1.marginMs;
  if (!Number.isSafeInteger(alertThresholdMs) || alertThresholdMs < 0) {
    throw new Error("alertThresholdMs must be a non-negative safe integer");
  }
  const candidates = await retentionCandidatesV1(store, options);
  const entries = candidates.map<RetentionDeadlineEntryV1>((candidate) => {
    if (candidate.blockEndTimeMs === null || candidate.fingerprintMismatch) {
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
