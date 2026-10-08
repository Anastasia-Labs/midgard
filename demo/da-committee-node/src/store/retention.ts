import {
  daRetentionPruneDecision,
  MIDGARD_RETENTION_WINDOW,
  retentionDeadlineAlert,
  type RetentionPruneDecision,
  type RetentionQueueReference,
} from "@al-ft/midgard-core";
import { isFinal } from "@al-ft/midgard-l1-follower";

import type { CommitteeConfig } from "../config.js";
import type {
  DaStoredPayloadRecord,
  StateQueueHeaderRecord,
} from "../domain.js";
import type { CommitteeStore } from "../store.js";

/**
 * Retention enforcement for the committee node store (GOAL_SPEC 9.4 / Q54).
 *
 * Releasing a payload cannot be undone, so it is decided only on what is
 * final (deeper than k, plan §9): a terminal record counts only when its
 * authenticated exit is final, and the challengeability horizon is measured
 * by the release clock, the start of the latest final block's slot, never by
 * the wall clock. A commit's validity ends at its header's `endTime`, so once
 * a final block is past a header's horizon every commit of that header that
 * can ever land is final: in the queue at that block (held below), or final
 * out of it. Missing authority retains bytes and is exposed in retention
 * readiness; every scan retries it. Waiting for finality is normal and is
 * never a readiness reason. The confirmed head, live queue and the queue at
 * the latest final block remain held as well.
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
  /**
   * The release clock: the start time of the latest final block's slot, or
   * null while no block is final (nothing is then past its horizon).
   */
  readonly finalBlockTimeMs: number | null;
  readonly recoveryProofUnavailable?: boolean;
};

/** What one retained payload's release is decided on. */
export type RetainedPayloadReleaseOptions = RetentionL1View & {
  readonly retentionDays?: number;
  /** Reported as a diagnostic when a payload or header does not match it. */
  readonly deploymentFingerprint?: string;
  /** k: a terminal record releases only when its exit is deeper. */
  readonly automaticRecoveryMaxDepth?: number;
};

export type RetentionScanOptions = RetainedPayloadReleaseOptions & {
  /** Wall-clock time of the scan: reporting and the deadline alert only. */
  readonly nowMs: number;
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

/**
 * Headers whose payloads stay retained although L1 at its tip no longer lists
 * them: every header a not-yet-final checkpoint moved or took out of the
 * queue, and the newest header whose merge is final. A reader at release
 * finality still sees each as queued, or as the confirmed head its queue
 * extends. Each is released once a later merge is final at the deployment's
 * finality depth, the only depth either source is judged at.
 */
export const finalityHeldHeaderHashes = (
  deferredHeaderHashes: readonly string[],
  headers: readonly StateQueueHeaderRecord[],
): readonly string[] => {
  const finalMerges = headers.filter(
    ({ status, finalized, observedChainPoint }) =>
      status === "merged" &&
      finalized &&
      observedChainPoint.providerSource ===
        "authenticated_state_queue_transition_v1" &&
      typeof observedChainPoint.blockHeight === "number",
  );
  // Paired retirement pins this whole latest merged native block; find its
  // retained boundary without spreading every record into call arguments.
  const newestBlock = finalMerges.reduce(
    (newest, { observedChainPoint }) =>
      Math.max(newest, observedChainPoint.blockHeight!),
    Number.NEGATIVE_INFINITY,
  );
  return [
    ...new Set([
      ...deferredHeaderHashes,
      // Merges sharing the newest block are kept together: block order alone
      // cannot tell which of them is last.
      ...finalMerges
        .filter(
          ({ observedChainPoint }) =>
            observedChainPoint.blockHeight === newestBlock,
        )
        .map(({ headerHash }) => headerHash),
    ]),
  ];
};

const isTerminalStatus = (status: StateQueueHeaderRecord["status"]): boolean =>
  status === "merged" || status === "removed";

/**
 * A terminal record that names its exit on the authenticated state-queue
 * transition source, at any depth: the record's authority, not its finality.
 */
export const hasAuthenticatedTerminalHistory = (
  header: StateQueueHeaderRecord | undefined,
): boolean => {
  if (header === undefined || !isTerminalStatus(header.status)) {
    return false;
  }
  const point = header.observedChainPoint;
  return (
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
    point.depth >= 1 &&
    header.computedHeaderHash === header.headerHash &&
    header.validationErrors.length === 0
  );
};

/** True only for an authenticated terminal record whose exit is final (deeper than k). */
export const terminalRecoveryFinal = (
  header: StateQueueHeaderRecord | undefined,
  options: Pick<
    RetentionScanOptions,
    "automaticRecoveryMaxDepth" | "deploymentFingerprint"
  >,
  payloadDeploymentFingerprint: string,
): boolean => {
  const securityParameter = options.automaticRecoveryMaxDepth;
  const exitDepth = header?.observedChainPoint.depth;
  return (
    securityParameter !== undefined &&
    Number.isSafeInteger(securityParameter) &&
    securityParameter >= 0 &&
    header?.deploymentFingerprint === payloadDeploymentFingerprint &&
    (options.deploymentFingerprint === undefined ||
      header.deploymentFingerprint === options.deploymentFingerprint) &&
    hasAuthenticatedTerminalHistory(header) &&
    typeof exitDepth === "number" &&
    isFinal(exitDepth, { securityParameter })
  );
};

/**
 * The one release decision for a retained payload, shared by the scan and
 * the store's locked re-decision: the core retention decision, measured by
 * the release clock. Before any block is final the clock reads as the epoch.
 */
export const retainedPayloadPruneDecision = (
  payload: Pick<
    DaStoredPayloadRecord,
    "headerHash" | "fetchedAt" | "deploymentFingerprint"
  >,
  header: StateQueueHeaderRecord | undefined,
  options: RetainedPayloadReleaseOptions,
): RetentionPruneDecision =>
  daRetentionPruneDecision({
    nowMs: options.finalBlockTimeMs ?? 0,
    blockEndTimeMs: retentionBlockEndTimeMs(payload, header),
    headerStatus: header?.status ?? "unobserved",
    queueReference: retentionQueueReference(payload.headerHash, options),
    retentionDays: options.retentionDays,
    terminalRecoveryFinal: terminalRecoveryFinal(
      header,
      options,
      payload.deploymentFingerprint,
    ),
  });

/**
 * Joins retained DA payloads to their state-queue headers and applies the core
 * retention decision to each pair.
 */
export const retentionCandidates = async (
  store: CommitteeStore,
  options: RetentionScanOptions,
): Promise<readonly RetentionCandidate[]> => {
  const payloads = await store.listDaPayloads();
  const headers = await store.getStateQueueHeaders(
    payloads.map(({ headerHash }) => headerHash),
  );
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
      !hasAuthenticatedTerminalHistory(header);
    return {
      headerHash: payload.headerHash,
      deploymentFingerprint: payload.deploymentFingerprint,
      blockEndTimeMs,
      headerStatus,
      queueReference,
      fingerprintMismatch,
      terminalHistoryAuthorityMismatch,
      decision: retainedPayloadPruneDecision(payload, header, options),
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
      finalBlockTimeMs: options.finalBlockTimeMs,
      retentionDays: options.retentionDays,
      confirmedHeadHash: options.confirmedHeadHash,
      liveQueueHeaderHashes: options.liveQueueHeaderHashes,
      automaticRecoveryMaxDepth: options.automaticRecoveryMaxDepth,
      deploymentFingerprint: options.deploymentFingerprint,
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
  /** Missing terminal authority retains bytes and requires another authenticated scan. */
  readonly recoveryProofUnavailable?: number;
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
    | "retentionAlertThresholdMs"
    | "deploymentFingerprint"
    | "automaticRecoveryMaxDepth"
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
  automaticRecoveryMaxDepth: config.automaticRecoveryMaxDepth,
  confirmedHeadHash: view.confirmedHeadHash,
  liveQueueHeaderHashes: view.liveQueueHeaderHashes,
  finalBlockTimeMs: view.finalBlockTimeMs,
  recoveryProofUnavailable: view.recoveryProofUnavailable,
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
    // Reported on the wall clock; the decision itself ran on the release
    // clock, which trails it by about k blocks.
    const base = {
      headerHash: candidate.headerHash,
      reasonCode: candidate.decision.reasonCode,
      challengeableUntilMs: candidate.decision.challengeableUntilMs,
      remainingMs: candidate.decision.challengeableUntilMs - options.nowMs,
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
      // A payload past its horizon on the wall clock is only waiting for
      // the release clock: no longer challengeable, nothing to alert on.
      alerting:
        candidate.decision.reasonCode === "still_challengeable" &&
        alert.remainingMs >= 0 &&
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
    ...(() => {
      const missing = candidates.filter(
        (candidate) =>
          candidate.decision.reasonCode === "terminal_recovery_pending" &&
          (options.recoveryProofUnavailable === true ||
            candidate.terminalHistoryAuthorityMismatch ||
            candidate.fingerprintMismatch ||
            options.automaticRecoveryMaxDepth === undefined),
      ).length;
      return missing === 0 ? {} : { recoveryProofUnavailable: missing };
    })(),
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
