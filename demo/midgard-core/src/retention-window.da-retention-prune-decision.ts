import {
  MIDGARD_RETENTION_WINDOW,
  requireRetentionDays,
  RETENTION_MS_PER_DAY,
  type RetentionDeadline,
  type RetentionHeaderStatus,
  type RetentionWindow,
} from "./retention-window.assert-da-challenge-window-within-maturity.js";

/**
 * Computes the retention deadline for one block. The deadline is keyed on the
 * block's END TIME (the L2 consensus fact), never on a local insert timestamp -
 * a late or replayed local write must not extend or shorten challengeability.
 */
export const retentionDeadlineForBlock = (args: {
  readonly blockEndTimeMs: unknown;
  readonly retentionDays?: unknown;
}): RetentionDeadline => {
  const blockEndTimeMs = args.blockEndTimeMs;
  if (
    typeof blockEndTimeMs !== "number" ||
    !Number.isSafeInteger(blockEndTimeMs) ||
    blockEndTimeMs < 0
  ) {
    throw new Error(
      "blockEndTimeMs must be a non-negative safe integer of milliseconds",
    );
  }
  const retentionDays =
    args.retentionDays === undefined
      ? MIDGARD_RETENTION_WINDOW.retentionDays
      : requireRetentionDays(args.retentionDays, "retentionDays");
  const deployedRetentionMs = retentionDays * RETENTION_MS_PER_DAY;
  const challengeableUntilMs =
    blockEndTimeMs + MIDGARD_RETENTION_WINDOW.requiredRetentionMs;
  return {
    blockEndTimeMs,
    challengeableUntilMs,
    retainUntilMs: blockEndTimeMs + deployedRetentionMs,
    deployedRetentionMs,
    remainingMs: (nowMs: number): number => challengeableUntilMs - nowMs,
  };
};

/**
 * Where one retained payload's header sits in the store's latest authenticated
 * L1 view: the header hash in the `ConfirmedState` datum, a header node still
 * live in the state queue, or neither. Always sourced from L1, never from local
 * header rows.
 */
export type RetentionQueueReference =
  | "confirmed_head"
  | "live_in_queue"
  | "none";

export type RetentionPruneReasonCode =
  | "confirmed_head_payload"
  | "live_queue_header"
  | "terminal_recovery_pending"
  | "removed_header"
  | "past_challengeability_horizon"
  | "still_challengeable";

export type RetentionPruneDecision = {
  readonly decision: "retain" | "prune";
  readonly reasonCode: RetentionPruneReasonCode;
  readonly challengeableUntilMs: number;
  readonly retainUntilMs: number;
  readonly remainingMs: number;
};

export type RetentionPruneInput = {
  readonly nowMs: number;
  /** End time of the payload's own block header. */
  readonly blockEndTimeMs: number;
  /** Stored header status; `unobserved` when the store has no header row. */
  readonly headerStatus: RetentionHeaderStatus | "unobserved";
  readonly queueReference: RetentionQueueReference;
  /** Authenticated terminal history is beyond the signed L1 recovery horizon. */
  readonly terminalRecoveryFinal?: boolean;
  readonly window?: RetentionWindow;
  readonly retentionDays?: number;
};

/**
 * Single authority on whether one retained DA payload may be pruned.
 *
 * Terminal headers remain retained until authenticated history is deeper than
 * the deployment's automatic recovery horizon. That hold also precedes the
 * wall-clock horizon: a provisional removal or merge can still be rolled back.
 * After retirement, removed headers can be pruned immediately and merged
 * headers once challengeability has passed. The L1 head and live queue remain
 * exempt throughout.
 */
export const daRetentionPruneDecision = (
  input: RetentionPruneInput,
): RetentionPruneDecision => {
  const window = input.window ?? MIDGARD_RETENTION_WINDOW;
  if (!Number.isSafeInteger(input.nowMs)) {
    throw new Error("nowMs must be a safe integer of milliseconds");
  }
  if (!Number.isSafeInteger(input.blockEndTimeMs) || input.blockEndTimeMs < 0) {
    throw new Error(
      "blockEndTimeMs must be a non-negative safe integer of milliseconds",
    );
  }
  const retentionDays = input.retentionDays ?? window.retentionDays;
  const challengeableUntilMs =
    input.blockEndTimeMs + window.requiredRetentionMs;
  const base = {
    challengeableUntilMs,
    retainUntilMs: input.blockEndTimeMs + retentionDays * RETENTION_MS_PER_DAY,
    remainingMs: challengeableUntilMs - input.nowMs,
  };
  if (input.queueReference === "confirmed_head") {
    return {
      decision: "retain",
      reasonCode: "confirmed_head_payload",
      ...base,
    };
  }
  if (input.queueReference === "live_in_queue") {
    return { decision: "retain", reasonCode: "live_queue_header", ...base };
  }
  if (
    (input.headerStatus === "removed" || input.headerStatus === "merged") &&
    input.terminalRecoveryFinal !== true
  ) {
    return {
      decision: "retain",
      reasonCode: "terminal_recovery_pending",
      ...base,
    };
  }
  if (input.headerStatus === "removed") {
    return { decision: "prune", reasonCode: "removed_header", ...base };
  }
  if (input.nowMs > challengeableUntilMs) {
    return {
      decision: "prune",
      reasonCode: "past_challengeability_horizon",
      ...base,
    };
  }
  return { decision: "retain", reasonCode: "still_challengeable", ...base };
};

/**
 * Validates the L1-view staleness deadline after which a store that cannot
 * obtain a fresh authenticated L1 view exits instead of running on a stale
 * one. The deadline must tolerate at least three missed passes, so one bad
 * poll is never fatal, and must not exceed the deployed retention margin, so
 * the deployed window is never silently breached while the process waits.
 */
export const resolveL1ViewFatalMs = (args: {
  readonly value: unknown;
  readonly defaultMs: number;
  readonly pollIntervalMs: number;
  readonly fieldName: string;
}): number => {
  const raw = args.value === undefined ? args.defaultMs : args.value;
  const value =
    typeof raw === "string" && /^\d+$/u.test(raw) ? Number(raw) : raw;
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `${args.fieldName} must be a non-negative safe integer of ms`,
    );
  }
  if (value < 3 * args.pollIntervalMs) {
    throw new Error(
      `${args.fieldName}=${String(value)} must be at least three poll intervals (${String(
        3 * args.pollIntervalMs,
      )} ms)`,
    );
  }
  if (value > MIDGARD_RETENTION_WINDOW.marginMs) {
    throw new Error(
      `${args.fieldName}=${String(value)} must not exceed the deployed retention margin ${String(
        MIDGARD_RETENTION_WINDOW.marginMs,
      )} ms`,
    );
  }
  return value;
};

/**
 * The most time a merged, no-longer-head payload can have left before its
 * challengeability deadline: the horizon minus block maturity. A header leaves
 * the state queue only by merging, which the state-queue validator allows no
 * earlier than block maturity after the header's end time, so once a payload
 * is still challengeable outside the queue it has at most this long left.
 */
export const MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS =
  MIDGARD_RETENTION_WINDOW.requiredRetentionMs -
  MIDGARD_RETENTION_WINDOW.maturityMs;

/**
 * Validates an operator's opt-in retention deadline alert threshold (the node's
 * `--alert-threshold-ms`, the committee's `DA_RETENTION_ALERT_THRESHOLD_MS`).
 * It must be a non-negative safe integer strictly below
 * `MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS`: a threshold at or above it alerts
 * on every merged payload for the whole of its retained life after the merge,
 * so it can never single one out. Configuration refuses that.
 *
 * A threshold below the bound still alerts on every merged payload during its
 * last `threshold` ms, because every such payload ages to its deadline on its
 * way to pruning. The alert is therefore information about pruning that is
 * coming, not a risk signal, and no readiness surface depends on it.
 */
export const requireRetentionAlertThresholdMs = (
  value: unknown,
  fieldName: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${fieldName} must be a non-negative safe integer of ms`);
  }
  if (value >= MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS) {
    throw new Error(
      `${fieldName}=${String(value)} must be below the merged-payload window ${String(
        MIDGARD_MERGED_PAYLOAD_MAX_REMAINING_MS,
      )} ms (challengeability horizon minus block maturity): a threshold at or above it alerts on every merged payload from the moment it merges`,
    );
  }
  return value;
};

export type RetentionDeadlineAlert = {
  readonly headerHash?: string;
  readonly challengeableUntilMs: number;
  readonly remainingMs: number;
  readonly headroomMs: number;
  readonly alerting: boolean;
};

/**
 * Executable deadline alert primitive: alerts when a record's remaining time to
 * its challengeability deadline is at or below `alertThresholdMs`.
 *
 * The threshold has no default. Pruning never removes still-challengeable
 * evidence, and every retained record ages towards its deadline on its normal
 * way to pruning, so an alerting record is not a fault: the alert is
 * informational and exists only for an operator who chose a threshold. (The derived margin is no
 * usable default: under the testing profiles it exceeds the whole horizon, and
 * on mainnet it exceeds what every merged non-head block has left, so it would
 * alert on every still-challengeable record.)
 */
export const retentionDeadlineAlert = (args: {
  readonly nowMs: number;
  readonly blockEndTimeMs: number;
  readonly retentionDays?: number;
  readonly alertThresholdMs: number;
  readonly headerHash?: string;
}): RetentionDeadlineAlert => {
  const { alertThresholdMs } = args;
  if (!Number.isSafeInteger(alertThresholdMs) || alertThresholdMs < 0) {
    throw new Error("alertThresholdMs must be a non-negative safe integer");
  }
  const deadline = retentionDeadlineForBlock({
    blockEndTimeMs: args.blockEndTimeMs,
    retentionDays: args.retentionDays,
  });
  const remainingMs = deadline.remainingMs(args.nowMs);
  const headroomMs = remainingMs - alertThresholdMs;
  return {
    headerHash: args.headerHash,
    challengeableUntilMs: deadline.challengeableUntilMs,
    remainingMs,
    headroomMs,
    alerting: headroomMs <= 0,
  };
};
