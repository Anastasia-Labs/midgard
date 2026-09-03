/**
 * Canonical V1 retention window (GOAL_SPEC 9.4 / Q54).
 *
 * Retained DA and proof evidence must survive the full challenge surface of a
 * block: block maturity plus the worst-case correction (proof) time, plus an
 * operational margin. Every number in this module is derived from the frozen
 * consensus and DA transport profiles - none of them may be re-stated as a
 * literal, so a profile change propagates instead of silently drifting.
 *
 * Authoritative economics: docs/midgard/decisions/0002-canonical-v1-goal-
 * economics-and-margins.md (maturity 604_800_000 ms; worst-case proof-time
 * bound = half maturity = 302_400_000 ms per 3.3 clause 3; RETENTION_DAYS 15).
 *
 * Enforcement is always against the half-maturity BOUND. The measured dispute
 * schedule (`measuredValidationDisputeScheduleMs`, ~11h) is recorded here for
 * observability only and must never be used as the retention floor.
 */

import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile-v1.js";
import { DA_TRANSPORT_LIMITS } from "./da-transport.js";

/** Milliseconds in one calendar-independent 24h day. */
export const RETENTION_MS_PER_DAY = 24 * 60 * 60 * 1000;

/**
 * Terminal L1 state-queue header statuses. Only a header that has reached a
 * terminal on-chain outcome can make its retained evidence prunable; anything
 * else (including an unrecognised status) is still challengeable.
 */
export const RETENTION_TERMINAL_HEADER_STATUSES = Object.freeze([
  "merged",
  "removed",
] as const);

/**
 * Closed set of header statuses the retention decision understands. A status
 * outside this set is treated as unknown and therefore retained.
 */
export const RETENTION_KNOWN_HEADER_STATUSES = Object.freeze([
  "unattested",
  "attesting",
  "attested",
  "merged",
  "removed",
  "conflicted",
] as const);

export type RetentionHeaderStatus =
  (typeof RETENTION_KNOWN_HEADER_STATUSES)[number];

export type RetentionWindow = {
  /** Block maturity, derived from the consensus profile. */
  readonly maturityMs: number;
  /**
   * Worst-case correction/proof-time BOUND: half of maturity. Enforcement uses
   * this bound, never a measured schedule.
   */
  readonly worstCaseProofTimeBoundMs: number;
  /** maturity + worst-case proof-time bound: the still-challengeable horizon. */
  readonly requiredRetentionMs: number;
  /** Deployed retention, in whole days, from the DA transport profile. */
  readonly retentionDays: number;
  /** Deployed retention expressed in milliseconds. */
  readonly deployedRetentionMs: number;
  /** Operational headroom: deployed minus required. */
  readonly marginMs: number;
  /**
   * Measured worst-case validation dispute schedule (opening, every bisection
   * move, settlement). Recorded for alerting/observability only.
   */
  readonly measuredValidationDisputeScheduleMs: number;
};

const deriveRetentionWindow = (): RetentionWindow => {
  const maturityMs = MIDGARD_CONSENSUS_LIMITS.blockMaturityMs;
  // GOAL_SPEC 3.3 clause 3: the complete correction path must fit inside the
  // first half of maturity, so half maturity is the worst-case bound.
  const worstCaseProofTimeBoundMs = maturityMs / 2;
  const requiredRetentionMs = maturityMs + worstCaseProofTimeBoundMs;
  const retentionDays = DA_TRANSPORT_LIMITS.minimumRetentionDays;
  const deployedRetentionMs = retentionDays * RETENTION_MS_PER_DAY;
  return Object.freeze({
    maturityMs,
    worstCaseProofTimeBoundMs,
    requiredRetentionMs,
    retentionDays,
    deployedRetentionMs,
    marginMs: deployedRetentionMs - requiredRetentionMs,
    measuredValidationDisputeScheduleMs:
      MIDGARD_CONSENSUS_LIMITS.minValidationDisputeMaturityMs,
  });
};

/** The single derived canonical V1 retention window. */
export const MIDGARD_RETENTION_WINDOW: RetentionWindow =
  deriveRetentionWindow();

// Module-load fail-closed assertion: the shipped profile pair must already
// cover the still-challengeable horizon. If a future profile edit breaks this,
// every importer fails at load rather than silently pruning live evidence.
if (
  !Number.isSafeInteger(MIDGARD_RETENTION_WINDOW.deployedRetentionMs) ||
  !Number.isSafeInteger(MIDGARD_RETENTION_WINDOW.requiredRetentionMs) ||
  MIDGARD_RETENTION_WINDOW.deployedRetentionMs <
    MIDGARD_RETENTION_WINDOW.requiredRetentionMs
) {
  throw new Error(
    `Canonical V1 retention window is under-provisioned: deployedRetentionMs=${String(
      MIDGARD_RETENTION_WINDOW.deployedRetentionMs,
    )} must be >= requiredRetentionMs=${String(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    )}`,
  );
}

/**
 * Minimum whole days of retention that cover the still-challengeable horizon.
 * Derived, so the node/committee floors cannot be lowered independently.
 */
export const MIDGARD_MIN_RETENTION_DAYS = Math.ceil(
  MIDGARD_RETENTION_WINDOW.requiredRetentionMs / RETENTION_MS_PER_DAY,
);

/**
 * Validates a retention-days value from configuration or a manifest. Rejects
 * every malformed shape (NaN, negative, fractional, string, null, unsafe
 * integer) before any comparison is attempted.
 */
export const requireRetentionDays = (
  value: unknown,
  fieldName: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `${fieldName} must be a non-negative safe integer number of days`,
    );
  }
  return value;
};

/** True when `retentionDays` whole days cover the still-challengeable horizon. */
export const retentionDaysCoverWindow = (
  value: unknown,
  fieldName = "retentionDays",
): boolean =>
  requireRetentionDays(value, fieldName) * RETENTION_MS_PER_DAY >=
  MIDGARD_RETENTION_WINDOW.requiredRetentionMs;

/**
 * Fail-closed retention-days floor check used by node and committee config
 * loading.
 */
export const assertRetentionDaysCoverWindow = (
  value: unknown,
  fieldName = "retentionDays",
): number => {
  const retentionDays = requireRetentionDays(value, fieldName);
  if (!retentionDaysCoverWindow(retentionDays, fieldName)) {
    throw new Error(
      `${fieldName} must be at least ${String(
        MIDGARD_MIN_RETENTION_DAYS,
      )} days so retained evidence survives block maturity (${String(
        MIDGARD_RETENTION_WINDOW.maturityMs,
      )} ms) plus the worst-case proof-time bound (${String(
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      )} ms)`,
    );
  }
  return retentionDays;
};

/**
 * Enforcement of GOAL_SPEC 3.3 clause 3: an observed or configured worst-case
 * correction path must fit inside the half-maturity bound.
 */
export const assertWorstCaseProofTimeWithinBound = (
  observedMs: unknown,
  fieldName = "worstCaseProofTimeMs",
): number => {
  if (
    typeof observedMs !== "number" ||
    !Number.isSafeInteger(observedMs) ||
    observedMs < 0
  ) {
    throw new Error(`${fieldName} must be a non-negative safe integer of ms`);
  }
  if (observedMs > MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs) {
    throw new Error(
      `${fieldName}=${String(observedMs)} exceeds the canonical V1 worst-case proof-time bound ${String(
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      )} ms`,
    );
  }
  return observedMs;
};

const asRecord = (value: unknown): Record<string, unknown> | undefined =>
  typeof value === "object" && value !== null && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : undefined;

/**
 * Binds the retention window to deployment identity: the deployment manifest's
 * `da.transportProfile.retentionDays` must itself cover the still-challengeable
 * horizon. Fails closed on any missing or malformed path segment.
 */
export const assertRetentionWindowCoversDeployment = (
  manifest: unknown,
): number => {
  const root = asRecord(manifest);
  if (root === undefined) {
    throw new Error("Deployment manifest must be an object");
  }
  const da = asRecord(root.da);
  if (da === undefined) {
    throw new Error("Deployment manifest da must be an object");
  }
  const transportProfile = asRecord(da.transportProfile);
  if (transportProfile === undefined) {
    throw new Error(
      "Deployment manifest da.transportProfile must be an object",
    );
  }
  return assertRetentionDaysCoverWindow(
    transportProfile.retentionDays,
    "Deployment manifest da.transportProfile.retentionDays",
  );
};

export type RetentionDeadline = {
  /** Block end time the deadline is keyed on. */
  readonly blockEndTimeMs: number;
  /** Last instant the block's evidence is still challengeable. */
  readonly challengeableUntilMs: number;
  /** Last instant the deployed retention window promises the evidence. */
  readonly retainUntilMs: number;
  /** Deployed retention window used for this block, in ms. */
  readonly deployedRetentionMs: number;
  /** Milliseconds left before the challengeability deadline (may be negative). */
  readonly remainingMs: (nowMs: number) => number;
};

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

export type RetentionPruneReasonCode =
  | "active_availability_challenge"
  | "availability_challenge_state_unknown"
  | "still_within_maturity"
  | "still_within_retention_window"
  | "header_status_not_terminal"
  | "header_status_unknown"
  | "missing_block_end_time"
  | "expired_and_terminal";

export type RetentionPruneDecision = {
  readonly decision: "retain" | "prune";
  readonly reasonCode: RetentionPruneReasonCode;
  readonly challengeableUntilMs?: number;
  readonly retainUntilMs?: number;
  readonly remainingMs?: number;
};

export type RetentionPruneRecord = {
  readonly headerHash?: string;
  readonly blockEndTimeMs?: number | null;
};

/**
 * Single authority on whether one retained DA record may be pruned.
 *
 * Fail-closed by construction: a missing block end time, an unknown header
 * status, or any not-yet-terminal header keeps the record. Only a block whose
 * challengeability horizon has strictly passed AND whose L1 header reached a
 * terminal outcome is prunable.
 */
export const daRetentionPruneDecision = (
  record: RetentionPruneRecord,
  options: {
    readonly nowMs: number;
    readonly window?: RetentionWindow;
    readonly retentionDays?: number;
    readonly headerStatus?: unknown;
    /** Only an authenticated `inactive` observation permits pruning. */
    readonly availabilityChallengeState?: unknown;
  },
): RetentionPruneDecision => {
  const window = options.window ?? MIDGARD_RETENTION_WINDOW;
  if (options.availabilityChallengeState === "active") {
    return {
      decision: "retain",
      reasonCode: "active_availability_challenge",
    };
  }
  if (options.availabilityChallengeState !== "inactive") {
    return {
      decision: "retain",
      reasonCode: "availability_challenge_state_unknown",
    };
  }
  const blockEndTimeMs = record.blockEndTimeMs;
  if (
    blockEndTimeMs === undefined ||
    blockEndTimeMs === null ||
    typeof blockEndTimeMs !== "number" ||
    !Number.isSafeInteger(blockEndTimeMs) ||
    blockEndTimeMs < 0
  ) {
    return { decision: "retain", reasonCode: "missing_block_end_time" };
  }

  const status = options.headerStatus;
  const knownStatus =
    typeof status === "string" &&
    (RETENTION_KNOWN_HEADER_STATUSES as readonly string[]).includes(status)
      ? (status as RetentionHeaderStatus)
      : undefined;
  if (knownStatus === undefined) {
    return { decision: "retain", reasonCode: "header_status_unknown" };
  }

  const retentionDays = options.retentionDays ?? window.retentionDays;
  const challengeableUntilMs = blockEndTimeMs + window.requiredRetentionMs;
  const retainUntilMs = blockEndTimeMs + retentionDays * RETENTION_MS_PER_DAY;
  const remainingMs = challengeableUntilMs - options.nowMs;
  const base = { challengeableUntilMs, retainUntilMs, remainingMs };

  if (options.nowMs <= blockEndTimeMs + window.maturityMs) {
    return { decision: "retain", reasonCode: "still_within_maturity", ...base };
  }
  if (options.nowMs <= challengeableUntilMs) {
    return {
      decision: "retain",
      reasonCode: "still_within_retention_window",
      ...base,
    };
  }
  if (
    !(RETENTION_TERMINAL_HEADER_STATUSES as readonly string[]).includes(
      knownStatus,
    )
  ) {
    return {
      decision: "retain",
      reasonCode: "header_status_not_terminal",
      ...base,
    };
  }
  return { decision: "prune", reasonCode: "expired_and_terminal", ...base };
};

export type RetentionDeadlineAlert = {
  readonly headerHash?: string;
  readonly challengeableUntilMs: number;
  readonly remainingMs: number;
  readonly headroomMs: number;
  readonly alerting: boolean;
};

/**
 * Executable deadline alert primitive. `alertThresholdMs` defaults to the
 * derived operational margin, so the alert fires exactly when a still
 * challengeable record has burned through its entire headroom.
 */
export const retentionDeadlineAlert = (args: {
  readonly nowMs: number;
  readonly blockEndTimeMs: number;
  readonly retentionDays?: number;
  readonly alertThresholdMs?: number;
  readonly headerHash?: string;
}): RetentionDeadlineAlert => {
  const alertThresholdMs =
    args.alertThresholdMs ?? MIDGARD_RETENTION_WINDOW.marginMs;
  if (
    !Number.isSafeInteger(alertThresholdMs) ||
    (alertThresholdMs as number) < 0
  ) {
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
