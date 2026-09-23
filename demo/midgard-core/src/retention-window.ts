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

import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import { DA_TRANSPORT_LIMITS } from "./da-transport.js";

/** Milliseconds in one calendar-independent 24h day. */
export const RETENTION_MS_PER_DAY = 24 * 60 * 60 * 1000;

/**
 * Closed set of L1 state-queue header statuses a store can record. Only
 * `removed` moves the retention decision; every other status is decided by the
 * queue references and the challengeability horizon alone.
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
//
// `requiredRetentionMs` is also the prune horizon of `daRetentionPruneDecision`.
// When the time-based availability-challenge bond reclaim lands, a payload must
// stay retained for as long as an availability challenge can be opened and
// answered, so this assertion must then also check
// `requiredRetentionMs >= daChallengeWindowMs + responseDeadlineMs`. That
// plan's own relation (challenge window <= block maturity) already implies it.
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
  readonly window?: RetentionWindow;
  readonly retentionDays?: number;
};

/**
 * Single authority on whether one retained DA payload may be pruned.
 *
 * A payload is prunable when its header was removed from the state queue OR
 * its challengeability horizon has strictly passed, unless it is the L1
 * confirmed head's payload or its header is still live in the L1 state queue.
 * There is no other arm: every input is required, so no caller can reach a
 * "keep everything" outcome by leaving one out. The retained set of any store
 * is therefore a subset of {confirmed head} + {live queue headers} + {payloads
 * whose block ended within the horizon}.
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
