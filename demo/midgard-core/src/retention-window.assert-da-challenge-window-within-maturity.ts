import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import { DA_TRANSPORT_LIMITS } from "./da-transport.js";
import { DEPLOYMENT_PROFILES } from "./generated-deployment-profiles.js";

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
// `requiredRetentionMs` is also the prune horizon of `daRetentionPruneDecision`,
// but that horizon alone does not keep a payload for as long as the pooled DA
// bond can still be slashed over it: on the testing profiles the latest
// response deadline (challenge window plus full response window after the
// block's end time) lies past the horizon. Retention for an availability
// challenge rests on the `live_in_queue` arm instead. A header is live in the
// queue for as long as it can be challenged, provided that
// `da_challenge_window_ms <= block_maturity_ms` in every profile:
//   - an Open must land strictly before `end_time + da_challenge_window_ms`,
//     and a merge no earlier than `end_time + block_maturity_ms`, so every Open
//     lands while the header is still in the queue;
//   - a `Challenged` header never merges, so it stays live until Close
//     publishes the payload on L1 or a timeout removes the header and slashes
//     the pool, after which there is nothing left to answer.
// `assertDaChallengeWindowWithinMaturity` checks that relation for every
// deployment profile at module load, below.
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
 * Fail-closed check that a deployment profile's DA challenge window fits inside
 * its block maturity, so every availability challenge opens while its header
 * is still live in the state queue, where `daRetentionPruneDecision` retains it
 * (see the note above the retention-window assertion). Rejects malformed or
 * non-positive timing before comparing.
 */
export const assertDaChallengeWindowWithinMaturity = (
  profileName: string,
  timing: {
    readonly block_maturity_ms: unknown;
    readonly da_challenge_window_ms: unknown;
  },
): void => {
  const maturityMs = timing.block_maturity_ms;
  const challengeWindowMs = timing.da_challenge_window_ms;
  if (
    typeof maturityMs !== "number" ||
    !Number.isSafeInteger(maturityMs) ||
    maturityMs <= 0 ||
    typeof challengeWindowMs !== "number" ||
    !Number.isSafeInteger(challengeWindowMs) ||
    challengeWindowMs <= 0
  ) {
    throw new Error(
      `Deployment profile ${profileName}: block_maturity_ms and da_challenge_window_ms must be positive safe integers of ms`,
    );
  }
  if (challengeWindowMs > maturityMs) {
    throw new Error(
      `Deployment profile ${profileName}: da_challenge_window_ms=${String(
        challengeWindowMs,
      )} must not exceed block_maturity_ms=${String(
        maturityMs,
      )}, or an availability challenge could open after its header merged and its payload was pruned`,
    );
  }
};

for (const profile of Object.values(DEPLOYMENT_PROFILES)) {
  assertDaChallengeWindowWithinMaturity(profile.name, profile.timing);
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
