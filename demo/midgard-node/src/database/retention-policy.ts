import {
  MIDGARD_RETENTION_WINDOW_V1,
  RETENTION_MS_PER_DAY_V1,
} from "@al-ft/midgard-core";

const DAY_IN_MILLIS = RETENTION_MS_PER_DAY_V1;

/**
 * Minimum enabled DA payload retention, in days.
 *
 * Derived from the canonical V1 retention window (block maturity plus the
 * worst-case proof-time bound plus the deployed margin), never a literal, so
 * the floor cannot drift away from the deployment manifest's
 * `da.transportProfile.retentionDays`.
 */
export const MIN_DA_PAYLOAD_RETENTION_DAYS =
  MIDGARD_RETENTION_WINDOW_V1.retentionDays;

export const validateRetentionDays = (retentionDays: number): number => {
  if (!Number.isSafeInteger(retentionDays) || retentionDays < 0) {
    throw new Error("RETENTION_DAYS must be a non-negative safe integer.");
  }
  if (retentionDays > 0 && retentionDays < MIN_DA_PAYLOAD_RETENTION_DAYS) {
    throw new Error(
      `RETENTION_DAYS must be 0 or at least ${MIN_DA_PAYLOAD_RETENTION_DAYS.toString()} days so DA payloads remain available through the challenge window.`,
    );
  }
  return retentionDays;
};

/**
 * Binds enabled retention to deployment identity: a node may retain longer than
 * the deployment manifest promises, never shorter. Fails closed at config load.
 */
export const assertRetentionDaysMatchesDeploymentV1 = (
  retentionDays: number,
  manifestRetentionDays: number = MIDGARD_RETENTION_WINDOW_V1.retentionDays,
): number => {
  const days = validateRetentionDays(retentionDays);
  if (
    !Number.isSafeInteger(manifestRetentionDays) ||
    manifestRetentionDays < 0
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.retentionDays must be a non-negative safe integer.",
    );
  }
  if (days === 0) {
    // Pruning disabled: nothing is ever removed, so the window is trivially
    // covered.
    return days;
  }
  if (days < manifestRetentionDays) {
    throw new Error(
      `RETENTION_DAYS=${days.toString()} is shorter than the deployment manifest da.transportProfile.retentionDays=${manifestRetentionDays.toString()}; retained evidence would be pruned while still challengeable.`,
    );
  }
  return days;
};

export const shouldPruneRetention = (retentionDays: number): boolean =>
  validateRetentionDays(retentionDays) > 0;

export const computeRetentionCutoff = (
  now: Date,
  retentionDays: number,
): Date => {
  const days = validateRetentionDays(retentionDays);
  return new Date(now.getTime() - days * DAY_IN_MILLIS);
};

/**
 * Cutoff for block END TIME below which a block is no longer challengeable.
 *
 * Derived from block maturity plus the worst-case proof-time BOUND (half
 * maturity). The measured dispute schedule is never used here.
 */
export const computeChallengeableCutoff = (now: Date): Date =>
  new Date(now.getTime() - MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs);
