import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";

const DAY_IN_MILLIS = RETENTION_MS_PER_DAY;

/**
 * Minimum non-zero `RETENTION_DAYS`, in days.
 *
 * Derived from the canonical V1 retention window (block maturity plus the
 * worst-case proof-time bound plus the deployed margin), never a literal, so
 * the floor cannot drift away from the deployment manifest's
 * `da.transportProfile.retentionDays`. It floors only the wall-clock tables:
 * DA payload availability is not governed by `RETENTION_DAYS` or by the
 * manifest value. A DA payload is kept until its block is past the
 * challengeability horizon or its header is removed from the state queue, and
 * always while it is the L1 confirmed head or live in the L1 state queue.
 */
export const MIN_DA_PAYLOAD_RETENTION_DAYS =
  MIDGARD_RETENTION_WINDOW.retentionDays;

export const validateRetentionDays = (retentionDays: number): number => {
  if (!Number.isSafeInteger(retentionDays) || retentionDays < 0) {
    throw new Error("RETENTION_DAYS must be a non-negative safe integer.");
  }
  if (retentionDays > 0 && retentionDays < MIN_DA_PAYLOAD_RETENTION_DAYS) {
    throw new Error(
      `RETENTION_DAYS must be 0 or at least ${MIN_DA_PAYLOAD_RETENTION_DAYS.toString()} days (the deployment manifest da.transportProfile.retentionDays floor); it governs only the wall-clock tables, never DA payloads.`,
    );
  }
  return retentionDays;
};

/**
 * Binds enabled retention to deployment identity: a node may retain longer than
 * the deployment manifest promises, never shorter. Fails closed at config load.
 */
export const assertRetentionDaysMatchesDeployment = (
  retentionDays: number,
  manifestRetentionDays: number = MIDGARD_RETENTION_WINDOW.retentionDays,
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
    // Wall-clock pruning disabled. DA payloads never follow RETENTION_DAYS:
    // they are pruned on the challengeability horizon and the L1 exemption
    // sets, so the window is covered either way.
    return days;
  }
  if (days < manifestRetentionDays) {
    throw new Error(
      `RETENTION_DAYS=${days.toString()} is shorter than the deployment manifest da.transportProfile.retentionDays=${manifestRetentionDays.toString()}; a wall-clock retention window shorter than the deployment manifest's is refused.`,
    );
  }
  return days;
};

/**
 * Whether the wall-clock retention tables (tx rejections, address history,
 * deposits, withdrawals) are pruned. DA payload pruning is not governed by
 * this switch and always runs.
 */
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
  new Date(now.getTime() - MIDGARD_RETENTION_WINDOW.requiredRetentionMs);
