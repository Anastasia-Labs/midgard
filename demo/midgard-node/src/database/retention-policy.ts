import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core";

const DAY_IN_MILLIS = RETENTION_MS_PER_DAY;

/**
 * The retention window, in days, of a contract bundle derived from the
 * compiled profile rather than loaded from a deployment manifest. Such a
 * bundle's deployment IS the compiled profile, so this is its declared window.
 * A node running a verified manifest never reads it: its housekeeping window
 * is the manifest's `da.transportProfile.retentionDays`
 * (`resolveHousekeepingRetentionDays`). DA payload availability follows
 * neither: a DA payload is kept until its block is past the challengeability
 * horizon or its header is removed from the state queue, and always while it
 * is the L1 confirmed head or live in the L1 state queue.
 */
export const MIN_DA_PAYLOAD_RETENTION_DAYS =
  MIDGARD_RETENTION_WINDOW.retentionDays;

/** Shape check only. The window it is compared with is the deployment's,
 * which config load cannot see: `resolveHousekeepingRetentionDays` decides. */
export const validateRetentionDays = (retentionDays: number): number => {
  if (!Number.isSafeInteger(retentionDays) || retentionDays < 0) {
    throw new Error("RETENTION_DAYS must be a non-negative safe integer.");
  }
  return retentionDays;
};

/**
 * The housekeeping retention window in days; 0 means nothing is pruned.
 *
 * Derived from the verified deployment manifest's declared
 * `da.transportProfile.retentionDays`, never from a compiled constant or an
 * env default:
 *  - unset `RETENTION_DAYS` uses the manifest window;
 *  - an explicit value at or above the manifest window is honoured;
 *  - an explicit value below it is a config fault and throws, so the node
 *    refuses to start rather than prune records the deployment promises;
 *  - an explicit 0 keeps the wall-clock tables forever (longer than any
 *    window), which is always allowed.
 * Without a manifest (a derived contract bundle) an unset value prunes
 * nothing, and an explicit one must cover the compiled profile's window.
 */
export const resolveHousekeepingRetentionDays = ({
  configured,
  manifestRetentionDays,
}: {
  readonly configured: number | undefined;
  readonly manifestRetentionDays: number | undefined;
}): number => {
  if (
    manifestRetentionDays !== undefined &&
    (!Number.isSafeInteger(manifestRetentionDays) || manifestRetentionDays < 1)
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.retentionDays must be a positive safe integer.",
    );
  }
  if (configured === undefined) return manifestRetentionDays ?? 0;
  const days = validateRetentionDays(configured);
  if (days === 0) return 0;
  if (manifestRetentionDays === undefined) {
    if (days < MIN_DA_PAYLOAD_RETENTION_DAYS) {
      throw new Error(
        `RETENTION_DAYS=${days.toString()} is shorter than the derived deployment's retention window of ${MIN_DA_PAYLOAD_RETENTION_DAYS.toString()} days; set it to 0, leave it unset, or raise it to at least ${MIN_DA_PAYLOAD_RETENTION_DAYS.toString()}.`,
      );
    }
    return days;
  }
  if (days < manifestRetentionDays) {
    throw new Error(
      `RETENTION_DAYS=${days.toString()} is shorter than the verified deployment manifest da.transportProfile.retentionDays=${manifestRetentionDays.toString()}; unset it to use the manifest window, set it to 0 to keep records forever, or raise it to at least ${manifestRetentionDays.toString()}.`,
    );
  }
  return days;
};

/**
 * Whether the housekeeping prunes run at all: the wall-clock tables (tx
 * rejections, address history), finalized journals and ended lease rows.
 * Deposit and withdrawal rows are never pruned. DA payload pruning is not
 * governed by this switch and always runs.
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

/**
 * The housekeeping cutoff: the older of the retention window's cutoff and
 * the challengeability cutoff, so no housekeeping prune ever reaches a record
 * still inside the DA challenge horizon, whatever window was configured.
 */
export const computeHousekeepingCutoff = (
  now: Date,
  retentionDays: number,
): Date => {
  const retention = computeRetentionCutoff(now, retentionDays);
  const challengeable = computeChallengeableCutoff(now);
  return retention.getTime() < challengeable.getTime()
    ? retention
    : challengeable;
};
