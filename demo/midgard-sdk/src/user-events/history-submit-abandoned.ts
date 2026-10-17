import type {
  EventHistorySubmissionAttempt,
  EventHistorySubmissionCheckpoint,
  EventHistorySubmissionDriver,
} from "./history-submit.js";

/** How long an abandoned attempt is kept: the chain view that settled it
 * changes back only by a rollback, and the deployments' automatic recovery
 * depth of 2160 blocks spans Cardano's stability window, 3k/f = 129,600
 * one-second slots. */
export const EVENT_HISTORY_ABANDONED_RETENTION_MS = 129_600_000;

export type EventHistoryAbandonedAttempt = EventHistorySubmissionAttempt & {
  readonly abandonedAtMs: number;
};

/** The checkpoint's abandoned attempts still within the retention bound, then
 * `attempts`, abandoned at `now`. */
export const abandonAttempts = (
  checkpoint: EventHistorySubmissionCheckpoint,
  attempts: readonly EventHistorySubmissionAttempt[],
  now: number,
): EventHistoryAbandonedAttempt[] => [
  ...(checkpoint.abandoned ?? []).filter(
    (kept) => now - kept.abandonedAtMs < EVENT_HISTORY_ABANDONED_RETENTION_MS,
  ),
  ...attempts.map((attempt) => ({ ...attempt, abandonedAtMs: now })),
];

/** Once the nonce is spent, an abandoned admission that a rollback landed is
 * the receipt. The pending admission spends the same nonce, so it is
 * abandoned in its place, and whichever of them lands wins. Saves and
 * returns true when it adopted one. */
export const adoptLandedAbandonedAdmission = async (
  checkpoint: EventHistorySubmissionCheckpoint,
  driver: Pick<EventHistorySubmissionDriver, "observe" | "now">,
  nonceSpent: () => Promise<boolean>,
  save: (next: EventHistorySubmissionCheckpoint) => Promise<void>,
): Promise<boolean> => {
  const candidates = (checkpoint.abandoned ?? []).filter(
    (attempt) => attempt.phase === "Admission",
  );
  if (
    driver.observe === undefined ||
    candidates.length === 0 ||
    !(await nonceSpent())
  )
    return false;
  for (const { abandonedAtMs: _at, ...attempt } of candidates) {
    if ((await driver.observe(attempt)).kind !== "Confirmed") continue;
    const { pending, ...rest } = checkpoint;
    const abandoned = abandonAttempts(
      checkpoint,
      pending === undefined ? [] : [pending],
      driver.now(),
    );
    await save({
      ...rest,
      admission: attempt,
      abandoned: abandoned.filter((kept) => kept.txHash !== attempt.txHash),
    });
    return true;
  }
  return false;
};

/** A stored publication receipt that reconciled as unable to land: a
 * rollback removed it and its validity has since ended. It is abandoned like
 * any other attempt, so the submission publishes again. */
export const abandonPublicationReceipt = (
  checkpoint: EventHistorySubmissionCheckpoint,
  now: number,
): EventHistorySubmissionCheckpoint => {
  const { publication: _publication, publicationAttempt, ...rest } = checkpoint;
  return {
    ...rest,
    abandoned: abandonAttempts(
      checkpoint,
      publicationAttempt === undefined ? [] : [publicationAttempt],
      now,
    ),
  };
};

/** Before publishing again, an abandoned publication that a rollback landed
 * is the publication. Publications share no input that makes an abandoned
 * one and its replacement mutually exclusive, so once the replacement is
 * built both may land; the checkpoint's publication is the one the admission
 * references. Saves and returns true when it adopted one. */
export const adoptLandedAbandonedPublication = async (
  checkpoint: EventHistorySubmissionCheckpoint,
  driver: Pick<EventHistorySubmissionDriver, "observe">,
  save: (next: EventHistorySubmissionCheckpoint) => Promise<void>,
): Promise<boolean> => {
  if (driver.observe === undefined) return false;
  // After a rollback an abandoned publication can land alongside its
  // replacement. The extra retention output is reclaimable through
  // history-data.ak Reclaim, and nothing is admitted or paid twice.
  for (const { abandonedAtMs: _at, ...attempt } of checkpoint.abandoned ?? []) {
    if (
      attempt.phase !== "Publication" ||
      (await driver.observe(attempt)).kind !== "Confirmed"
    )
      continue;
    await save({
      ...checkpoint,
      publication: { txHash: attempt.txHash, outputIndex: attempt.outputIndex },
      publicationAttempt: attempt,
      abandoned: checkpoint.abandoned!.filter(
        (kept) => kept.txHash !== attempt.txHash,
      ),
    });
    return true;
  }
  return false;
};
