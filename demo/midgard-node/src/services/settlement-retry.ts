/**
 * How the settlement worker retries a failed job (#821). Each consecutive
 * failure doubles the wait. A job whose consecutive failures reach
 * `SETTLEMENT_JOB_FAILURE_CAP` is stuck: `/readyz` names it, and it is
 * retried only once per `SETTLEMENT_STUCK_RETRY_MS`, so it never crowds out
 * the node's other settlement jobs, and an outage longer than the backoff
 * still clears without an operator. A success resets the count. Block
 * production never reads the settlement journal.
 */

/** Consecutive failures that make a job stuck: seven retries over about
 * 11 minutes of backoff (5 s doubling to 320 s). */
export const SETTLEMENT_JOB_FAILURE_CAP = 8;
export const SETTLEMENT_JOB_BASE_BACKOFF_MS = 5_000;
export const SETTLEMENT_STUCK_RETRY_MS = 60 * 60_000;

/** The wait before a job's next attempt, after a failure that follows
 * `failures` consecutive failed attempts. */
export const settlementRetryDelayMs = (failures: number): number =>
  failures + 1 >= SETTLEMENT_JOB_FAILURE_CAP
    ? SETTLEMENT_STUCK_RETRY_MS
    : SETTLEMENT_JOB_BASE_BACKOFF_MS * 2 ** failures;

/**
 * The worker's backoff for the reconcile of the attempt that blocks new work:
 * a failed status read waits on the same schedule as a failed job, but never
 * adds to any job's `failures`, because the read says nothing about the job.
 * It lives for the worker run and restarts when another attempt blocks.
 */
export type SettlementReconcileBackoff = {
  txHash?: string;
  failures: number;
  dueAt: number;
  lastError?: string;
};

export const settlementReconcileBackoff = (): SettlementReconcileBackoff => ({
  failures: 0,
  dueAt: 0,
});

/** The wait before the next reconcile of `txHash`, or undefined when due. */
export const settlementReconcileWait = (
  backoff: SettlementReconcileBackoff,
  txHash: string,
  now: number,
): number | undefined => {
  if (backoff.txHash !== txHash) {
    backoff.txHash = txHash;
    backoff.failures = 0;
    backoff.dueAt = 0;
    backoff.lastError = undefined;
  }
  return now < backoff.dueAt ? backoff.dueAt : undefined;
};

export const recordSettlementReconcileFailure = (
  backoff: SettlementReconcileBackoff,
  now: number,
  error: string,
): void => {
  backoff.dueAt = now + settlementRetryDelayMs(backoff.failures);
  backoff.failures += 1;
  backoff.lastError = error;
};

export const clearSettlementReconcileFailures = (
  backoff: SettlementReconcileBackoff,
): void => {
  backoff.failures = 0;
  backoff.dueAt = 0;
  backoff.lastError = undefined;
};
