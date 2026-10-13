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
