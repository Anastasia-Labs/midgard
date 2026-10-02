import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import type { WatcherAvailabilityStatus } from "./runtime.release-watcher-availability-workflows.js";

/**
 * Consecutive failed reconciliations, all of them L1 transients, reported as
 * `waiting` before the report turns `blocked`. A real refusal is `blocked` at
 * once. Either way the reconciliation is retried, and `blocked` clears with
 * the first one that completes.
 */
export const WATCHER_AVAILABILITY_TRANSIENT_FAILURES_BEFORE_BLOCKED = 8;

/** Retry spacing after a failed reconciliation: 1 s doubling to 60 s. */
export const watcherAvailabilityRetryDelayMs = (consecutive: number): number =>
  Math.min(60_000, 1_000 * 2 ** Math.min(Math.max(consecutive - 1, 0), 6));

/** The concise cause of a report, never an embedded transaction payload. */
export const watcherAvailabilityStatusDetail = (detail: string): string =>
  detail.replace(/[a-fA-F0-9]{128,}/g, "[hex omitted]").slice(0, 2048);

/**
 * Retries a failed reconciliation of the same observation on its own timer,
 * so a quiet queue (no block touches it, so nothing calls `reconcile`) cannot
 * keep a failure that has already passed. Any newer reconciliation request
 * supersedes the timer; it settles its own failure.
 */
export const createWatcherAvailabilityReconcileRetry = (
  timers: Readonly<{
    delayMs?: (consecutive: number) => number;
    setTimer?: (run: () => void, ms: number) => unknown;
    clearTimer?: (timer: unknown) => void;
  }> = {},
) => {
  const delayMs = timers.delayMs ?? watcherAvailabilityRetryDelayMs;
  const setTimer =
    timers.setTimer ??
    ((run: () => void, ms: number) => setTimeout(run, ms).unref());
  const clearTimer =
    timers.clearTimer ??
    ((timer: unknown) => clearTimeout(timer as NodeJS.Timeout));
  let requests = 0;
  let consecutive = 0;
  let transientOnly = true;
  let lastFailed = false;
  let timer: unknown;
  const cancel = (): void => {
    if (timer !== undefined) clearTimer(timer);
    timer = undefined;
  };
  return Object.freeze({
    /** A reconciliation was requested; returns its ticket. */
    request: (): number => {
      cancel();
      requests += 1;
      return requests;
    },
    /** The report for the reconciliation that just failed with `cause`. */
    failed: (
      cause: unknown,
      pendingHeaders: readonly string[],
    ): WatcherAvailabilityStatus => {
      lastFailed = true;
      consecutive += 1;
      transientOnly &&= isWatcherL1TransientFailure(cause);
      return {
        phase:
          transientOnly &&
          consecutive < WATCHER_AVAILABILITY_TRANSIENT_FAILURES_BEFORE_BLOCKED
            ? "waiting"
            : "blocked",
        pendingHeaders,
        detail: cause instanceof Error ? cause.message : String(cause),
      };
    },
    /**
     * Ends the current reconciliation of `ticket`: a completed one resets the
     * count, a failed one arms one retry unless a newer request exists.
     */
    settle: (ticket: number, retry: () => Promise<void>): void => {
      const failed = lastFailed;
      lastFailed = false;
      if (!failed) {
        consecutive = 0;
        transientOnly = true;
        return;
      }
      if (ticket !== requests) return;
      cancel();
      timer = setTimer(() => {
        timer = undefined;
        // The retry reports its own outcome; a rejection here has no caller.
        if (ticket === requests) retry().catch(() => undefined);
      }, delayMs(consecutive));
    },
    /** Revokes any armed retry (rollback, shutdown, close). */
    cancel: (): void => {
      cancel();
      requests += 1;
    },
  });
};
