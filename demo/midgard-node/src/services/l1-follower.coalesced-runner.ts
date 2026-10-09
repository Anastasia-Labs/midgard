import {
  type DriverHold,
  EVENTS_INGESTION_FAILED,
  failureHold,
  isRetriedHold,
  isTransientFailureHold,
} from "../l1-events/driver.js";
import { NODE_TRANSIENT_BUDGET_MS } from "./transient-exhaustion.js";

/** Retry delays for a held driver: capped exponential. */
const RETRY_INITIAL_MS = 500;
const RETRY_MAX_MS = 30_000;

export type CoalescedRunnerBound = Readonly<{
  /** How long runs may end on a transient failure in a row (default
   * `NODE_TRANSIENT_BUDGET_MS`). */
  budgetMs?: number;
  /** Wall-clock ms (default `Date.now`). */
  now?: () => number;
  /** Called once, with the transient failures, when the budget ran out. */
  onExhausted: (holds: readonly DriverHold[]) => void;
}>;

/**
 * Runs `run` once per trigger, coalesced: at most one run at a time and one
 * queued. A run that ends with a hold a retry can clear (`isRetriedHold`: a
 * wait, or a transient failure) is retried on a capped backoff until a run
 * clears them or `signal` aborts. A run whose holds are all `notRetried` (a
 * failure that is not transient, a refusal only a chain change lifts) is not
 * retried on a timer: the next trigger (a follower change) runs it again.
 *
 * A wait on another actor (the store, the L1 node, a peer, this node's own
 * commit path) has no deadline. A transient failure
 * (`isTransientFailureHold`: the database did not answer) does: once runs
 * have ended on one for `budgetMs` in a row, with no run between them free
 * of one, the runner stops (no further run) and calls `onExhausted`, and the
 * node exits non-zero (`transient-exhaustion.ts`).
 */
export const coalescedRunner = (
  run: () => Promise<readonly DriverHold[]>,
  signal: AbortSignal,
  bound: CoalescedRunnerBound,
): (() => void) => {
  const budgetMs = bound.budgetMs ?? NODE_TRANSIENT_BUDGET_MS;
  const now = bound.now ?? Date.now;
  let running = false;
  let queued = false;
  let exhausted = false;
  let transientSince: number | undefined;
  let retryMs = RETRY_INITIAL_MS;
  let timer: ReturnType<typeof setTimeout> | undefined;
  /** Whether `holds` ran the budget out; records their transient failures. */
  const spent = (holds: readonly DriverHold[]): boolean => {
    const failing = holds.filter(isTransientFailureHold);
    if (failing.length === 0) {
      transientSince = undefined;
      return false;
    }
    const at = now();
    transientSince ??= at;
    if (at - transientSince < budgetMs) return false;
    exhausted = true;
    bound.onExhausted(failing);
    return true;
  };
  const trigger = (): void => {
    if (signal.aborted || exhausted) return;
    if (timer !== undefined) {
      clearTimeout(timer);
      timer = undefined;
    }
    if (running) {
      queued = true;
      return;
    }
    running = true;
    void run()
      .catch((error: unknown) => [
        failureHold(EVENTS_INGESTION_FAILED, "driver run", error),
      ])
      .then((holds) => {
        running = false;
        if (spent(holds)) return;
        if (queued) {
          queued = false;
          trigger();
          return;
        }
        if (!holds.some(isRetriedHold) || signal.aborted) {
          retryMs = RETRY_INITIAL_MS;
          return;
        }
        timer = setTimeout(trigger, retryMs);
        timer.unref?.();
        retryMs = Math.min(retryMs * 2, RETRY_MAX_MS);
      });
  };
  signal.addEventListener("abort", () => {
    if (timer !== undefined) clearTimeout(timer);
  });
  return trigger;
};
