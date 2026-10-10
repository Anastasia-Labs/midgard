import { isWatcherL1TransientFailure } from "./transient-failure.js";

/** Spacing between attempts after an L1 transient: 250 ms doubling to 30 s. */
export const watcherL1TransientRetryDelayMs = (retry: number): number =>
  Math.min(30_000, 250 * 2 ** Math.min(Math.max(retry - 1, 0), 7));

/** Resolves after `ms`, or as soon as `signal` aborts; never rejects. */
const pause = (ms: number, signal: AbortSignal | undefined): Promise<void> =>
  new Promise((resolve) => {
    if (signal?.aborted === true) return resolve();
    const done = () => {
      clearTimeout(timer);
      signal?.removeEventListener("abort", done);
      resolve();
    };
    const timer = setTimeout(done, ms);
    signal?.addEventListener("abort", done, { once: true });
  });

/**
 * L1 transients outlasted a `budgetMs` given to `retryWatcherL1Transient`:
 * the follower or its node transport did not answer for that long. `cause`
 * is the last attempt's error.
 */
export class WatcherL1UnavailableError extends Error {
  readonly reason = "watcher_l1_unavailable";
  constructor(
    readonly budgetMs: number,
    readonly attempts: number,
    override readonly cause: Error,
  ) {
    super(
      `watcher_l1_unavailable: the L1 follower or its node transport did not answer for ${Math.round(budgetMs / 1000).toString()} s (${attempts.toString()} attempts): ${cause.message}`,
    );
    this.name = "WatcherL1UnavailableError";
  }
}

/**
 * Runs `attempt` until it settles with anything but an L1 transient (see
 * `isWatcherL1TransientFailure`): the follower or its node transport did not
 * answer, which says nothing about the chain, so the same read is made again
 * after a capped backoff, each attempt costing at most one read every 30 s.
 * Without `budgetMs` there is no limit: an outage of a running watcher's L1
 * source lasts as long as it lasts. With it, a transient that persists for
 * `budgetMs` after the first throws `WatcherL1UnavailableError`. Any other
 * error, and a transient once `signal` has aborted (before or during the
 * wait), is rethrown unchanged. The attempt itself must be safe to repeat.
 */
export const retryWatcherL1Transient = async <T>(
  attempt: () => Promise<T>,
  options: Readonly<{
    signal?: AbortSignal;
    /** Called before each wait; `retry` counts from 1. */
    onRetry?: (error: Error, retry: number, delayMs: number) => void;
    delayMs?: (retry: number) => number;
    /** Narrows or replaces what counts as a transient. */
    transient?: (error: unknown) => error is Error;
    /** How long transients are retried, from the first; default unbounded. */
    budgetMs?: number;
    /** The clock `budgetMs` is measured on; default `Date.now`. */
    now?: () => number;
  }> = {},
): Promise<T> => {
  const delayMs = options.delayMs ?? watcherL1TransientRetryDelayMs;
  const transient = options.transient ?? isWatcherL1TransientFailure;
  const now = options.now ?? Date.now;
  let failingSince: number | undefined;
  for (let retry = 1; ; retry += 1) {
    try {
      return await attempt();
    } catch (error) {
      if (!transient(error) || options.signal?.aborted) throw error;
      if (options.budgetMs !== undefined) {
        const at = now();
        failingSince ??= at;
        if (at - failingSince >= options.budgetMs)
          throw new WatcherL1UnavailableError(options.budgetMs, retry, error);
      }
      const ms = delayMs(retry);
      options.onRetry?.(error, retry, ms);
      await pause(ms, options.signal);
      // An abort during the wait ends the retries: no attempt starts after it.
      if (options.signal?.aborted) throw error;
    }
  }
};
