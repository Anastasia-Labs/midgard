import type { FailureClass } from "./failure.js";
import type { FollowStatus, FollowWaitCause } from "./status.js";

/** What the follow loop hands its failure recorder. */
export type StuckRecorderInput = {
  readonly stuckAfter: number;
  /** How long transient `store` and `apply` failures may go on, with no
   * event settled between them, before the loop stops `exhausted`. */
  readonly transientBudgetMs: number;
  /** Wall-clock ms. */
  readonly now: () => number;
  readonly log: (line: string) => void;
  readonly publish: (
    change: Partial<Omit<FollowStatus, "readiness">>,
  ) => Promise<void>;
};

/**
 * The follow loop's failure record (`followChain`). `failed` records a
 * failure and returns whether the loop stops on it (`true`: the caller
 * returns).
 *
 * A transient failure is waited on, within a bound for the store's: a
 * `store` or `apply` failure (the store did not answer) starts a clock that
 * only a settled event stops; one that comes `transientBudgetMs` or more
 * after that start stops the loop (`state` `exhausted`), and its host exits
 * non-zero so its supervisor's restart is the backoff. A `stream` failure
 * (waiting on the L1 node) and a held writer lease (`store_locked`, another
 * process's progress) have no bound.
 *
 * A failure that is not transient counts toward `stuck` at `at` (the event
 * point) or, without it, at the failing step (`cause`): a deterministic
 * failure at once, an unknown one at `stuckAfter` in a row. Stuck, the loop
 * stops (`state` `intervention`, `stuck` naming the failure) and the process
 * stays up. `settled` clears the count and the clock once an event settles.
 */
export const stuckRecorder = ({
  stuckAfter,
  transientBudgetMs,
  now,
  log,
  publish,
}: StuckRecorderInput) => {
  let failure: { at: string; count: number } | null = null;
  let transientSince: number | undefined;
  const failed = async (
    cause: FollowWaitCause,
    detail: string,
    kind: FailureClass,
    at?: string,
  ): Promise<boolean> => {
    if (kind !== "transient") {
      const where = at ?? cause;
      const count = failure?.at === where ? failure.count + 1 : 1;
      failure = { at: where, count };
      if (kind === "deterministic" || count >= stuckAfter) {
        log(
          `follower stopped: ${where} failed ${count} times (${kind}): ${detail}`,
        );
        await publish({
          state: "intervention",
          waiting: null,
          stuck: { at: where, failures: count, detail },
          lastError: detail,
        });
        return true;
      }
    } else if (cause === "store" || cause === "apply") {
      const at = now();
      transientSince ??= at;
      const failingMs = at - transientSince;
      if (failingMs >= transientBudgetMs) {
        const exhausted = `${detail}; transient ${cause} failures for ${Math.round(failingMs / 1_000).toString()} s with no event settled (budget ${Math.round(transientBudgetMs / 1_000).toString()} s)`;
        log(`follower exhausted its transient budget: ${exhausted}`);
        await publish({
          state: "exhausted",
          waiting: { cause, detail: exhausted },
          lastError: detail,
        });
        return true;
      }
    }
    await publish({
      state: "waiting",
      waiting: { cause, detail },
      lastError: detail,
    });
    return false;
  };
  const settled = (): void => {
    failure = null;
    transientSince = undefined;
  };
  return { failed, settled };
};
