/**
 * The node's exit on a transient failure that outlived its bound (owner
 * ruling 2026-10-09: retry only what is plausibly transient, bounded).
 *
 * A running node rides out a PostgreSQL that does not answer: the instance
 * lock reconnects (`node-instance-lock.ts`), the follower's store restarts
 * (`followChain`), the driver's coalesced runner retries its transient holds
 * (`l1-follower.coalesced-runner.ts`). Each does so for at most
 * `NODE_TRANSIENT_BUDGET_MS` in a row. Past it the source signals
 * `TransientBudgetExhaustedError` here, the node logs
 * `node_transient_budget_exhausted source=<source> reason=<reason>` and exits
 * non-zero, and its supervisor's restart is the backoff from there, as
 * cardano-db-sync does on a lost database.
 *
 * Waiting on another actor (another process's instance lock, the L1 node's
 * chain-sync stream, a peer) is not a retry and is not bounded here. A
 * failure that is not transient never exits: the node stays up, unready,
 * under its named reason.
 */
import { Cause, Data, Deferred, Exit } from "effect";

/** How long a running node rides out a transient database failure: the
 * startup database budget and the instance lock's reacquire budget. */
export const NODE_TRANSIENT_BUDGET_MS = 15 * 60_000;

/** A transient failure outlived its bound; the node exits non-zero on it. */
export class TransientBudgetExhaustedError extends Data.TaggedError(
  "TransientBudgetExhaustedError",
)<{
  /** What rode the failure out: `instance_lock`, `l1_follower`, `driver`. */
  readonly source: string;
  /** The named reason it held under. */
  readonly reason: string;
  readonly message: string;
}> {}

export type TransientExhaustion = Deferred.Deferred<
  never,
  TransientBudgetExhaustedError
>;

/**
 * Signals that `source` exhausted its bound, from outside the effect runtime
 * (a promise callback). The first signal wins; later ones change nothing.
 */
export const signalTransientExhausted = (
  exhaustion: TransientExhaustion,
  fields: Readonly<{ source: string; reason: string; detail: string }>,
): void => {
  Deferred.unsafeDone(
    exhaustion,
    Exit.fail(
      new TransientBudgetExhaustedError({
        source: fields.source,
        reason: fields.reason,
        message: `${fields.source} rode out transient failures past its bound under ${fields.reason}: ${fields.detail}`,
      }),
    ),
  );
};

/** The `TransientBudgetExhaustedError` on `cause`, or undefined. */
export const findTransientBudgetExhausted = (
  cause: Cause.Cause<unknown>,
): TransientBudgetExhaustedError | undefined =>
  [...Cause.failures(cause), ...Cause.defects(cause)].find(
    (error): error is TransientBudgetExhaustedError =>
      error instanceof TransientBudgetExhaustedError,
  );
